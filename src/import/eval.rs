//! Evaluation of the syntax tree.
//!
//! This module allows converting a syntax tree parsed from a map file through
//! the [`ast`][super::ast] module and produce the map features described by
//! the file.
//!
//! The language relies on a number of built-in definitions that may differ
//! for different maps, such as functions or units. These are supplied through
//! an implementation of the trait [`Builtin`]. Consequently, quite a few
//! types herein are generic over this trait, expressed through the type
//! argument `B`.

use std::{fmt, fs, io, ops};
use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;
use crate::render::Color;
use crate::path::{Distance, Edge, Position, Subpath, Trace};
use super::ast;
use super::ast::ShortString;
use super::path::ImportPath;


//------------ Builtin -------------------------------------------------------

/// A type providing the builtins for evaluation.
pub trait Builtin: Sized {
    /// A type holding information for each scope.
    ///
    /// When a new scope is started, a value of this type is created via
    /// `Default`. The type should only hold information that differs for
    /// this scope and get unchanged information from the parent scope.
    type Scope: Default;

    /// The representation of additional types produced by builtins.
    ///
    /// This type is held by the [`Value::Builtin(_)`] variant.
    type Value: Clone;

    /// Creates a new scope based on the parent scope.
    fn new_scope(
        &self,
        parent: &Scope<Self>,
    ) -> Self::Scope;

    /// Evaluates a distance.
    fn eval_distance(
        &self,
        number: f64, unit: &str, scope: &Scope<Self>,
        pos: ast::Pos, err: &mut EvalErrors,
    ) -> Result<Distance, Failed>;

    /// Evaluates a function.
    fn eval_function<'s>(
        &'s self,
        name: &str,
        args: ArgumentList<'s, Self>,
        scope: &Scope<'s, Self>,
        pos: ast::Pos,
        err: &mut EvalErrors,
    ) -> Result<Value<'s, Self>, Failed>;

    /// Evaluates a procedure.
    fn eval_procedure(
        &self,
        name: &str,
        args: ArgumentList<Self>,
        scope: &Scope<Self>,
        pos: ast::Pos,
        err: &mut EvalErrors,
    ) -> Result<(), Failed>;

    /// Evaluates an assignment to a render parameter.
    fn eval_render_param(
        &self,
        name: &str,
        value: Expression<Self>,
        scope: &mut Scope<Self>,
        pos: ast::Pos, err: &mut EvalErrors
    ) -> Result<(), Failed>;


    fn load(&self, path: &Path) -> Result<(), LoadErrors> {
        let scope = Scope::root(self);
        let mut err = LoadErrors::default();
        self.load_dir(path, scope, &mut err);
        err.check()
    }

    fn load_dir(
        &self,
        path: &Path,
        mut context: Scope<Self>,
        err: &mut LoadErrors,
    ) {
        // Before we do anything else, we run init.map if it is present on the
        // context so that it can make global definitions.
        let init = path.join("init.map");
        if fs::metadata(&init).map(|meta| meta.is_file()).unwrap_or(false) {
            self.load_file(&init, &mut context, err);
        }

        // Now we walk over the directory and load directories and all
        // .map files.
        let dir = match fs::read_dir(path) {
            Ok(dir) => dir,
            Err(e) => {
                err.push(path, e);
                return;
            }
        };
        for entry in dir {
            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => continue, // Silently skip these errors.
            };
            let ftype = match entry.file_type() {
                Ok(ftype) => ftype,
                Err(_) => continue, // And these, too.
            };
            if ftype.is_dir() {
                self.load_dir(&entry.path(), Scope::new(&context), err);
            }
            else if ftype.is_file() {
                let path = entry.path();
                if let Some(name) = path.file_name() {
                    let name = name.to_str().unwrap_or(&"");
                    if name.starts_with(".") || name == "init.map" {
                        continue
                    }
                }
                if path.extension().and_then(|s| s.to_str()) == Some("map") {
                    self.load_file(&path, &mut Scope::new(&context), err);
                }
            }
        }
    }

    fn load_file(
        &self,
        path: &Path,
        scope: &mut Scope<Self>,
        err: &mut LoadErrors,
    ) {
        let data = match fs::read_to_string(path) {
            Ok(data) => data,
            Err(e) => {
                err.push(path, e);
                return
            }
        };
        let stm = match ast::StatementList::parse_str(&data) {
            Ok(stm) => stm,
            Err(e) => {
                err.push(path, e);
                return
            }
        };
        if let Err(e) = stm.eval_all(scope) {
            err.push(path, e);
        }
    }
}


//------------ Scope ---------------------------------------------------------

/// The current scope of evaluation.
///
/// Each block has its own scope that keeps local variables and render
/// parameter assignments. Since it inherits all assignments from the outer
/// block, the scope optionally keeps a reference to the parent scope.
pub struct Scope<'a, B: Builtin> {
    /// A reference to the builtin.
    builtin: &'a B,

    /// The optional parent scope.
    parent: Option<&'a Scope<'a, B>>,

    /// The local variables of this scope.
    variables: HashMap<ShortString, Value<'a, B>>,

    /// The builtin’s data for this scope.
    custom: B::Scope,
}

impl<'a, B: Builtin> Scope<'a, B> {
    pub fn root(builtin: &'a B) -> Self {
        Self {
            builtin,
            parent: None,
            variables: Default::default(),
            custom: Default::default(),
        }
    }

    pub fn new(parent: &'a Scope<'a, B>) -> Self {
        Self {
            builtin: parent.builtin,
            parent: Some(parent),
            variables: Default::default(),
            custom: parent.builtin.new_scope(parent),
        }
    }

    pub fn set_var(&mut self, ident: ShortString, value: Value<'a, B>) {
        self.variables.insert(ident.clone(), value);
    }

    pub fn get_var(&self, ident: &str) -> Option<&Value<'a, B>> {
        match self.variables.get(ident) {
            Some(value) => Some(value),
            None => self.parent.and_then(|parent| parent.get_var(ident))
        }
    }

    pub fn get_var_cloned(&self, ident: &str) -> Option<Value<'a, B>> {
        self.get_var(ident).cloned()
    }

    pub fn builtin(&self) -> &B {
        self.builtin
    }

    pub fn custom(&self) -> &B::Scope {
        &self.custom
    }

    pub fn custom_mut(&mut self) -> &mut B::Scope {
        &mut self.custom
    }

    pub fn parent(&self) -> Option<&Self> {
        self.parent
    }
}


//============ Evaluated expressions =========================================

//------------ Expression ----------------------------------------------------

/// An evaluated expression.
///
/// It consists of a value and information about the source position which is
/// necessary for error reporting.
pub struct Expression<'a, B: Builtin> {
    /// The value of the expression.
    pub value: Value<'a, B>,

    /// The position of the start of the expression in the source.
    pub pos: ast::Pos,
}

impl<'a, B: Builtin> Expression<'a, B> {
    fn new(value: Value<'a, B>, pos: ast::Pos) -> Self {
        Expression { value, pos }
    }

    pub fn eval<T: FromExpression<'a, B>>(
        self, err: &mut EvalErrors
    ) -> Result<T, Failed> {
        T::from_expression(self, err)
    }
}

impl<'a, B: Builtin> Clone for Expression<'a, B> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            pos: self.pos,
        }
    }
}


//------------ FromExpression ------------------------------------------------

/// A type that can be taken from an expression.
pub trait FromExpression<'a, B: Builtin>: Sized {
    fn matches(expr: &Expression<'a, B>) -> bool;

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed>;
}

impl<'a, B, T> FromExpression<'a, B> for (T, ast::Pos)
where
    B: Builtin,
    T: FromExpression<'a, B>
{
    fn matches(expr: &Expression<'a, B>) -> bool {
        T::matches(expr)
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        let pos = expr.pos;
        T::from_expression(expr, err).map(|res| (res, pos))
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for Color {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Color(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Color(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected color");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for Distance {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Distance(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Distance(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected distance");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for Number {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Number(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Number(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected number");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for f64 {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Number(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Number(val) => Ok(val.into()),
            _ => {
                err.add(expr.pos, "expected number");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for u8 {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Number(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, errs:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Number(val) => {
                match val.try_into() {
                    Ok(val) => Ok(val),
                    Err(err) => {
                        errs.add(expr.pos, err);
                        Err(Failed)
                    }
                }
            }
            _ => {
                errs.add(expr.pos, "expected number");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for i16 {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Number(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, errs:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Number(val) => {
                match val.try_into() {
                    Ok(val) => Ok(val),
                    Err(err) => {
                        errs.add(expr.pos, err);
                        Err(Failed)
                    }
                }
            }
            _ => {
                errs.add(expr.pos, "expected number");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for Position {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Position(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Position(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected postiion");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for SymbolSet {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::SymbolSet(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::SymbolSet(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected symbol set");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for String {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Text(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Text(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected text");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for Trace {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Trace(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Trace(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected trace");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for (Distance, Distance) {
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::Vector(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::Vector(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected a vector");
                Err(Failed)
            }
        }
    }
}

impl<'a, B: Builtin> FromExpression<'a, B> for &'a ImportPath{
    fn matches(expr: &Expression<'a, B>) -> bool {
        matches!(expr.value, Value::ImportPath(_))
    }

    fn from_expression(
        expr: Expression<'a, B>, err:  &mut EvalErrors
    ) -> Result<Self, Failed> {
        match expr.value {
            Value::ImportPath(val) => Ok(val),
            _ => {
                err.add(expr.pos, "expected an import path");
                Err(Failed)
            }
        }
    }
}


//------------ Value ---------------------------------------------------------

/// The value of an evaluated expression.
pub enum Value<'a, B: Builtin> {
    Color(Color),
    Distance(Distance),
    List(Vec<Expression<'a, B>>),
    Number(Number),
    Position(Position),
    SymbolSet(SymbolSet),
    Text(String),
    Trace(Trace),
    Vector((Distance, Distance)),
    ImportPath(&'a ImportPath),
    Custom(B::Value),
}

impl<'a, B: Builtin> Clone for Value<'a, B> {
    fn clone(&self) -> Self {
        use self::Value::*;

        match self {
            Color(value) => Color(*value),
            Distance(value) => Distance(value.clone()),
            List(value) => List(value.clone()),
            Number(value) => Number(*value),
            Position(value) => Position(value.clone()),
            SymbolSet(value) => SymbolSet(value.clone()),
            Text(value) => Text(value.clone()),
            Trace(value) => Trace(value.clone()),
            Vector(value) => Vector(value.clone()),
            ImportPath(value) => ImportPath(*value),
            Custom(value) => Custom(value.clone()),
        }
    }
}


//------------ Number --------------------------------------------------------

/// An evaluated number.
///
/// This number can either be an integer or a float. Note that the integer
/// variant is limited to a `i32`. Integers outside its range will be
/// represented by the float variant.
#[derive(Clone, Copy, Debug)]
pub enum Number {
    Int(i32),
    Float(f64),
}

impl From<Number> for f64 {
    fn from(src: Number) -> f64 {
        match src {
            Number::Int(val) => val.into(),
            Number::Float(val) => val
        }
    }
}

impl TryFrom<Number> for u8 {
    type Error = &'static str;

    fn try_from(src: Number) -> Result<Self, Self::Error> {
        match src {
            Number::Int(val) => {
                val.try_into().map_err(|_| "value out of range")
            }
            Number::Float(_) => {
                Err("integer number expected")
            }
        }
    }
}

impl TryFrom<Number> for i16 {
    type Error = &'static str;

    fn try_from(src: Number) -> Result<Self, Self::Error> {
        match src {
            Number::Int(val) => {
                val.try_into().map_err(|_| "value out of range")
            }
            Number::Float(_) => {
                Err("integer number expected")
            }
        }
    }
}


//------------ SymbolSet -----------------------------------------------------

/// An evaluated set of symbols.
#[derive(Clone, Debug, Default)]
pub struct SymbolSet {
    /// The symbols
    ///
    /// This is actually a map for two reasons. For one, we can store the
    /// source position of the symbol. And secondly, we abuse the option as
    /// a marker that the symbol has been taken out without actually taking
    /// it out.
    set: HashMap<ShortString, Option<ast::Pos>>,

    /// The position of the start of the set.
    pos: ast::Pos,
}

impl SymbolSet {
    fn new(set: ast::SymbolSet) -> Self {
        SymbolSet {
            set: HashMap::from_iter(set.symbols.into_iter().map(|item| {
                (item.ident.ident, Some(item.pos))
            })),
            pos: set.pos
        }
    }

    pub fn insert(&mut self, symbol: impl Into<ShortString>) -> bool {
        // XXX This inserts the symbol as used.
        self.set.insert(symbol.into(), None).is_none()
    }

    pub fn contains(&self, symbol: impl AsRef<str>) -> bool {
        self.set.contains_key(symbol.as_ref())
    }

    pub fn take(&mut self, symbol: impl AsRef<str>) -> bool {
        match self.set.get_mut(symbol.as_ref()) {
            Some(item) => {
                *item = None;
                true
            }
            None => false
        }
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn pos(&self) -> ast::Pos {
        self.pos
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn check_exhausted(&self, err: &mut EvalErrors) -> Result<(), Failed> {
        for (key, &value) in &self.set {
            if let Some(pos) = value {
                err.add(pos, format!("unexpected symbol ':{}'", key));
                return Err(Failed)
            }
        }
        Ok(())
    }

    /// Returns the final member of the symbols set.
    ///
    /// If there is more than one item left, adds an error. If there are no
    /// items left, returns `Ok(None)`.
    pub fn take_final(
        self, err: &mut EvalErrors
    ) -> Result<Option<ShortString>, Failed> {
        let mut res = None;
        let mut many = false;

        for (key, value) in self.set.into_iter() {
            if let Some(pos) = value {
                if res.is_some() {
                    err.add(pos, format!("unexpected symbol ':{}'", key));
                    many = true;
                }
                else {
                    res = Some(key)
                }
            }
        }
        if many {
            return Err(Failed);
        }
        Ok(res)
    }

    pub fn into_iter(self) -> impl Iterator<Item = ShortString> {
        self.set.into_iter().map(|item| item.0)
    }
}

impl<'a> PartialEq<&'a str> for SymbolSet {
    fn eq(&self, other: &&'a str) -> bool {
        if self.set.len() != 1 {
            return false;
        }
        self.set.keys().next().map(|s| s.as_str()) == Some(other)
    }
}


//============ Evaluations for AST Types =====================================
//
// In this section, we add an `eval` method to every relevant type from the
// `ast` module.

//------------ StatementList -------------------------------------------------

impl ast::StatementList {
    pub fn eval_all<B: Builtin>(
        self,
        scope: &mut Scope<B>,
    ) -> Result<(), EvalErrors> {
        let mut err = EvalErrors::default();
        self.eval(scope, &mut err);
        err.check()
    }

    pub fn eval<B: Builtin>(
        self,
        scope: &mut Scope<B>,
        err: &mut EvalErrors
    ) {
        for statement in self.statements {
            statement.eval(scope, err)
        }
    }
}

//------------ Statement -----------------------------------------------------

impl ast::Statement {
    pub fn eval<B: Builtin>(
        self,
        scope: &mut Scope<B>,
        err: &mut EvalErrors
    ) {
        match self {
            ast::Statement::Let(stm) => stm.eval(scope, err),
            ast::Statement::NoOp(_) => { },
            ast::Statement::Procedure(stm) => {
                let _ = stm.eval(scope, err);
            }
            ast::Statement::With(stm) => stm.eval(scope, err),
            ast::Statement::Block(stm) => {
                stm.eval(&mut Scope::new(scope), err)
            }
        }
    }
}

//----------- Let ------------------------------------------------------------

impl ast::Let {
    fn eval<B: Builtin>(self, scope: &mut Scope<B>, err: &mut EvalErrors) {
        for assignment in self.assignments.assignments {
            let target = assignment.target.eval();
            let expression = match assignment.expression.eval(scope, err) {
                Ok(expression) => expression,
                Err(_) => continue,
            };
            scope.set_var(target, expression.value);
        }
    }
}

//---------- Procedure -------------------------------------------------------

impl ast::Procedure {
    fn eval<B: Builtin>(
        self, scope: &mut Scope<B>, err: &mut EvalErrors
    ) -> Result<(), Failed> {
        let pos = self.ident.pos;
        let ident = self.ident.eval();
        let args = self.args.eval_args(scope, err)?;
        scope.builtin.eval_procedure(
            ident.as_ref(), args, scope, pos, err
        )
    }
}

//----------- With -----------------------------------------------------------

impl ast::With {
    pub fn eval<B: Builtin>(
        self,
        scope: &Scope<B>,
        err: &mut EvalErrors
    ) {
        // We need our own scope.
        let mut scope = Scope::new(scope);

        // Next we update the render params from self.params.
        self.params.eval(&mut scope, err);

        // Finally we run the block.
        self.block.eval(&mut scope, err);
    }
}

//------------ AssignmentList ------------------------------------------------

impl ast::AssignmentList {
    fn eval<B: Builtin>(self, scope: &mut Scope<B>, err: &mut EvalErrors) {
        for item in self.assignments {
            let pos = item.target.pos;
            let target = item.target.eval();
            let expression = match item.expression.eval(&scope, err) {
                Ok(expression) => expression,
                Err(_) => continue,
            };
            let _ = scope.builtin.eval_render_param(
                target.as_ref(), expression, scope, pos, err
            );
        }
    }
}

//------------ ArgumentList --------------------------------------------------

impl ast::ArgumentList {
    fn eval_args<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<ArgumentList<'s, B>, Failed> {
        let mut good = true;
        let mut res = ArgumentList::new(self.pos);
        for argument in self.arguments {
            match argument.eval(scope, err) {
                Ok(expr) => res.arguments.push(expr),
                Err(_) => good = false,
            }
        }
        if good {
            Ok(res)
        }
        else {
            Err(Failed)
        }
    }
}

//------------ Expression ----------------------------------------------------

impl ast::Expression {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Expression<'s, B>, Failed> {
        if self.fragments.len() == 1 {
            let first = self.fragments.into_iter().next().unwrap();
            Ok(Expression::new(first.1.eval(scope, err)?, self.pos))
        }
        else {
            let mut path = Trace::new();
            let mut fragments = self.fragments.into_iter();
            while let Some((conn, frag)) = fragments.next() {
                let (post, pre) = conn.tension();
                match frag.eval_path_component(scope, err)? {
                    PathComponent::Subpath(subpath) => {
                        path.push_subpath(post, pre, subpath)
                    }
                    PathComponent::Position(pos) => {
                        let (end_conn, end_frag) = match fragments.next() {
                            Some(stuff) => stuff,
                            None => {
                                err.add(
                                    self.pos,
                                    "path ends after sole position"
                                );
                                return Err(Failed)
                            }
                        };
                        if end_conn != ast::Connector::Straight {
                            err.add(
                                self.pos,
                                "smooth connector in position pair"
                            );
                            return Err(Failed)
                        }
                        let end_pos = match end_frag.eval_path_component(
                            scope, err
                        )? {
                            PathComponent::Position(pos) => pos,
                            _ => {
                                err.add(
                                    self.pos,
                                    "lone position in path definition"
                                );
                                return Err(Failed)
                            }
                        };
                        path.push_edge(post, pre, Edge::new(pos, end_pos));
                    }
                    PathComponent::Trace(val) => {
                        path.push_trace(post, pre, val)
                    }
                }
            }
            Ok(Expression::new(Value::Trace(path), self.pos))
        }
    }
}

//------------ Fragment ------------------------------------------------------

impl ast::Fragment {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        match self {
            ast::Fragment::Complex(frag) => frag.eval(scope, err),
            ast::Fragment::List(frag) => frag.eval(scope, err),
            ast::Fragment::Vector(frag) => frag.eval(scope, err),
            ast::Fragment::Atom(frag) => frag.eval(scope, err),
        }
    }

    fn eval_path_component<'s, B: Builtin>(
        self, scope: &'s Scope<B>, err: &mut EvalErrors
    ) -> Result<PathComponent<'s>, Failed> {
        match self {
            ast::Fragment::Complex(frag) => {
                frag.eval_path_component(scope, err)
            }
            _ => {
                err.add(self.pos(), "expected path component");
                Err(Failed)
            }
        }
    }
}

//------------ Complex -------------------------------------------------------

impl ast::Complex {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        match self.section {
            Some(section) => {
                let base = self.external.eval_import_path(scope, err)?;
                section.eval(base, scope, err)
            }
            None => self.external.eval(scope, err),
        }
    }

    fn eval_path_component<'s, B: Builtin>(
        self, scope: &'s Scope<B>, err: &mut EvalErrors
    ) -> Result<PathComponent<'s>, Failed> {
        match self.section {
            Some(section) => {
                let base = self.external.eval_import_path(scope, err);
                section.eval_either(base?, scope, err)
            }
            None => {
                self.external.eval_path_component(scope, err)
            }
        }
    }
}

//------------ External ------------------------------------------------------

impl ast::External {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        let pos = self.ident.pos;
        let ident = self.ident.eval();
        match self.args {
            Some(args) => {
                let args = args.eval_args(scope, err)?;
                scope.builtin.eval_function(
                    ident.as_ref(), args, scope, pos, err
                )
            }
            None => {
                match scope.get_var_cloned(ident.as_ref()) {
                    Some(val) => Ok(val),
                    None => {
                        err.add(
                            self.pos,
                            format!( "undefined variable '{}'", ident)
                        );
                        Err(Failed)
                    }
                }
            }
        }
    }

    fn eval_import_path<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<&'s ImportPath, Failed> {
        let pos = self.pos;
        match self.eval(scope, err)? {
            Value::ImportPath(value) => Ok(value),
            _ => {
                err.add(pos, "expected import path");
                Err(Failed)
            }
        }
    }

    fn eval_path_component<'s, B: Builtin>(
        self, scope: &'s Scope<B>, err: &mut EvalErrors
    ) -> Result<PathComponent<'s>, Failed> {
        if self.args.is_some() {
            err.add(self.pos, "expected path variable");
            return Err(Failed)
        }
        match scope.get_var(self.ident.as_ref()) {
            Some(Value::Trace(path)) => Ok(PathComponent::Trace(path)),
            _ => {
                err.add(self.pos, "expected path variable");
                Err(Failed)
            }
        }
    }
}

//------------ Section -------------------------------------------------------

impl ast::Section {
    fn eval_subpath<B: Builtin>(
        self, base: &ImportPath, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<Subpath, Failed> {
        let start = self.start.eval_pair(&base, scope, err);
        let end = match self.end {
            Some(end) => end.eval_pair(&base, scope, err),
            None => {
                err.add(self.pos, "expected subpath section");
                return Err(Failed)
            }
        };
        let offset = self.offset.into_iter().fold(
            Ok(Distance::default()),
            |res, item| {
                let item = item.eval_subpath_distance(scope, err);
                if let (Ok(mut res), Ok(item)) = (res, item) {
                    res += item;
                    Ok(res)
                }
                else {
                    Err(Failed)
                }
            }
        )?;
        let start = start?;
        let end = end?;
        Ok(Subpath::eval(
            base.path(), start.0, start.1, end.0, end.1, offset
        ))
    }

    fn eval_position<B: Builtin>(
        self, base: &ImportPath, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<Position, Failed> {
        if self.end.is_some() {
            err.add(self.pos, "expected position section");
            return Err(Failed)
        }
        let start = self.start.eval_pair(&base, scope, err);
        let offset = self.offset.into_iter().fold(
            Ok(PositionOffset::default()),
            |res, item| {
                let item = item.eval_position(scope, err);
                if let (Ok(mut res), Ok(item)) = (res, item) {
                    res += item;
                    Ok(res)
                }
                else {
                    Err(Failed)
                }
            }
        )?;
        let start = start?;
        Ok(Position::eval(
            base.path(),
            start.0, start.1,
            offset.sideways, offset.shift, offset.rotation
        ))
    }

    fn eval_either<'s, B: Builtin>(
        self, base: &ImportPath, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<PathComponent<'s>, Failed> {
        if self.end.is_some() {
            self.eval_subpath(base, scope, err).map(PathComponent::Subpath)
        }
        else {
            self.eval_position(base, scope, err).map(PathComponent::Position)
        }
    }

    fn eval<'s, B: Builtin>(
        self, base: &ImportPath, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        if self.end.is_some() {
            self.eval_subpath(base, scope, err).map(|subpath| {
                let mut path = Trace::new();
                path.push_subpath(1., 1., subpath);
                Value::Trace(path)
            })
        }
        else {
            self.eval_position(base, scope, err).map(Value::Position)
        }
    }
}

//------------ Location ------------------------------------------------------

impl ast::Location {
    fn eval_pair<B: Builtin>(
        self, base: &ImportPath, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<(u32, Distance), Failed> {
        let node = match base.get_named(self.node.as_ref()) {
            Some(node) => Ok(node),
            None => {
                err.add(
                    self.node.pos,
                    format!("unresolved path node '{}'", self.node.as_ref())
                );
                 Err(Failed)
            }
        };
        let distance = self.distance.into_iter().fold(
            Ok(Distance::default()),
            |res, item| {
                match (res, item.eval_distance(scope, err)) {
                    (Ok(mut res), Ok(item)) => {
                        res += item;
                        Ok(res)
                    }
                    _ => Err(Failed),
                }
            }
        )?;
        let node = node?;
        Ok((node, distance))
    }
}

//------------ Distance ------------------------------------------------------

impl ast::Distance {
    fn eval_distance<B: Builtin>(
        self, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<Distance, Failed> {
        let value = self.value.eval_distance(scope, err)?;
        match self.op {
            ast::AddSub::Add => Ok(value),
            ast::AddSub::Sub => Ok(-value),
        }
    }
}

//------------ Offset --------------------------------------------------------

impl ast::Offset {
    fn eval_subpath_distance<B: Builtin>(
        self, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<Distance, Failed> {
        match self {
            ast::Offset::Sideways(sideways) => {
                let value = sideways.value.eval_distance(scope, err)?;
                match sideways.direction {
                    ast::Direction::Left => Ok(value),
                    ast::Direction::Right => Ok(-value),
                }
            }
            ast::Offset::Shift(shift) => {
                err.add(shift.pos, "expected sideways offset");
                Err(Failed)
            }
            ast::Offset::Angle(angle) => {
                err.add(angle.pos, "expected sideways offset");
                Err(Failed)
            }
        }
    }

    fn eval_position<B: Builtin>(
        self, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<PositionOffset, Failed> {
        match self {
            ast::Offset::Sideways(sideways) => {
                let value = sideways.value.eval_distance(scope, err)?;
                let value = match sideways.direction {
                    ast::Direction::Left => value,
                    ast::Direction::Right => -value,
                };
                Ok(PositionOffset::sideways(value))
            }
            ast::Offset::Shift(shift) => {
                let (x, y) = shift.value.eval_pair(scope, err)?;
                let value = match shift.op {
                    ast::AddSub::Add => (x, y),
                    ast::AddSub::Sub => (-x, -y),
                };
                Ok(PositionOffset::shift(value))
            }
            ast::Offset::Angle(angle) => {
                Ok(PositionOffset::rotation(angle.value.eval_float()))
            }
        }
    }
}

//------------ Expressions ---------------------------------------------------

impl ast::Expression {
    fn eval_expression<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Expression<'s, B>, Failed> {
        if self.fragments.len() == 1 {
            let first = self.fragments.into_iter().next().unwrap();
            Ok(Expression::new(first.1.eval(scope, err)?, self.pos))
        }
        else {
            let mut path = Trace::new();
            let mut fragments = self.fragments.into_iter();
            while let Some((conn, frag)) = fragments.next() {
                let (post, pre) = conn.tension();
                match frag.eval_path_component(scope, err)? {
                    PathComponent::Subpath(subpath) => {
                        path.push_subpath(post, pre, subpath)
                    }
                    PathComponent::Position(pos) => {
                        let (end_conn, end_frag) = match fragments.next() {
                            Some(stuff) => stuff,
                            None => {
                                err.add(
                                    self.pos,
                                    "path ends after sole position"
                                );
                                return Err(Failed)
                            }
                        };
                        if end_conn != ast::Connector::Straight {
                            err.add(
                                self.pos,
                                "smooth connector in position pair"
                            );
                            return Err(Failed)
                        }
                        let end_pos = match end_frag.eval_path_component(
                            scope, err
                        )? {
                            PathComponent::Position(pos) => pos,
                            _ => {
                                err.add(
                                    self.pos,
                                    "lone position in path definition"
                                );
                                return Err(Failed)
                            }
                        };
                        path.push_edge(post, pre, Edge::new(pos, end_pos));
                    }
                    PathComponent::Trace(val) => {
                        path.push_trace(post, pre, val)
                    }
                }
            }
            Ok(Expression::new(Value::Trace(path), self.pos))
        }
    }
}

//------------ List ----------------------------------------------------------

impl ast::List {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        self.content.into_iter().fold(Ok(Vec::new()), |res, expr| {
            match (res, expr.eval_expression(scope, err)) {
                (Ok(mut res), Ok(expr)) => {
                    res.push(expr);
                    Ok(res)
                }
                _ => Err(Failed)
            }
        }).map(Value::List)
    }
}

//------------ Vector --------------------------------------------------------

impl ast::Vector {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        self.eval_pair(scope, err).map(Value::Vector)
    }

    fn eval_pair<B: Builtin>(
        self, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<(Distance, Distance), Failed> {
        let x = self.x.eval_distance(scope, err);
        let y = self.y.eval_distance(scope, err)?;
        Ok((x?, y))
    }
}

//------------ Atom ----------------------------------------------------------

impl ast::Atom {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        match self {
            ast::Atom::Number(atom) => atom.eval(),
            ast::Atom::SymbolSet(atom) => atom.eval(),
            ast::Atom::Text(atom) => atom.eval(),
            ast::Atom::UnitNumber(atom) => atom.eval(scope, err),
        }
    }
}

//------------ Number --------------------------------------------------------

impl ast::Number {
    fn eval<'s, B: Builtin>(self) -> Result<Value<'s, B>, Failed> {
        if let Ok(value) = i32::from_str(&self.value) {
            Ok(Value::Number(Number::Int(value)))
        }
        else {
            Ok(Value::Number(
                Number::Float(f64::from_str(&self.value).unwrap())
            ))
        }
    }

    fn eval_float(self) -> f64 {
        f64::from_str(&self.value).unwrap()
    }
}

//------------ SymbolSet -----------------------------------------------------

impl ast::SymbolSet {
    fn eval<'s, B: Builtin>(self) -> Result<Value<'s, B>, Failed> {
        Ok(Value::SymbolSet(SymbolSet::new(self)))
    }
}

//------------ Text ----------------------------------------------------------

impl ast::Text {
    fn eval<'s, B: Builtin>(self) -> Result<Value<'s, B>, Failed> {
        let mut res = self.first.content;
        self.others.into_iter().for_each(|val| res.push_str(&val.content));
        Ok(Value::Text(res))
    }
}

//------------ UnitNumber ----------------------------------------------------

impl ast::UnitNumber {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut EvalErrors
    ) -> Result<Value<'s, B>, Failed> {
        self.eval_distance(scope, err).map(Value::Distance)
    }

    fn eval_distance<B: Builtin>(
        self, scope: &Scope<B>, err: &mut EvalErrors
    ) -> Result<Distance, Failed> {
        scope.builtin.eval_distance(
            self.number.eval_float(), self.unit.as_ref(), scope,
            self.pos, err
        )
    }
}

//------------ Identifier ----------------------------------------------------

impl ast::Identifier {
    fn eval(self) -> ShortString {
        self.ident
    }
}


//============ Helper Types ==================================================

//------------ ArgumentList --------------------------------------------------

/// Evaluated arguments of a function.
pub struct ArgumentList<'a, B: Builtin> {
    /// The arguments.
    arguments: Vec<Expression<'a, B>>,

    /// The start of this argument list in its source.
    pos: ast::Pos,
}

impl<'a, B: Builtin> ArgumentList<'a, B> {
    fn new(pos: ast::Pos) -> Self {
        ArgumentList {
            arguments: Vec::new(),
            pos
        }
    }

    pub fn pos(&self) -> ast::Pos {
        self.pos
    }

    pub fn is_empty(&self) -> bool {
        self.arguments.is_empty()
    }

    pub fn into_array<const N: usize>(
        self, err: &mut EvalErrors,
    ) -> Result<[Expression<'a, B>; N], Failed> {
        self.try_into_array().map_err(|this| {
            err.add(this.pos,
                format!("expected {} arguments", N)
            );
            Failed
        })
    }

    pub fn try_into_array<const N: usize>(
        self
    ) -> Result<[Expression<'a, B>; N], Self> {
        TryFrom::try_from(self.arguments).map_err(|arguments| {
            Self { arguments, pos: self.pos }
        })
    }

    pub fn into_var_array<const N: usize>(
        self, err: &mut EvalErrors,
    ) -> Result<([Expression<'a, B>; N], Vec<Expression<'a, B>>), Failed> {
        self.try_into_var_array().map_err(|this| {
            err.add(this.pos,
                concat!("expected at least ", stringify!(N), " arguments")
            );
            Failed
        })
    }

    pub fn try_into_var_array<const N: usize>(
        mut self
    ) -> Result<([Expression<'a, B>; N], Vec<Expression<'a, B>>), Self> {
        if self.arguments.len() < N {
            return Err(self);
        }
        let tail = self.arguments.split_off(N);
        let head = match TryFrom::try_from(self.arguments) {
            Ok(head) => head,
            Err(_) => unreachable!()
        };
        Ok((head, tail))
    }

    pub fn take_first_if_matches<T: FromExpression<'a, B>>(
        &mut self, err: &mut EvalErrors
    ) -> Result<Option<T>, Failed> {
        match self.arguments.first() {
            Some(arg) => {
                if !T::matches(arg) {
                    return Ok(None)
                }
            }
            None => return Ok(None)
        }
        let arg = self.arguments.remove(0);
        arg.eval(err).map(Some)
    }
}

impl<'a, B: Builtin> Clone for ArgumentList<'a, B> {
    fn clone(&self) -> Self {
        ArgumentList {
            arguments: self.arguments.clone(),
            pos: self.pos
        }
    }
}


/*
//------------ FromArguments -------------------------------------------------

pub trait FromArguments<'a, B: Builtin>: Sized {
    fn from_arguments(
        args: ArgumentList<'a, B>, err: &mut EvalErrors
    ) -> Result<Self, Failed>;
}

impl<'a, B, T0> FromArguments<'a, B> for (T0,)
where
    B: Builtin,
    T0: FromExpression<'a, B>,
{
    fn from_arguments(
        args: ArgumentList<'a, B>, err: &mut EvalErrors
    ) -> Result<Self, Failed> {
        if args.arguments.len() != 1 {
            err.add(args.pos, "expected exactly two arguments");
            return Err(Failed)
        }
        let mut args = args.arguments.into_iter();
        Ok((T0::from_expression(args.next().unwrap(), err)?,))
    }
}

impl<'a, B, T0, T1> FromArguments<'a, B> for (T0, T1)
where
    B: Builtin,
    T0: FromExpression<'a, B>,
    T1: FromExpression<'a, B>,
{
    fn from_arguments(
        args: ArgumentList<'a, B>, err: &mut EvalErrors
    ) -> Result<Self, Failed> {
        if args.arguments.len() != 2 {
            err.add(args.pos, "expected exactly two arguments");
            return Err(Failed)
        }
        let mut args = args.arguments.into_iter();
        let r0 = T0::from_expression(args.next().unwrap(), err);
        let r1 = T1::from_expression(args.next().unwrap(), err);
        Ok((r0?, r1?))
    }
}

impl<'a, B, T0, T1, T2> FromArguments<'a, B> for (T0, T1, T2)
where
    B: Builtin,
    T0: FromExpression<'a, B>,
    T1: FromExpression<'a, B>,
    T2: FromExpression<'a, B>,
{
    fn from_arguments(
        args: ArgumentList<'a, B>, err: &mut EvalErrors
    ) -> Result<Self, Failed> {
        if args.arguments.len() != 3 {
            err.add(args.pos, "expected exactly two arguments");
            return Err(Failed)
        }
        let mut args = args.arguments.into_iter();
        let r0 = T0::from_expression(args.next().unwrap(), err);
        let r1 = T1::from_expression(args.next().unwrap(), err);
        let r2 = T2::from_expression(args.next().unwrap(), err);
        Ok((r0?, r1?, r2?))
    }
}

impl<'a, B, T0, T1, T2, T3> FromArguments<'a, B> for (T0, T1, T2, T3)
where
    B: Builtin,
    T0: FromExpression<'a, B>,
    T1: FromExpression<'a, B>,
    T2: FromExpression<'a, B>,
    T3: FromExpression<'a, B>,
{
    fn from_arguments(
        args: ArgumentList<'a, B>, err: &mut EvalErrors
    ) -> Result<Self, Failed> {
        if args.arguments.len() != 4 {
            err.add(args.pos, "expected exactly two arguments");
            return Err(Failed)
        }
        let mut args = args.arguments.into_iter();
        let r0 = T0::from_expression(args.next().unwrap(), err);
        let r1 = T1::from_expression(args.next().unwrap(), err);
        let r2 = T2::from_expression(args.next().unwrap(), err);
        let r3 = T3::from_expression(args.next().unwrap(), err);
        Ok((r0?, r1?, r2?, r3?))
    }
}

impl<'a, B, T0, T1, T2, T3, T4> FromArguments<'a, B> for (T0, T1, T2, T3, T4)
where
    B: Builtin,
    T0: FromExpression<'a, B>,
    T1: FromExpression<'a, B>,
    T2: FromExpression<'a, B>,
    T3: FromExpression<'a, B>,
    T4: FromExpression<'a, B>,
{
    fn from_arguments(
        args: ArgumentList<'a, B>, err: &mut EvalErrors
    ) -> Result<Self, Failed> {
        if args.arguments.len() != 5 {
            err.add(args.pos, "expected exactly two arguments");
            return Err(Failed)
        }
        let mut args = args.arguments.into_iter();
        let r0 = T0::from_expression(args.next().unwrap(), err);
        let r1 = T1::from_expression(args.next().unwrap(), err);
        let r2 = T2::from_expression(args.next().unwrap(), err);
        let r3 = T3::from_expression(args.next().unwrap(), err);
        let r4 = T4::from_expression(args.next().unwrap(), err);
        Ok((r0?, r1?, r2?, r3?, r4?))
    }
}
*/


//------------ PathComponent -------------------------------------------------

enum PathComponent<'s> {
    Subpath(Subpath),
    Position(Position),
    Trace(&'s Trace),
}


//------------ PositionOffset ------------------------------------------------

/// The combined offset of a position.
#[derive(Clone, Debug, Default)]
struct PositionOffset {
    sideways: Distance,
    shift: (Distance, Distance),
    rotation: Option<f64>,
}

impl PositionOffset {
    fn sideways(sideways: Distance) -> Self {
        PositionOffset {
            sideways,
            shift: (Default::default(), Default::default()),
            rotation: None,
        }
    }

    fn shift(shift: (Distance, Distance)) -> Self {
        PositionOffset {
            sideways: Default::default(),
            shift,
            rotation: None,
        }
    }

    fn rotation(rotation: f64) -> Self {
        PositionOffset {
            sideways: Default::default(),
            shift: (Default::default(), Default::default()),
            rotation: Some(rotation),
        }
    }

}

impl ops::AddAssign for PositionOffset {
    fn add_assign(&mut self, other: Self) {
        self.sideways += other.sideways;
        self.shift.0 += other.shift.0;
        self.shift.1 += other.shift.1;
        self.rotation = match (self.rotation, other.rotation) {
            (Some(l), Some(r)) => Some(l + r),
            (Some(l), None) => Some(l),
            (None, Some(r)) => Some(r),
            (None, None) => None
        }
    }
}


//============ Errors ========================================================

//------------ EvalErrors ----------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct EvalErrors {
    errors: Vec<(ast::Pos, String)>,
}

impl EvalErrors {
    pub fn add(&mut self, pos: ast::Pos, error: impl Into<String>) {
        self.errors.push((pos, error.into()))
    }

    pub fn check(self) -> Result<(), Self> {
        if self.errors.is_empty() {
            Ok(())
        }
        else {
            Err(self)
        }
    }

    pub fn iter<'a>(
        &'a self
    ) -> impl Iterator<Item = (ast::Pos, &'a str)> + 'a {
        self.errors.iter().map(|item| (item.0, item.1.as_ref()))
    }
}


//------------ LoadErrors ----------------------------------------------------

#[derive(Default)]
pub struct LoadErrors(Vec<(String, Error)>);

impl LoadErrors {
    fn push(&mut self, path: &Path, err: impl Into<Error>) {
        self.0.push((path.to_string_lossy().into(), err.into()))
    }

    pub fn extend(&mut self, err: Self) {
        self.0.extend(err.0)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn check(self) -> Result<(), Self> {
        if self.0.is_empty() {
            Ok(())
        }
        else {
            Err(self)
        }
    }
}

impl fmt::Display for LoadErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &(ref path, ref err) in &self.0 {
            match *err {
                Error::Parse(ref err) => {
                    writeln!(f, "{}: {}", path, err)?;
                }
                Error::Eval(ref err) => {
                    for (pos, err) in err.iter() {
                        writeln!(f, "{}:{}: {}", path, pos, err)?;
                    }
                }
                Error::Io(ref err) => {
                    writeln!(f, "{}: {}", path, err)?;
                }
            }
        }
        Ok(())
    }
}


//------------ Error ---------------------------------------------------------

enum Error {
    Parse(ast::Error),
    Eval(EvalErrors),
    Io(io::Error)
}

impl From<ast::Error> for Error {
    fn from(err: ast::Error) -> Error {
        Error::Parse(err)
    }
}

impl From<EvalErrors> for Error {
    fn from(err: EvalErrors) -> Error {
        Error::Eval(err)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}


//------------ Failed --------------------------------------------------------

/// A marker type indicating that an operation has failed.
///
/// This type is used as the error type of a result in cases where the actual
/// error has been been added to an error collection.
#[derive(Copy, Clone, Debug)]
pub struct Failed;

impl<T> From<Failed> for Result<T, Failed> {
    fn from(_: Failed) -> Result<T, Failed> {
        Err(Failed)
    }
}

