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

use std::ops;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::str::FromStr;
use crate::render::Color;
use crate::path::{Distance, Edge, Position, Subpath, Trace};
use super::ast;
use super::ast::ShortString;
use super::path::ImportPath;


//------------ Builtin -------------------------------------------------------

/// A type providing the builtins for evaluation.
pub trait Builtin: Sized {
    /// The type collecting the data produced during evaluation.
    type Target;

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

    /// Evaluates a distance.
    fn eval_distance(
        &self,
        number: f64, unit: &str, scope: &Scope<Self>,
        pos: ast::Pos, err: &mut Error,
    ) -> Result<Distance, Failed>;

    /// Evaluates a function.
    fn eval_function<'s>(
        &self,
        name: &str,
        args: ArgumentList<Self>,
        scope: &Scope<'s, Self>,
        pos: ast::Pos,
        err: &mut Error,
    ) -> Result<Value<'s, Self>, Failed>;

    /// Evaluates a procedure.
    fn eval_procedure(
        &self,
        name: &str,
        args: ArgumentList<Self>,
        target: &mut Self::Target,
        scope: &Scope<Self>,
        pos: ast::Pos,
        err: &mut Error,
    ) -> Result<(), Failed>;

    /// Evaluates an assignment to a render parameter.
    fn eval_render_param(
        &self,
        name: &str,
        value: Expression<Self>,
        scope: &mut Scope<Self>,
        pos: ast::Pos, err: &mut Error
    ) -> Result<(), Failed>;
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
            custom: Default::default(),
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

    pub fn into_color(
        self, err: &mut Error
    ) -> Result<(Color, ast::Pos), Failed> {
        match self.value {
            Value::Color(val) => Ok((val, self.pos)),
            _ => {
                err.add(self.pos, "expected distance");
                Err(Failed)
            }
        }
    }

    pub fn into_distance(
        self, err: &mut Error
    ) -> Result<(Distance, ast::Pos), Failed> {
        match self.value {
            Value::Distance(val) => Ok((val, self.pos)),
            _ => {
                err.add(self.pos, "expected distance");
                Err(Failed)
            }
        }
    }

    pub fn into_number(
        self, err: &mut Error
    ) -> Result<(Number, ast::Pos), Failed> {
        match self.value {
            Value::Number(val) => Ok((val, self.pos)),
            _ => {
                err.add(self.pos, "expected number");
                Err(Failed)
            }
        }
    }

    pub fn into_f64(
        self, err: &mut Error
    ) -> Result<(f64, ast::Pos), Failed> {
        self.into_number(err).map(|(val, pos)| (val.into_f64(), pos))
    }

    pub fn into_u8(
        self, err: &mut Error
    ) -> Result<(u8, ast::Pos), Failed> {
        let (val, pos) = self.into_number(err)?;
        match val.into_u8() {
            Ok(val) => Ok((val, pos)),
            Err(msg) => {
                err.add(pos, msg);
                Err(Failed)
            }
        }
    }

    pub fn into_i16(
        self, err: &mut Error
    ) -> Result<(i16, ast::Pos), Failed> {
        let (val, pos) = self.into_number(err)?;
        match val.into_i16() {
            Ok(val) => Ok((val, pos)),
            Err(msg) => {
                err.add(pos, msg);
                Err(Failed)
            }
        }
    }

    pub fn into_trace(
        self, err: &mut Error
    ) -> Result<(Trace, ast::Pos), Failed> {
        match self.value {
            Value::Trace(val) => Ok((val, self.pos)),
            _ => {
                err.add(self.pos, "expected path");
                Err(Failed)
            }
        }
    }

    pub fn into_position(
        self, err: &mut Error
    ) -> Result<(Position, ast::Pos), Failed> {
        match self.value {
            Value::Position(val) => Ok((val, self.pos)),
            _ => {
                err.add(self.pos, "expected position");
                Err(Failed)
            }
        }
    }

    pub fn into_symbol(
        self, err: &mut Error
    ) -> Result<(ShortString, ast::Pos), Failed> {
        match self.value {
            Value::SymbolSet(set) => {
                if set.len() != 1 {
                    err.add(self.pos, "expected exactly one symbol");
                    Err(Failed)
                }
                else {
                    Ok((set.into_iter().next().unwrap(), self.pos))
                }
            }
            _ => {
                err.add(self.pos, "expected symbol");
                Err(Failed)
            }
        }
    }

    pub fn into_symbol_set(
        self, err: &mut Error
    ) -> Result<SymbolSet, Failed> {
        match self.value {
            Value::SymbolSet(val) => Ok(val),
            _ => {
                err.add(self.pos, "expected symbol set");
                Err(Failed)
            }
        }
    }

    pub fn into_text(
        self, err: &mut Error
    ) -> Result<(String, ast::Pos), Failed> {
        match self.value {
            Value::Text(val) => Ok((val, self.pos)),
            _ => {
                err.add(self.pos, "expected text");
                Err(Failed)
            }
        }
    }

    pub fn into_vector(
        self, err: &mut Error
    ) -> Result<((Distance, Distance), ast::Pos), Failed> {
        match self.value {
            Value::Vector(val) => Ok((val, self.pos)),
            _ => {
                err.add(self.pos, "expected a vector");
                Err(Failed)
            }
        }
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

impl Number {
    pub fn into_u8(self) -> Result<u8, &'static str> {
        match self {
            Number::Int(val) => {
                val.try_into().map_err(|_| "value out of range")
            }
            Number::Float(_) => {
                Err("integer number expected".into())
            }
        }
    }

    pub fn into_i16(self) -> Result<i16, &'static str> {
        match self {
            Number::Int(val) => {
                val.try_into().map_err(|_| "value out of range")
            }
            Number::Float(_) => {
                Err("integer number expected")
            }
        }
    }

    pub fn into_f64(self) -> f64 {
        match self {
            Number::Int(val) => val.into(),
            Number::Float(val) => val
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

    pub fn check_exhausted(&self, err: &mut Error) -> Result<(), Failed> {
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
        self, err: &mut Error
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


//============ Evaluations for AST Types =====================================
//
// In this section, we add an `eval` method to every relevant type from the
// `ast` module.

//------------ StatementList -------------------------------------------------

impl ast::StatementList {
    pub fn eval_all<B: Builtin>(
        self,
        target: &mut B::Target,
        scope: &mut Scope<B>,
    ) -> Result<(), Error> {
        let mut err = Error::default();
        self.eval(target, scope, &mut err);
        err.check()
    }

    pub fn eval<B: Builtin>(
        self,
        target: &mut B::Target,
        scope: &mut Scope<B>,
        err: &mut Error
    ) {
        for statement in self.statements {
            statement.eval(target, scope, err)
        }
    }
}

//------------ Statement -----------------------------------------------------

impl ast::Statement {
    pub fn eval<B: Builtin>(
        self,
        target: &mut B::Target,
        scope: &mut Scope<B>,
        err: &mut Error
    ) {
        match self {
            ast::Statement::Let(stm) => stm.eval(scope, err),
            ast::Statement::NoOp(_) => { },
            ast::Statement::Procedure(stm) => {
                let _ = stm.eval(target, scope, err);
            }
            ast::Statement::With(stm) => stm.eval(target, scope, err),
            ast::Statement::Block(stm) => {
                stm.eval(target, &mut Scope::new(scope), err)
            }
        }
    }
}

//----------- Let ------------------------------------------------------------

impl ast::Let {
    fn eval<B: Builtin>(self, scope: &mut Scope<B>, err: &mut Error) {
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
        self, target: &mut B::Target, scope: &mut Scope<B>, err: &mut Error
    ) -> Result<(), Failed> {
        let pos = self.ident.pos;
        let ident = self.ident.eval();
        let args = self.args.eval_args(scope, err)?;
        scope.builtin.eval_procedure(
            ident.as_ref(), args, target, scope, pos, err
        )
    }
}

//----------- With -----------------------------------------------------------

impl ast::With {
    pub fn eval<B: Builtin>(
        self,
        target: &mut B::Target,
        scope: &Scope<B>,
        err: &mut Error
    ) {
        // We need our own scope.
        let mut scope = Scope::new(scope);

        // Next we update the render params from self.params.
        self.params.eval(&mut scope, err);

        // Finally we run the block.
        self.block.eval(target, &mut scope, err);
    }
}

//------------ AssignmentList ------------------------------------------------

impl ast::AssignmentList {
    fn eval<B: Builtin>(self, scope: &mut Scope<B>, err: &mut Error) {
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
        self, scope: &Scope<'s, B>, err: &mut Error
    ) -> Result<ArgumentList<'s, B>, Failed> {
        let mut good = true;
        let mut res = ArgumentList::new(self.pos);
        for argument in self.arguments {
            match argument {
                ast::Argument::Keyword(assignment) => {
                    match assignment.expression.eval(scope, err) {
                        Ok(expr) => {
                            res.keyword.insert(assignment.target, expr);
                        }
                        Err(_) => good = false,
                    }
                }
                ast::Argument::Pos(expr) => {
                    match expr.eval(scope, err) {
                        Ok(expr) => res.positional.push(expr),
                        Err(_) => good = false,
                    }
                }
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
        self, scope: &Scope<'s, B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
    ) -> Result<Value<'s, B>, Failed> {
        match self {
            ast::Fragment::Complex(frag) => frag.eval(scope, err),
            ast::Fragment::List(frag) => frag.eval(scope, err),
            ast::Fragment::Vector(frag) => frag.eval(scope, err),
            ast::Fragment::Atom(frag) => frag.eval(scope, err),
        }
    }

    fn eval_path_component<'s, B: Builtin>(
        self, scope: &'s Scope<B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
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
        self, scope: &'s Scope<B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
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
        self, scope: &'s Scope<B>, err: &mut Error
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
        self, base: &ImportPath, scope: &Scope<B>, err: &mut Error
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
        self, base: &ImportPath, scope: &Scope<B>, err: &mut Error
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
        self, base: &ImportPath, scope: &Scope<'s, B>, err: &mut Error
    ) -> Result<PathComponent<'s>, Failed> {
        if self.end.is_some() {
            self.eval_subpath(base, scope, err).map(PathComponent::Subpath)
        }
        else {
            self.eval_position(base, scope, err).map(PathComponent::Position)
        }
    }

    fn eval<'s, B: Builtin>(
        self, base: &ImportPath, scope: &Scope<'s, B>, err: &mut Error
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
        self, base: &ImportPath, scope: &Scope<B>, err: &mut Error
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
        self, scope: &Scope<B>, err: &mut Error
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
        self, scope: &Scope<B>, err: &mut Error
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
        self, scope: &Scope<B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
    ) -> Result<Value<'s, B>, Failed> {
        self.eval_pair(scope, err).map(Value::Vector)
    }

    fn eval_pair<B: Builtin>(
        self, scope: &Scope<B>, err: &mut Error
    ) -> Result<(Distance, Distance), Failed> {
        let x = self.x.eval_distance(scope, err);
        let y = self.y.eval_distance(scope, err)?;
        Ok((x?, y))
    }
}

//------------ Atom ----------------------------------------------------------

impl ast::Atom {
    fn eval<'s, B: Builtin>(
        self, scope: &Scope<'s, B>, err: &mut Error
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
        self, scope: &Scope<'s, B>, err: &mut Error
    ) -> Result<Value<'s, B>, Failed> {
        self.eval_distance(scope, err).map(Value::Distance)
    }

    fn eval_distance<B: Builtin>(
        self, scope: &Scope<B>, err: &mut Error
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
    /// The positional arguments.
    positional: Vec<Expression<'a, B>>,

    /// The keyword arguments
    keyword: HashMap<ast::Identifier, Expression<'a, B>>,

    /// The start of this argument list in its source.
    pos: ast::Pos,
}

impl<'a, B: Builtin> ArgumentList<'a, B> {
    fn new(pos: ast::Pos) -> Self {
        ArgumentList {
            positional: Vec::new(),
            keyword: HashMap::new(),
            pos
        }
    }

    pub fn pos(&self) -> ast::Pos {
        self.pos
    }

    pub fn positional(&self) -> &[Expression<B>] {
        &self.positional
    }

    pub fn into_var_positionals(
        self,
        err: &mut Error,
        test: impl FnOnce(&Self, &mut Error) -> Result<bool, Failed>
    ) -> Result<Vec<Expression<'a, B>>, Result<Self, Failed>> {
        if !self.keyword.is_empty() {
            err.add(self.pos, "expected positional arguments only");
            Err(Err(Failed))
        }
        else {
            match test(&self, err) {
                Ok(true) => Ok(self.positional),
                Ok(false) => Err(Ok(self)),
                Err(_) => Err(Err(Failed))
            }
        }
    }

    pub fn into_positionals<const N: usize>(
        self, err: &mut Error
    ) -> Result<[Expression<'a, B>; N], Failed>
    where [Expression<'a, B>; N]: Default {
        if !self.keyword.is_empty() {
            err.add(self.pos, "expected positional arguments only");
            return Err(Failed)
        }
        if self.positional.len() != N {
            err.add(
                self.pos(),
                format!("expected exactly {} positional arguments", N)
            );
            return Err(Failed)
        }

        // XXX This could be more efficient using MaybeUninit but
        //     mem::transmute doesn’t seem to like const generics just yet.
        //     Or I am doing something wrong.

        let mut res: [Expression<B>; N] = Default::default();

        for (pos, src) in self.positional.into_iter().enumerate() {
            res[pos] = src;
        }

        Ok(res)
    }

    /// Returns exactly n positional arguments.
    ///
    /// Fails if there are keyword arguments or more than n positional
    /// arguments. Returns `Ok(None)` if there are less than n positional
    /// arguments.
    pub fn into_n_positionals(
        self, n: usize, err: &mut Error
    ) -> Result<Vec<Expression<'a, B>>, Result<ArgumentList<'a, B>, Failed>> {
        self.into_var_positionals(err, |args, err| {
            match n.cmp(&args.positional().len()) {
                Ordering::Greater => Ok(false),
                Ordering::Equal => Ok(true),
                Ordering::Less => {
                    err.add(
                        args.pos(),
                        format!("expected exactly {} positional arguments", n)
                    );
                    Err(Failed)
                }
            }
        })
    }

    /// Returns the only positional argument.
    pub fn into_sole_positional(
        self, err: &mut Error
    ) -> Result<Expression<'a, B>, Result<ArgumentList<'a, B>, Failed>> {
        self.into_n_positionals(1, err).map(|mut res| res.pop().unwrap())
    }

    /// Returns the only positional argument if there is one.
    pub fn into_opt_sole_position(
        self, err: &mut Error
    ) -> Result<Option<Expression<'a, B>>, Failed> {
        if !self.keyword.is_empty() {
            err.add(self.pos, "expected zero or one positional arguments");
            return Err(Failed)
        }
        let mut args = self.positional.into_iter();
        let res = match args.next() {
            None => return Ok(None),
            Some(res) => res,
        };
        if args.next().is_some() {
            err.add(self.pos, "expected zero or one positional arguments");
            return Err(Failed)
        }
        Ok(Some(res))
    }

    /// Checks that there are keyword arguments only.
    pub fn keyword_only(&self, err: &mut Error) -> Result<(), Failed> {
        if !self.positional.is_empty() {
            err.add(self.pos, "expected keyword arguments only");
            Err(Failed)
        }
        else {
            Ok(())
        }
    }

    /// Returns a keyword argument.
    pub fn get_keyword(&self, key: &str) -> Option<&Expression<'a, B>> {
        self.keyword.get(key)
    }
}

impl<'a, B: Builtin> Clone for ArgumentList<'a, B> {
    fn clone(&self) -> Self {
        ArgumentList {
            positional: self.positional.clone(),
            keyword: self.keyword.clone(),
            pos: self.pos
        }
    }
}

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

//------------ Error ---------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct Error {
    errors: Vec<(ast::Pos, String)>,
}

impl Error {
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

