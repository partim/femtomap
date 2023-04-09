//! Drawing the map.
//!
//! This module provides the means to actually draw the map. The main type
//! is [`Canvas`] which is used to do the actual drawing. 

use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;
use kurbo::{
    DEFAULT_ACCURACY, BezPath, PathEl, ParamCurve, ParamCurveArclen, PathSeg,
    Point,
};
use crate::path::SegTime;
use super::text::{Font, Text, TextMetrics};


//------------ Canvas --------------------------------------------------------

/// A virtual surface to draw on.
#[derive(Debug)]
pub struct Canvas {
    /// The Cairo context we use for actual rendering.
    cairo: cairo::Context,
}

impl Canvas {
    pub fn start(&mut self) -> Group {
        Group::new(self)
    }

    pub fn prepare_text<'a>(
        &self, text: &'a str, font: &'a Font
    ) -> Text<'a> {
        Text::prepare(text, font)
    }

    pub fn text_metrics(
        &self, text: &Text
    ) -> TextMetrics {
        text.text_metrics(&self.cairo)
    }
}


//------------ Group ---------------------------------------------------------

#[derive(Debug)]
pub struct Group<'a> {
    canvas: &'a Canvas,
}

impl<'a> Group<'a> {
    fn new(canvas: &'a Canvas) -> Self {
        canvas.cairo.save().expect("cairo_save failed");
        Self { canvas }
    }

    pub fn start(&mut self) -> Group {
        Group::new(self.canvas)
    }

    fn cairo(&self) -> &cairo::Context {
        &self.canvas.cairo
    }
}

/// # Change Style Parameters
///
impl<'a> Group<'a> {
    /// Changes the style for strokes and fills to use the given color.
    pub fn apply_color(&mut self, color: Color) {
        self.cairo().set_source_rgba(
            color.red, color.green, color.blue, color.alpha
        )
    }

    /// Changes the dash pattern to be used.
    ///
    /// Dashing is off by default.
    pub fn apply_dash<const N: usize>(&mut self, pattern: DashPattern<N>) {
        self.cairo().set_dash(&pattern.dashes, pattern.offset)
    }

    /// Changes the line cap to be used.
    ///
    /// The default cap is [`LineCap::Butt`].
    pub fn apply_line_cap(&mut self, cap: LineCap) {
        self.cairo().set_line_cap(
            match cap {
                LineCap::Butt => cairo::LineCap::Butt,
                LineCap::Round => cairo::LineCap::Round,
                LineCap::Square => cairo::LineCap::Square,
            }
        );
    }

    /// Changes the line join to be used.
    ///
    /// The default join is [`LineJoin::Miter(10.)`].
    pub fn apply_line_join(&mut self, join: LineJoin) {
        match join {
            LineJoin::Miter(limit) => {
                self.cairo().set_line_join(cairo::LineJoin::Miter);
                self.cairo().set_miter_limit(limit);
            }
            LineJoin::Round => {
                self.cairo().set_line_join(cairo::LineJoin::Round);
            }
            LineJoin::Bevel => {
                self.cairo().set_line_join(cairo::LineJoin::Bevel);
            }
        }
    }

    /// Changes the width of a stroked line.
    pub fn apply_line_width(&mut self, width: f64) {
        self.cairo().set_line_width(width)
    }

    /// Changes the compositing operator.
    ///
    /// The default operator is [`Operator::SourceOver`]
    pub fn apply_operator(&mut self, operator: Operator) {
        self.cairo().set_operator(
            match operator {
                Operator::SourceOver => cairo::Operator::Over,
                Operator::DestinationOut => cairo::Operator::DestOut,
            }
        );
    }
}

/// # Drawing Paths
///
impl<'a> Group<'a> {
    /// Applies a path.
    pub fn apply_path(&mut self, path: impl IntoIterator<Item = PathEl>) {
        self.cairo().new_path();
        path.into_iter().for_each(|el| match el {
            PathEl::MoveTo(p) => self.cairo().move_to(p.x, p.y),
            PathEl::LineTo(p) => self.cairo().line_to(p.x, p.y),
            PathEl::QuadTo(..) => unreachable!(),
            PathEl::CurveTo(u, v, s) => {
                self.cairo().curve_to(u.x, u.y, v.x, v.y, s.x, s.y)
            }
            PathEl::ClosePath => self.cairo().close_path(),
        })
    }

    /// Fills the currently applied path.
    ///
    /// Note that this does not clear the currently applied path.
    pub fn fill(&mut self) {
        self.cairo().fill_preserve().expect("cairo_fill_preserve failed");
    }

    /// Strokes the currently applied path.
    ///
    /// Note that this does not clear the currently applied path.
    pub fn stroke(&mut self) {
        self.cairo().stroke_preserve().expect("cairo_stroke_preserve failed");
    }
}

/// # Drawing Text
///
impl<'a> Group<'a> {
    /// Fills the given text at the given position.
    ///
    /// Note that this does clear the currently applied path.
    pub fn fill_text(&mut self, text: &Text, at: Point) {
        text.fill(self.cairo(), at)
    }

    /// Strokes the given text at the given position.
    ///
    /// Note that this does clear the currently applied path.
    pub fn stroke_text(&mut self, text: &Text, at: Point) {
        text.stroke(self.cairo(), at)
    }
}

impl<'a> Drop for Group<'a> {
    fn drop(&mut self) {
        self.canvas.cairo.restore().expect("cairo_restore failed");
    }
}


//------------ Color ---------------------------------------------------------

/// A color.
#[derive(Clone, Copy, Debug, Default)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(try_from = "String"))
]
pub struct Color {
    red: f64,
    green: f64,
    blue: f64,
    alpha: f64
}

impl Color {
    pub const fn rgb(red: f64, green: f64, blue: f64) -> Self {
        Color { red, green, blue, alpha: 1. }
    }

    pub const fn rgba(red: f64, green: f64, blue: f64, alpha: f64) -> Self {
        Color { red, green, blue, alpha }
    }

    pub const fn grey(level: f64) -> Self {
        Color::rgb(level, level, level)
    }

    pub fn hex(mut hex: &str) -> Result<Self, InvalidHexColor> {
        if hex.starts_with('#') {
            hex = &hex[1..];
        }
        let (r, g, b, a) = if hex.len() == 6 {
            (
                u8::from_str_radix(&hex[0..2], 16)?,
                u8::from_str_radix(&hex[2..4], 16)?,
                u8::from_str_radix(&hex[4..6], 16)?,
                0xFF,
            )
        }
        else if hex.len() == 8 {
            (
                u8::from_str_radix(&hex[0..2], 16)?,
                u8::from_str_radix(&hex[2..4], 16)?,
                u8::from_str_radix(&hex[4..6], 16)?,
                u8::from_str_radix(&hex[6..8], 16)?,
            )
        }
        else {
            return Err(InvalidHexColor)
        };
        Ok(Color::rgba(
            r as f64 / 255.,
            g as f64 / 255.,
            b as f64 / 255.,
            a as f64 / 255.,
        ))
    }

    pub fn apply(self, target: &mut Group) {
        target.apply_color(self)
    }

    pub fn with_alpha(self, alpha: f64) -> Self {
        Color { red: self.red, green: self.green, blue: self.blue, alpha }
    }

    pub fn lighten(self, factor: f64) -> Self {
        fn component(x: f64, factor: f64) -> f64 {
            x * factor + 1. - factor
        }

        Color {
            red: component(self.red, factor),
            green: component(self.green, factor),
            blue: component(self.blue, factor),
            alpha: self.alpha
        }
    }
}

impl Color {
    pub const WHITE: Color = Color::rgb(1., 1., 1.);
    pub const BLACK: Color = Color::rgb(0., 0., 0.);
    pub const RED: Color = Color::rgb(1., 0., 0.);
    pub const TRANSPARENT: Color = Color::rgba(0., 0., 0., 0.);
}

impl TryFrom<String> for Color {
    type Error = InvalidHexColor;

    fn try_from(src: String) -> Result<Self, Self::Error> {
        Self::hex(&src)
    }
}

impl FromStr for Color {
    type Err = InvalidHexColor;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        Self::hex(src)
    }
}


//------------ DashPattern ---------------------------------------------------

/// A pattern for drawing dashed strokes.
#[derive(Clone, Copy, Debug)]
pub struct DashPattern<const N: usize> {
    dashes: [f64; N],
    offset: f64
}

impl<const N: usize> DashPattern<N> {
    pub fn new(dashes: [f64; N], offset: f64) -> Self {
        Self { dashes, offset }
    }

    pub fn apply(self, canvas: &mut Group) {
        canvas.apply_dash(self)
    }
}


//------------ LineCap -------------------------------------------------------

/// The style of endpoints of strokes.
#[derive(Clone, Copy, Debug, Default)]
#[non_exhaustive]
pub enum LineCap {
    /// The stroke stops dead at the endpoint.
    #[default]
    Butt,

    /// A circle with stroke width is drawn at the endpoint.
    Round,

    /// A square with stroke width is drawn at the endpoint.
    Square,
}

impl LineCap {
    pub fn apply(self, canvas: &mut Group) {
        canvas.apply_line_cap(self);
    }
}



//------------ LineJoin ------------------------------------------------------

/// The style of points where a stroke changes direction.
///
/// The default value is `LineJoin::Miter(10.)`.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub enum LineJoin {
    /// The direction is changed in a sharp corner.
    ///
    /// The argument is the limit on the ratio of the miter length to the
    /// line width used to draw a miter join. Beyond this, a bevel is used.
    Miter(f64),

    /// A circle with the stroke width is drawn around the change point.
    Round,

    /// A bevel is drawn at the change point.
    Bevel,
}

impl Default for LineJoin {
    fn default() -> Self {
        Self::Miter(10.)
    }
}

impl LineJoin {
    pub fn apply(self, canvas: &mut Group) {
        canvas.apply_line_join(self);
    }
}


//------------ Operator ------------------------------------------------------

/// The compositing operator to be used when drawing new content.
#[derive(Clone, Copy, Debug, Default)]
#[non_exhaustive]
pub enum Operator {
    /// Draws the source over the existing content.
    #[default]
    SourceOver,

    /// Existing content is kept where it doesn't overlap the new content.
    DestinationOut,
}

impl Operator {
    pub fn apply(self, canvas: &mut Group) {
        canvas.apply_operator(self);
    }
}



//------------ Path ----------------------------------------------------------

/// A path bound to a canvas.
///
/// The path provides a number of convenient method for manipulation. It can
/// also be applied to its canvas at any time.
///
/// All the path’s points are in canvas coordinates. All lengths are canvas
/// lengths in _bp_.
#[derive(Clone, Debug)]
pub struct Path {
    path: BezPath,
}

impl Path {
    pub fn new() -> Self {
        Path {
            path: BezPath::new(),
        }
    }

    pub fn move_to(&mut self, p: Point) {
        self.path.move_to(p);
    }

    pub fn line_to(&mut self, p: Point) {
        self.path.line_to(p);
    }

    pub fn curve_to(&mut self, p0: Point, p1: Point, p2: Point) {
        self.path.curve_to(p0, p1, p2)
    }

    pub fn line_append(&mut self, path: &Path) {
        let mut segs = path.path.segments();
        if let Some(seg) = segs.next() {
            self.path.line_to(first_point(seg));
            self.append_seg(seg);
        }
        for seg in segs {
            self.append_seg(seg);
        }
    }

    pub fn curve_append(&mut self, p1: Point, p2: Point, path: &Path) {
        let mut segs = path.path.segments();
        if let Some(seg) = segs.next() {
            self.path.curve_to(p1, p2, first_point(seg));
            self.append_seg(seg);
        }
        for seg in segs {
            self.append_seg(seg);
        }
    }

    pub fn apply(&self, canvas: &mut  Group) {
        canvas.apply_path(self)
    }
}

impl Path {
    /// Returns the number of nodes in the path.
    pub fn node_len(&self) -> u32 {
        self.path.elements().len().try_into().unwrap()
    }

    /// Returns the arc length of the path.
    pub fn arclen(&self) -> f64 {
        self.path.segments().fold(0., |len, seg| {
            len + seg.arclen(DEFAULT_ACCURACY)
        })
    }

    /*
    /// Returns the path time where the arc length reaches the given value.
    ///
    /// If `arclen` is greater than the path’s arc length, returns the time
    /// value of the end of the path.
    pub fn arctime(&self, arclen: f64) -> f64 {
        let mut arclen = arclen * self.canvas.canvas_bp();
        let mut i = 0.; // avoid int-to-float conversion 
        for seg in self.path.segments() {
            let seg_arclen = seg.arclen(DEFAULT_ACCURACY);
            if seg_arclen > arclen {
                let time = seg.inv_arclen(arclen, DEFAULT_ACCURACY);
                return i + time;
            }
            arclen -= seg_arclen;
            i += 1.
        }
        i
    }
    */

    /// Returns the subpath between the two given path times.
    pub fn subpath(&self, start_time: f64, end_time: f64) -> Self {
        let mut start = self.resolve_time(start_time);
        let end = self.resolve_time(end_time);
        let mut res = Path::new();
        if start.seg == end.seg {
            let seg = self.get_seg(start).subsegment(start.time..end.time);
            res.move_to_seg(seg);
            res.append_seg(seg);
        }
        else if start <= end {
            let first = self.get_seg(start).subsegment(start.time..1.);
            res.move_to_seg(first);
            res.append_seg(first);
            start.seg += 1;
            while start.seg < end.seg {
                res.append_seg(self.get_seg(start))
            }
            let last = self.get_seg(end).subsegment(0. .. end.time);
            res.append_seg(last);
        }
        else {
            let first = self.get_seg(start)
                .subsegment(0. .. start.time)
                .reverse();
            res.move_to_seg(first);
            res.append_seg(first);
            start.seg -= 1;
            while start.seg > end.seg {
                res.append_seg(self.get_seg(start).reverse())
            }
            let last = self.get_seg(end)
                .subsegment(end.time .. 1.)
                .reverse();
            res.append_seg(last);
        }
        res
    }
}

/// # Internal Helpers
///
impl Path {
    /// Resolves path time into a location.
    ///
    /// The integer part of the path time denotes the segment as one less the
    /// segment index. The fractional part of the path time denotes the time
    /// on the segment.
    ///
    /// Negative path times are truncated to zero. Path times beyond the end
    /// of the path are truncated to the end of the path.
    fn resolve_time(&self, time: f64) -> SegTime {
        if time < 0. {
            return SegTime::new(0, 0.)
        }

        // Safely convert the integer part to a u32. Avoid current undefined
        // behaviour in float-to-int conversion.
        let seg = if time >= std::u32::MAX as f64 { std::u32::MAX - 1 }        
        else { time as u32 };
        
        let seg = seg + 1;
        let time = time.fract();

        if seg >= self.node_len() {
            SegTime::new(self.node_len() - 1, 1.)
        }
        else {
            SegTime::new(seg, time)
        }
    }
    
    /// Returns the complete path segment with the given index.
    fn get_seg(&self, loc: SegTime) -> PathSeg {
        self.path.get_seg(loc.seg as usize).unwrap()
    }

    /// Moves to the beginning of the segment.
    fn move_to_seg(&mut self, seg: PathSeg) {
        self.path.move_to(match seg {
            PathSeg::Line(line) => line.p0,
            PathSeg::Quad(..) => unreachable!(),
            PathSeg::Cubic(cubic) => cubic.p0
        })
    }

    /// Appends the tail end of the segment.
    ///
    /// This assumes that the last point on the path is already the start
    /// point of the segment.
    fn append_seg(&mut self, seg: PathSeg) {
        match seg {
            PathSeg::Line(line) => self.path.line_to(line.p1),
            PathSeg::Quad(..) => unreachable!(),
            PathSeg::Cubic(cubic) => {
                self.path.curve_to(cubic.p1, cubic.p2, cubic.p3)
            }
        }
    }
}

impl<'a> IntoIterator for &'a Path {
    type Item = PathEl;
    type IntoIter = <&'a BezPath as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.path).into_iter()
    }
}


//------------ Helper Functions ----------------------------------------------

fn first_point(seg: PathSeg) -> Point {
    match seg {
        PathSeg::Line(line) => line.p0,
        PathSeg::Quad(..) => unreachable!(),
        PathSeg::Cubic(cubic) => cubic.p0
    }
}


//============ Error Types ===================================================

//------------ InvalidHexColor -----------------------------------------------

#[derive(Debug)]
pub struct InvalidHexColor;

impl From<ParseIntError> for InvalidHexColor {
    fn from(_: ParseIntError) -> Self {
        InvalidHexColor
    }
}

impl fmt::Display for InvalidHexColor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("invalid color")
    }
}

