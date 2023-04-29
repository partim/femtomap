//! Drawing the map.
//!
//! This module provides the means to actually draw the map. The main type
//! is [`Canvas`] which is used to do the actual drawing. 

use kurbo::{
    DEFAULT_ACCURACY, BezPath, PathEl, ParamCurve, ParamCurveArclen, PathSeg,
    Point, Rect,
};
use crate::path::SegTime;
use super::text::{Font, Text, TextMetrics};


pub use super::pattern::Color;


//------------ Canvas --------------------------------------------------------

/// A virtual surface to draw on.
#[derive(Debug)]
pub struct Canvas {
    /// The Cairo context we use for actual rendering.
    cairo: cairo::Context,

    /// The Pango context we use for text rendering.
    pango: pango::Context,
}

// XXX Cairo-specific things. 
impl Canvas {
    /// Creates a new canvas.
    pub fn new(surface: &cairo::Surface) -> Self {
        let cairo = cairo::Context::new(surface).expect("cairo_create failed");
        let pango = pangocairo::create_context(&cairo);

        Canvas { cairo, pango }
    }

    /// Sets the clipping ara.
    pub fn set_clip(&self, rect: Rect) {
        self.cairo.move_to(rect.x0, rect.y0);
        self.cairo.line_to(rect.x0, rect.y1);
        self.cairo.line_to(rect.x1, rect.y1);
        self.cairo.line_to(rect.x1, rect.y0);
        self.cairo.close_path();
        self.cairo.clip();
    }
}

impl Canvas {
    pub fn start(&mut self) -> Group {
        Group::new(self)
    }

    pub fn prepare_text<'a>(
        &self, text: &'a str, font: Font
    ) -> Text<'a> {
        Text::prepare(self.pango(), text, font)
    }

    pub fn text_metrics(
        &self, text: &Text
    ) -> TextMetrics {
        text.text_metrics()
    }

    pub(super) fn pango(&self) -> &pango::Context {
        &self.pango
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

    pub(super) fn cairo(&self) -> &cairo::Context {
        &self.canvas.cairo
    }
}

/// # Change Style Parameters
///
impl<'a> Group<'a> {
    pub fn apply(&mut self, property: impl Property) {
        property.apply_to_group(self);
    }

    /// Changes the width of a stroked line.
    pub fn apply_line_width(&mut self, width: f64) {
        self.cairo().set_line_width(width)
    }
}

/// # Drawing with outlines
///
impl<'a> Group<'a> {
    /// Applies a path.
    pub fn apply_outline(&mut self, path: impl IntoIterator<Item = PathEl>) {
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

/// # Temporary: Build outlines step by step.
///
impl<'a> Group<'a> {
    pub fn new_path(&mut self) {
        self.cairo().new_path()
    }

    pub fn move_to(&mut self, x: f64, y: f64) {
        self.cairo().move_to(x, y)
    }

    pub fn line_to(&mut self, x: f64, y: f64) {
        self.cairo().line_to(x, y)
    }

    pub fn curve_to(
        &mut self, x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64
    ) {
        self.cairo().curve_to(x1, y1, x2, y2, x3, y3)
    }

    pub fn arc(
        &mut self, xc: f64, yc: f64, radius: f64, angle1: f64, angle2: f64
    ) {
        self.cairo().arc(xc, yc, radius, angle1, angle2)
    }

    pub fn close_path(&mut self) {
        self.cairo().close_path()
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
        self.canvas.cairo.new_path();
    }
}


//------------ Property ------------------------------------------------------

pub trait Property {
    fn apply_to_group(self, group: &mut Group);
}

impl<'a> Property for &'a BezPath {
    fn apply_to_group(self, group: &mut Group) {
        group.cairo().new_path();
        self.iter().for_each(|el| match el {
            PathEl::MoveTo(p) => group.cairo().move_to(p.x, p.y),
            PathEl::LineTo(p) => group.cairo().line_to(p.x, p.y),
            PathEl::QuadTo(..) => unreachable!(),
            PathEl::CurveTo(u, v, s) => {
                group.cairo().curve_to(u.x, u.y, v.x, v.y, s.x, s.y)
            }
            PathEl::ClosePath => group.cairo().close_path(),
        })
    }
}

impl Property for Rect {
    fn apply_to_group(self, group: &mut Group) {
        let cairo = group.cairo();
        cairo.new_path();
        cairo.move_to(self.x0, self.y0);
        cairo.line_to(self.x0, self.y1);
        cairo.line_to(self.x1, self.y1);
        cairo.line_to(self.x1, self.y0);
        cairo.close_path();
    }
}


//------------ DashPattern ---------------------------------------------------

/// A pattern for drawing dashed strokes.
#[derive(Clone, Copy, Debug)]
pub struct DashPattern<const N: usize> {
    dashes: [f64; N],
    offset: f64
}

impl DashPattern<0> {
    pub const fn empty() -> Self {
        Self::new([], 0.)
    }
}

impl<const N: usize> DashPattern<N> {
    pub const fn new(dashes: [f64; N], offset: f64) -> Self {
        Self { dashes, offset }
    }
}

impl<const N: usize> Property for DashPattern<N> {
    fn apply_to_group(self, group: &mut Group) {
        group.cairo().set_dash(&self.dashes, self.offset)
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

impl Property for LineCap {
    fn apply_to_group(self, group: &mut Group) {
        group.cairo().set_line_cap(
            match self {
                LineCap::Butt => cairo::LineCap::Butt,
                LineCap::Round => cairo::LineCap::Round,
                LineCap::Square => cairo::LineCap::Square,
            }
        );
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

impl Property for LineJoin {
    fn apply_to_group(self, group: &mut Group) {
        match self {
            LineJoin::Miter(limit) => {
                group.cairo().set_line_join(cairo::LineJoin::Miter);
                group.cairo().set_miter_limit(limit);
            }
            LineJoin::Round => {
                group.cairo().set_line_join(cairo::LineJoin::Round);
            }
            LineJoin::Bevel => {
                group.cairo().set_line_join(cairo::LineJoin::Bevel);
            }
        }
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

impl Property for Operator {
    fn apply_to_group(self, group: &mut Group) {
        group.cairo().set_operator(
            match self {
                Operator::SourceOver => cairo::Operator::Over,
                Operator::DestinationOut => cairo::Operator::DestOut,
            }
        );
    }
}

//------------ Matrix --------------------------------------------------------

/// The transformation matrix for points on the canvas.
#[derive(Clone, Copy, Debug)]
pub struct Matrix {
    cairo: cairo::Matrix
}

impl Matrix {
    pub fn identity() -> Self {
        Matrix { cairo: cairo::Matrix::identity() }
    }

    fn convert(self, op: impl FnOnce(&mut cairo::Matrix)) -> Self {
        let mut res = self.clone();
        op(&mut res.cairo);
        res
    }

    pub fn translate(self, point: Point) -> Self {
        self.convert(|res| res.translate(point.x, point.y))
    }

    pub fn rotate(self, angle: f64) -> Self {
        self.convert(|res| res.rotate(angle))
    }
}

impl Property for Matrix {
    fn apply_to_group(self, group: &mut Group) {
        group.cairo().set_matrix(self.cairo);
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
        canvas.apply_outline(self)
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

