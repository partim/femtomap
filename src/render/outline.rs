//! Outlines of shapes.

use std::{cmp, iter, slice};
use kurbo::{
    DEFAULT_ACCURACY, BezPath, PathEl, ParamCurve, ParamCurveArclen,
    ParamCurveDeriv, PathSeg, Point, Segments, Vec2
};
use crate::path::SegTime;
use super::canvas::{Sketch, SketchProperty};


//------------ Outline -------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct Outline {
    /// The actual outline.
    outline: BezPath,
}

impl From<BezPath> for Outline {
    fn from(outline: BezPath) -> Self {
        Outline { outline }
    }
}

impl<'a> SketchProperty for &'a Outline {
    fn apply_to_sketch(self, group: &mut Sketch) {
        self.outline.apply_to_sketch(group)
    }
}

impl Outline {
    fn new() -> Self {
        Self::default()
    }

    /// Returns the number of nodes in the path.
    pub fn node_len(&self) -> u32 {
        self.outline.elements().len().try_into().unwrap()
    }

    /// Returns the arc length of the path.
    pub fn arclen(&self) -> f64 {
        self.outline.segments().fold(0., |len, seg| {
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
        for seg in self.outline.segments() {
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
        let mut res = Outline::new();
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

    pub fn iter_positions(
        &self, part_len: f64, offset: Option<f64>
    ) -> PositionIter {
        PositionIter::new(self, part_len, offset)
    }

    pub fn positions(&self) -> Positions {
        Positions::new(self)
    }
}

/// # Internal Helpers
///
impl Outline {
    /// Resolves path time into a location.
    ///
    /// The integer part of the path time denotes the segment as one less the
    /// segment index. The fractional part of the path time denotes the time
    /// on the segment.
    ///
    /// Negative path times are truncated to zero. Outline times beyond the end
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
        self.outline.get_seg(loc.seg as usize).unwrap()
    }

    /// Moves to the beginning of the segment.
    fn move_to_seg(&mut self, seg: PathSeg) {
        self.outline.move_to(match seg {
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
            PathSeg::Line(line) => self.outline.line_to(line.p1),
            PathSeg::Quad(..) => unreachable!(),
            PathSeg::Cubic(cubic) => {
                self.outline.curve_to(cubic.p1, cubic.p2, cubic.p3)
            }
        }
    }
}

impl<'a> IntoIterator for &'a Outline {
    type Item = PathEl;
    type IntoIter = <&'a BezPath as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.outline).into_iter()
    }
}


//------------ OutlineIter ---------------------------------------------------

pub struct OutlineIter<I>(pub I);

impl<I: IntoIterator<Item = PathEl>> SketchProperty for OutlineIter<I> {
    fn apply_to_sketch(self, group: &mut Sketch) {
        group.cairo().new_path();
        self.0.into_iter().for_each(|el| match el {
            PathEl::MoveTo(p) => group.cairo().move_to(p.x, p.y),
            PathEl::LineTo(p) => group.cairo().line_to(p.x, p.y),
            PathEl::QuadTo(p1, p2) => {
                let p0 = Point::from(group.cairo().current_point().expect(
                    "cairo_get_current_point failed"
                ));
                let c = kurbo::QuadBez::new(p0, p1, p2).raise();
                group.cairo().curve_to(
                    c.p1.x, c.p1.y, c.p2.x, c.p2.y, c.p3.x, c.p3.y
                );
            }
            PathEl::CurveTo(u, v, s) => {
                group.cairo().curve_to(u.x, u.y, v.x, v.y, s.x, s.y)
            }
            PathEl::ClosePath => group.cairo().close_path(),
        })
     }
}


//------------ Positions -----------------------------------------------------

pub struct Positions<'a> {
    cur_seg: Option<PathSeg>,
    iter: Segments<iter::Cloned<slice::Iter<'a, PathEl>>>,
}

impl<'a> Positions<'a> {
    fn new(outline: &'a Outline) -> Self {
        Self {
            cur_seg: None,
            iter: kurbo::segments(outline.into_iter()),
        }
    }

    pub fn advance(&mut self, mut distance: f64) -> Option<(Point, f64)> {
        let mut seg = match self.cur_seg {
            Some(seg) => seg,
            None => self.iter.next()?, // Return on empty
        };

        loop {
            let seg_len = seg.arclen(DEFAULT_ACCURACY);
            match distance.partial_cmp(&seg_len).unwrap() {
                cmp::Ordering::Less => {
                    let end = seg.inv_arclen(distance, DEFAULT_ACCURACY);
                    self.cur_seg = Some(seg.subsegment(end..1.));
                    return Some((
                        seg.eval(end),
                        dir(seg, end).atan2()
                    ))
                }
                cmp::Ordering::Equal => {
                    self.cur_seg = None;
                    return Some((
                        seg.end(),
                        dir(seg, 1.).atan2()
                    ))
                }
                cmp::Ordering::Greater => {
                    distance -= seg_len;
                    match self.iter.next() {
                        Some(next_seg) => seg = next_seg,
                        None => {
                            self.cur_seg = None;
                            return None
                        }
                    }
                }
            }
        }
    }
}


//------------ PositionIter --------------------------------------------------

pub struct PositionIter<'a> {
    positions: Positions<'a>,
    offset: Option<f64>,
    part_len: f64,
}

impl<'a> PositionIter<'a> {
    fn new(outline: &'a Outline, part_len: f64, offset: Option<f64>) -> Self {
        Self {
            positions: outline.positions(),
            part_len,
            offset
        }
    }
}

impl<'a> Iterator for PositionIter<'a> {
    type Item = (Point, f64);

    fn next(&mut self) -> Option<Self::Item> {
        self.positions.advance(
            self.offset.take().unwrap_or(self.part_len)
        )
    }
}


//------------ Helper Functions ----------------------------------------------

/*
fn first_point(seg: PathSeg) -> Point {
    match seg {
        PathSeg::Line(line) => line.p0,
        PathSeg::Quad(..) => unreachable!(),
        PathSeg::Cubic(cubic) => cubic.p0
    }
}
*/

fn dir(seg: PathSeg, at: f64) -> Vec2 {
    match seg {
        PathSeg::Line(seg) => seg.deriv().eval(at),
        PathSeg::Quad(seg) => seg.deriv().eval(at),
        PathSeg::Cubic(seg) => seg.deriv().eval(at),
    }.to_vec2()
}

