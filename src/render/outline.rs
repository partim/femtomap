//! Outlines of shapes.

use std::{cmp, slice};
use kurbo::{PathEl, Point};
use crate::path::{SegTime, Segment};
use super::canvas::{Sketch, SketchProperty};


//------------ Outline -------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct Outline {
    /// The actual outline.
    outline: Vec<Segment>,
}

/*
impl From<BezPath> for Outline {
    fn from(outline: BezPath) -> Self {
        Outline { outline }
    }
}
*/

impl FromIterator<Segment> for Outline {
    fn from_iter<T>(iter: T) -> Self
    where T: IntoIterator<Item = Segment> {
        Outline { outline: iter.into_iter().collect() }
    }
}


impl<'a> SketchProperty for &'a Outline {
    fn apply_to_sketch(self, group: &mut Sketch) {
        group.cairo().new_path();
        match self.outline.first() {
            Some(seg) => group.cairo().move_to(seg.p0().x, seg.p0().y),
            None => return
        };
        self.outline.iter().for_each(|seg| {
            if let Some((p1, p2)) = seg.control() {
                group.cairo().curve_to(
                    p1.x, p1.y, p2.x, p2.y,
                    seg.p3().x, seg.p3().y
                );
            }
            else {
                group.cairo().line_to(seg.p3().x, seg.p3().y)
            }
        })
    }
}

impl Outline {
    fn new() -> Self {
        Self::default()
    }

    /// Pushes a segment to the end of the outline.
    ///
    /// This assumes that the segment continues the current path
    fn push_cont_seg(&mut self, seg: Segment) {
        self.outline.push(seg)
    }

    /// Returns the number of nodes in the path.
    pub fn node_len(&self) -> u32 {
        match self.outline.len() {
            0 => 0,
            len => u32::try_from(len + 1).expect("super long outline")
        }
    }

    /// Returns the arc length of the path.
    pub fn arclen(&self) -> f64 {
        self.outline.iter().fold(0., |len, seg| {
            len + seg.arclen()
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
            res.push_cont_seg(
                self.get_seg(start).sub(start.time, end.time)
            );
        }
        else if start <= end {
            res.push_cont_seg(
                self.get_seg(start).sub(start.time, 1.)
            );
            start.seg += 1;
            while start.seg < end.seg {
                res.push_cont_seg(self.get_seg(start))
            }
            res.push_cont_seg(
                self.get_seg(end).sub(0., end.time)
            );
        }
        else {
            res.push_cont_seg(
                self.get_seg(start).sub(0., start.time).rev()
            );
            start.seg -= 1;
            while start.seg > end.seg {
                res.push_cont_seg(self.get_seg(start).rev());
            }
            res.push_cont_seg(
                self.get_seg(end).sub(end.time, 1.).rev()
            );
        }
        res
    }

    pub fn offset(&self, offset: f64) -> Self {
        Self::from_iter(self.outline.iter().map(|seg| seg.offset(offset)))
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
    fn get_seg(&self, loc: SegTime) -> Segment {
        *self.outline.get(loc.seg as usize).unwrap()
    }
}

/*
impl<'a> IntoIterator for &'a Outline {
    type Item = PathEl;
    type IntoIter = <&'a BezPath as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.outline).into_iter()
    }
}
*/


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
    cur_seg: Option<Segment>,
    iter: slice::Iter<'a, Segment>,
}

impl<'a> Positions<'a> {
    fn new(outline: &'a Outline) -> Self {
        Self {
            cur_seg: None,
            iter: outline.outline.iter(),
        }
    }

    pub fn advance(&mut self, mut distance: f64) -> Option<(Point, f64)> {
        let mut seg = match self.cur_seg {
            Some(seg) => seg,
            None => *self.iter.next()?, // Return on empty
        };

        loop {
            let seg_len = seg.arclen();
            let seg_distance = distance * seg.off_factor();
            match seg_distance.partial_cmp(&seg_len).unwrap() {
                cmp::Ordering::Less => {
                    let end = seg.arctime(seg_distance);
                    self.cur_seg = Some(seg.sub(end, 1.));
                    return Some((
                        seg.point(end),
                        seg.dir(end).atan2()
                    ))
                }
                cmp::Ordering::Equal => {
                    self.cur_seg = None;
                    return Some((
                        seg.p3(),
                        seg.dir(1.).atan2()
                    ))
                }
                cmp::Ordering::Greater => {
                    distance -= seg_len / seg.off_factor();
                    match self.iter.next() {
                        Some(next_seg) => seg = *next_seg,
                        None => {
                            self.cur_seg = None;
                            return None
                        }
                    }
                }
            }
        }
    }

    pub fn advance_sub(&mut self, mut distance: f64) -> Option<Outline> {
        let mut seg = match self.cur_seg {
            Some(seg) => seg,
            None => *self.iter.next()?, // Return on empty
        };
        let mut res = Outline::new();

        loop {
            let seg_len = seg.arclen();
            let seg_distance = distance * seg.off_factor();
            match seg_distance.partial_cmp(&seg_len).unwrap() {
                cmp::Ordering::Less => {
                    let end = seg.arctime(distance);
                    self.cur_seg = Some(seg.sub(end, 1.));
                    res.push_cont_seg(seg.sub(0., end));
                    return Some(res);
                }
                cmp::Ordering::Equal => {
                    self.cur_seg = None;
                    res.push_cont_seg(seg);
                    return Some(res);
                }
                cmp::Ordering::Greater => {
                    distance -= seg_len / seg.off_factor();
                    res.push_cont_seg(seg);
                    match self.iter.next() {
                        Some(next_seg) => seg = *next_seg,
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

