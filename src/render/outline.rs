//! Outlines of shapes.

use kurbo::{BezPath, PathEl, Point};
use super::canvas::{Sketch, SketchProperty};


//------------ Outline -------------------------------------------------------

#[derive(Clone, Debug)]
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
 
