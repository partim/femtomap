//! Types for referencing the real world.

/// A point on the surface of a spherical body.
///
/// The point is expressed by its longitude and latitude. The scale of these
/// angles, i.e., whether they are expressed in degrees or randians or
/// something else entirely, is left to the user. For instance, if a map is
/// to be rendered as tiles in Spherical Mercator projection, scaling to a
/// range between -1 and 1 is convenient as they then represent the zoom 0
/// tile.
#[derive(Clone, Copy, Debug)]
pub struct Point {
    /// The longitude of the point.
    ///
    /// This is the west-east position of the point or its ‘x value.’
    pub lon: f64,

    /// The latitude of the point.
    ///
    /// This is the south-north position of the point or its ‘y value.’
    pub lat: f64,
}

#[derive(Clone, Copy, Debug)]
pub struct Rect {
    pub sw: Point,
    pub ne: Point,
}

impl From<kurbo::Rect> for Rect {
    fn from(src: kurbo::Rect) -> Rect {
        Rect {
            sw: Point { lon: src.x0, lat: src.y0 },
            ne: Point { lon: src.x1, lat: src.y1 }
        }
    }
}

impl From<Rect> for kurbo::Rect {
    fn from(src: Rect) -> kurbo::Rect {
        kurbo::Rect::new(src.sw.lon, src.sw.lat, src.ne.lon, src.ne.lat)
    }
}

