//! Paths, traces, and positions.

pub use self::path::{
    Distance, Location, MapDistance, MapDistanceVec, Path, PathBuilder,
    SegTime, Transform,
};
pub use self::trace::{
    Edge, Position, Segment, SegmentIter, Subpath, Trace
};

mod path;
mod trace;

