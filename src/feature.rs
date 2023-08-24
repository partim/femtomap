//! Something that is rendered onto a map.
//!
//! Features are the basic definitions of anything that will eventually be
//! rendered onto a map in a way that is independent of the projection, scale,
//! or style of the map.
//!
//! An application defines its own feature type an implements the [`Feature`]
//! trait for it. It can use the [`FeatureSet`] collection to store features
//! or create them on the fly – for instance from a database.
//!
//! A feature has an associated type called a shape. It is produced when
//! the details of the map to be rendered – called the _style_ – are known.
//! The shape can then be rendered onto the map canvas. This two-stage process
//! has been chosen because of Femotmap’s layering model of simply painting
//! over lower layers. This often requires features to be drawn multiple times
//! at different layers. But because shaping can be expensive, we only want to
//! do it once.
//!
//! An application’s feature type is either an enum of all the primitive
//! features in use or a trait object. However, because [`Feature`] is not
//! object safe, you will have to declare your own trait for `dyn Trait`.

use std::slice;
use std::cmp::Ordering;
use rstar::{AABB, Envelope, RTree, RTreeObject};
use crate::world;
use crate::render::Canvas;


//------------ Feature -------------------------------------------------------

/// A type describing something to be rendered on a map.
///
/// Features are a description of something to be rendered in a way that is
/// independent of the eventual map style, scale, or resolution. The process
/// of translating this information for a concrete map is called _shaping_
/// and is done by the [`shape`][Self::shape] method.
pub trait Feature {
    /// A type providing context for shaping the feature.
    type Style;

    /// The result of shaping the feature.
    ///
    /// The lifetime argument can be used to keep a reference to the feature
    /// in the shape if necessary.
    type Shape<'a> where Self: 'a;

    /// Returns the bounding box of the feature when stored.
    fn storage_bounds(&self) -> world::Rect;

    /// Shapes the feature using the given style.
    fn shape(
        &self, style: &Self::Style, canvas: &Canvas
    ) -> Option<Self::Shape<'_>>;
}


//------------ StoredFeature -------------------------------------------------

/// A feature stored in a [`FeatureSet`].
///
/// This is a private type used as the actual type stored in the R-tree.
struct StoredFeature<F> {
    /// The feature itself.
    feature: F,

    /// The layer of the feature.
    layer: i16,

    /// The group of the feature within the layer.
    ///
    /// Smaller groups are rendered first.
    group: i16,

    /// The bounds of the feature.
    ///
    /// The co-ordinates are zoom, longitude, and latitude.
    bounds: AABB<[f64; 3]>,
}

impl<F: Feature> StoredFeature<F> {
    /// Creates a new stored feature from its components.
    fn new(feature: F, scale: (f64, f64), layer: i16, group: i16) -> Self {
        let bounds = feature.storage_bounds();
        StoredFeature {
            feature,
            layer,
            group,
            bounds: AABB::from_corners(
                [scale.0, bounds.sw.lon, bounds.sw.lat],
                [scale.1, bounds.ne.lon, bounds.ne.lat],
            ),
        }
    }
}

impl<F> RTreeObject for StoredFeature<F> {
    type Envelope = AABB<[f64; 3]>;

    fn envelope(&self) -> Self::Envelope {
        self.bounds
    }
}


//------------ FeatureSet ----------------------------------------------------

/// A set of features.
///
/// This type stores a set of features and allows to select those that are
/// within a given bounding box.
///
/// You can create a new feature set via the [builder][FeatureSetBuilder].
/// Once you have one, you can use
/// [`locate_unordered`][Self::locate_unordered] to get an iterator over the
/// features within a bounding box or [`shape`][Self::shape] to shape all
/// features within the box and return the shapes.
pub struct FeatureSet<F> {
    /// The R-tree with all the features.
    features: RTree<StoredFeature<F>>,

    /// The bounding box of the whole feature set.
    bounds: AABB<[f64; 3]>,
}

impl<F> FeatureSet<F> {
    /// Returns a builder to create a new feature set.
    pub fn builder() -> FeatureSetBuilder<F> {
        Default::default()
    }

    /// Returns the shaped features for the given bounds and style.
    ///
    /// The method will shape all features that intersect with the given
    /// bounds. This will include all the features whose bounding box
    /// intersects with `bounds` and whose range of minimal and maximum scale
    /// as provided to [`FeatureSetBuilder::insert`] includes `scale`.
    ///
    /// The method returns a vec – albeit wrapped in a special helper type –
    /// of all the shapes resulting from shaping features intersecting with
    /// the given bounds using the given style. The returned shapes are
    /// sorted accoring to their feature’s layer and group.
    pub fn shape(
        &self, scale: f64, bounds: world::Rect, style: &F::Style,
        canvas: &Canvas,
    ) -> ShapedFeatures<F::Shape<'_>>
    where F: Feature {
        let mut shapes: Vec<_> = self.features.locate_in_envelope_intersecting(
            &Self::envelope(scale, bounds)
        ).filter_map(|item| {
            item.feature.shape(style, canvas).map(|shape| {
                Shaped::new(item.layer, item.group, shape)
            })
        }).collect();
        shapes.sort_unstable_by(|left, right| {
            left.key().partial_cmp(
                &right.key()
            ).unwrap_or(Ordering::Equal)
        });
        ShapedFeatures { shapes }
    }

    /// Returns an iterator over features intersecting with the given bounds.
    ///
    /// The returned iterator is unordered, i.e., the features are not
    /// arranged according to their layer and group.
    pub fn locate_unordered(
        &self, scale: f64, bounds: world::Rect
    ) -> impl Iterator<Item = &F> {
        self.features.locate_in_envelope_intersecting(
            &Self::envelope(scale, bounds)
        ).map(|item| &item.feature)
    }

    /// Returns whether there are any features intersecting the given bounds.
    pub fn is_covered(&self, scale: f64, bounds: world::Rect) -> bool {
        self.bounds.contains_envelope(&Self::envelope(scale, bounds))
    }

    /// Converts the user bounds into our internal bounds.
    fn envelope(scale: f64, bounds: world::Rect) -> AABB<[f64; 3]> {
        AABB::from_corners(
            [scale, bounds.sw.lon, bounds.sw.lat],
            [scale, bounds.ne.lon, bounds.ne.lat]
        )
    }
}


//------------ FeatureSetBuilder ---------------------------------------------

/// Builds a new feature set.
///
/// You can aquire a new, empty builder via [`new`](Self::new), the `Default`
/// implementation, or [`FeatureSet::builder`]. You can then add features
/// using the [`insert`][Self::insert] method and finally convert the builder
/// into an imutable feature set via [`finialize`][Self::finalize].
pub struct FeatureSetBuilder<F> {
    /// The features to be added to the feature set.
    features: Vec<StoredFeature<F>>,

    /// The bounding box of the feature set.
    ///
    /// This will be `None` if `features´ is empty.
    bounds: Option<AABB<[f64; 3]>>,
}

impl<F> Default for FeatureSetBuilder<F> {
    fn default() -> Self {
        Self {
            features: Default::default(),
            bounds: None,
        }
    }
}

impl<F> FeatureSetBuilder<F> {
    /// Creates a new empty feature set builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a feature into the set.
    ///
    /// The feature itself is given by `feature`.
    /// 
    /// The `scale` argument provides the minimum and maximum scale for which
    /// the feature should be visible. These values are simply used as given.
    /// They don’t need to be the actual scale but could also be zoom levels
    /// or some other abstract numerical value that represents how detailed
    /// the map needs to be.
    pub fn insert(
        &mut self, feature: F, scale: (f64, f64), layer: i16, group: i16,
    )
    where F: Feature {
        let feature = StoredFeature::new(feature, scale, layer, group);
        if let Some(bounds) = self.bounds.as_mut() {
            bounds.merge(&feature.bounds)
        }
        else {
            self.bounds = Some(feature.bounds)
        };
        self.features.push(feature);
    }

    /// Converts the builder into the final, imutable feature set.
    pub fn finalize(self) -> FeatureSet<F> {
        FeatureSet {
            features: RTree::bulk_load(self.features),
            bounds: self.bounds.unwrap_or_else(|| {
                AABB::from_point([0., 0., 0.])
            })
        }
    }
}


//------------ Shaped --------------------------------------------------------

/// A single feature that has been shaped for rendering.
pub struct Shaped<S> {
    layer: i16,
    group: i16,
    shape: S,
}

impl<S> Shaped<S> {
    fn new(layer: i16, group: i16, shape: S) -> Self {
        Self { layer, group, shape }
    }

    pub fn key(&self) -> (i16, i16) {
        (self.layer, self.group)
    }

    pub fn layer(&self) -> i16 {
        self.layer
    }

    pub fn shape(&self) -> &S {
        &self.shape
    }
}


//------------ ShapedFeatures ------------------------------------------------

/// An ordered list of shaped features.
///
/// This type is returned by [`FeatureSet::shape`]. You can only iterate over
/// it.
pub struct ShapedFeatures<S> {
    shapes: Vec<Shaped<S>>,
}

impl<S> ShapedFeatures<S> {
    pub fn iter(&self) -> slice::Iter<Shaped<S>> {
        self.shapes.iter()
    }

    pub fn as_slice(&self) -> &[Shaped<S>] {
        self.shapes.as_ref()
    }

    pub fn layer_groups(&self) -> ShapeGroupIter<S> {
        ShapeGroupIter { slice: self.as_slice() }
    }
}

impl<'a, S> IntoIterator for &'a ShapedFeatures<S> {
    type Item = &'a Shaped<S>;
    type IntoIter = slice::Iter<'a, Shaped<S>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}


//------------ ShapeGroupIter -----------------------------------------------

pub struct ShapeGroupIter<'a, S> {
    slice: &'a [Shaped<S>],
}

impl<'a, S> Iterator for ShapeGroupIter<'a, S> {
    type Item = &'a [Shaped<S>];

    fn next(&mut self) -> Option<Self::Item> {
        if self.slice.is_empty() {
            None
        }
        else {
            let mut len = 1;
            let mut iter = self.slice.windows(2);
            while let Some([l, r]) = iter.next() {
                if l.layer() == r.layer() {
                    len += 1
                }
                else {
                    break
                }
            }
            let (head, tail) = self.slice.split_at(len);
            self.slice = tail;
            Some(head)
        }
    }
}

