use std::borrow::Cow;
use std::marker::PhantomData;
use kurbo::{Point, Rect, Vec2};


//------------ Text ----------------------------------------------------------

/// A line of text prepared for rendering onto a canvas.
#[derive(Clone, Debug)]
pub struct Text<'a> {
    layout: pango::Layout,
    offset: Vec2,
    marker: PhantomData<&'a ()>,
}

impl<'a> Text<'a> {
    pub(super) fn prepare(
        pango: &pango::Context, text: &'a str, font: Font
    ) -> Self {
        let layout = pango::Layout::new(pango);
        layout.set_text(text);
        layout.set_attributes(Some(&font.attrs));
        let offset = Vec2::new(
            0.,
            -f64::from(layout.baseline()) / f64::from(pango::SCALE)
        );

        Self {
            layout,
            offset,
            marker: PhantomData,
        }
    }

    pub(super) fn text_metrics(&self) -> TextMetrics {
        let (ink, logical) = self.layout.extents();
        TextMetrics {
            ink: rect_from_pango(ink) + self.offset,
            logical: rect_from_pango(logical) + self.offset,
        }
    }

    pub(super) fn fill(&self, cairo: &cairo::Context, at: Point) {
        self.start_path(cairo, at);
        pangocairo::show_layout(cairo, &self.layout);
    }

    pub(super) fn stroke(&self, cairo: &cairo::Context, at: Point) {
        self.start_path(cairo, at);
        pangocairo::layout_path(cairo, &self.layout);
        cairo.stroke().expect("cairo_stroke failed");
    }

    fn start_path(&self, cairo: &cairo::Context, at: Point) {
        let at = at + self.offset;
        cairo.new_path();
        cairo.move_to(at.x, at.y);
    }
}


//------------ TextMetrics ---------------------------------------------------

/// A line of text prepared for rendering onto a canvas.
pub struct TextMetrics {
    ink: Rect,
    logical: Rect,
}

impl TextMetrics {
    pub fn inked(&self) -> Rect {
        self.ink
    }

    pub fn logical(&self) -> Rect {
        self.logical
    }

    /*
    pub fn advance(&self) -> Point {
        Point::new(self.text.x_advance(), self.text.y_advance())
    }

    pub fn ascent(&self) -> f64 {
        self.font.ascent()
    }

    pub fn descent(&self) -> f64 {
        self.font.descent()
    }

    pub fn line_height(&self) -> f64 {
        self.font.height()
    }
    */
}

fn rect_from_pango(rect: pango::Rectangle) -> Rect {
    let x = f64::from(rect.x()) / f64::from(pango::SCALE);
    let y = f64::from(rect.y()) / f64::from(pango::SCALE);
    Rect::new(
        x, y,
        f64::from(rect.width()) / f64::from(pango::SCALE) + x,
        f64::from(rect.height()) / f64::from(pango::SCALE) + y,
    )
}


//------------ Font ----------------------------------------------------------

/// Description of a font.
#[derive(Clone, Debug)]
pub struct Font {
    attrs: pango::AttrList,
}


//------------ FontBuilder ---------------------------------------------------

/// Building a font description.
#[derive(Clone, Debug, Default)]
pub struct FontBuilder {
    family: Option<FontFamily>,
    size: Option<f64>,
    features: Option<FontFeatures>,
    line_height: Option<f64>,
    stretch: Option<FontStretch>,
    style: Option<FontStyle>,
    variant: Option<FontVariant>,
    weight: Option<FontWeight>,
}

impl FontBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn family(mut self, family: FontFamily) -> Self {
        self.family = Some(family);
        self
    }

    pub fn size(mut self, size: f64) -> Self {
        self.size = Some(size);
        self
    }

    pub fn features(mut self, features: FontFeatures) -> Self {
        self.features = Some(features);
        self
    }

    pub fn line_height(mut self, line_height: f64) -> Self {
        self.line_height = Some(line_height);
        self
    }

    pub fn stretch(mut self, stretch: FontStretch) -> Self {
        self.stretch = Some(stretch);
        self
    }

    pub fn style(mut self, style: FontStyle) -> Self {
        self.style = Some(style);
        self
    }

    pub fn variant(mut self, variant: FontVariant) -> Self {
        self.variant = Some(variant);
        self
    }

    pub fn weight(mut self, weight: FontWeight) -> Self {
        self.weight = Some(weight);
        self
    }

    /// Takes unset fields from a different value.
    pub fn update(&mut self, other: &Self) {
        if self.family.is_none() {
            self.family = other.family.clone()
        }
        if self.size.is_none() {
            self.size = other.size;
        }
        if self.features.is_none() {
            self.features = other.features.clone();
        }
        if self.line_height.is_none() {
            self.line_height = other.line_height
        }
        if self.stretch.is_none() {
            self.stretch = other.stretch;
        }
        if self.style.is_none() {
            self.style = other.style;
        }
        if self.variant.is_none() {
            self.variant = other.variant;
        }
        if self.weight.is_none() {
            self.weight = other.weight;
        }
    }

    pub fn finalize(self) -> Font {
        let mut descr = pango::FontDescription::new();
        if let Some(family) = self.family.as_ref() {
            descr.set_family(family.family.as_ref());
        }
        if let Some(size) = self.size {
            descr.set_absolute_size(size * f64::from(pango::SCALE));
        }
        if let Some(stretch) = self.stretch {
            descr.set_stretch(stretch.into())
        }
        if let Some(style) = self.style {
            descr.set_style(style.into())
        }
        if let Some(variant) = self.variant {
            descr.set_variant(variant.into())
        }
        if let Some(weight) = self.weight {
            descr.set_weight(weight.into())
        }

        let attrs = pango::AttrList::new();
        attrs.insert(pango::AttrFontDesc::new(&descr));
        if let Some(line_height) = self.line_height {
            attrs.insert(pango::AttrFloat::new_line_height(line_height));
        }
        if let Some(features) = self.features.as_ref() {
            attrs.insert(
                pango::AttrFontFeatures::new(features.features.as_ref())
            );
        }
        Font { attrs }
    }
}


//------------ FontFamily ----------------------------------------------------

/// The font family of a font face.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FontFamily {
    family: Cow<'static, str>,
}

impl FontFamily {
    pub const fn from_static(name: &'static str) -> Self {
        FontFamily {
            family: Cow::Borrowed(name)
        }
    }

    pub fn from_string(name: String) -> Self {
        FontFamily {
            family: Cow::Owned(name)
        }
    }
}


//------------ FontFeatures --------------------------------------------------

/// The font family of a font face.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FontFeatures {
    features: Cow<'static, str>,
}

impl FontFeatures {
    pub const fn from_static(features: &'static str) -> Self {
        Self {
            features: Cow::Borrowed(features)
        }
    }

    pub fn from_string(features: String) -> Self {
        Self {
            features: Cow::Owned(features)
        }
    }
}


//------------ FontStretch ---------------------------------------------------

/// The stretch of the font
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FontStretch {
    UltraCondensed,
    ExtraCondensed,
    Condensed,
    SemiCondensed,
    #[default]
    Normal,
    SemiExpanded,
    Expanded,
    ExtraExpanded,
    UltraExpanded
}

impl From<FontStretch> for pango::Stretch {
    fn from(src: FontStretch) -> Self {
        match src {
            FontStretch::UltraCondensed => pango::Stretch::UltraCondensed,
            FontStretch::ExtraCondensed => pango::Stretch::ExtraCondensed,
            FontStretch::Condensed => pango::Stretch::Condensed,
            FontStretch::SemiCondensed => pango::Stretch::SemiCondensed,
            FontStretch::Normal => pango::Stretch::Normal,
            FontStretch::SemiExpanded => pango::Stretch::SemiExpanded,
            FontStretch::Expanded => pango::Stretch::Expanded,
            FontStretch::ExtraExpanded => pango::Stretch::ExtraExpanded,
            FontStretch::UltraExpanded => pango::Stretch::UltraExpanded,
        }
    }
}


//------------ FontStyle -----------------------------------------------------

/// The glyph style of the font.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FontStyle {
    #[default]
    Normal,
    Oblique,
    Italic,
}

impl From<FontStyle> for pango::Style {
    fn from(src: FontStyle) -> Self {
        match src {
            FontStyle::Normal => pango::Style::Normal,
            FontStyle::Oblique => pango::Style::Oblique,
            FontStyle::Italic => pango::Style::Italic,
        }
    }
}


//------------ FontVariant ---------------------------------------------------

/// The capitalization variant of the font.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FontVariant {
    #[default]
    Normal,
    SmallCaps,
}

impl From<FontVariant> for pango::Variant {
    fn from(src: FontVariant) -> Self {
        match src {
            FontVariant::Normal => pango::Variant::Normal,
            FontVariant::SmallCaps => pango::Variant::SmallCaps,
        }
    }
}


//------------ FontWeight ----------------------------------------------------

/// The weight of the font face.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FontWeight {
    Thin,
    Ultralight,
    Light,
    Semilight,
    Book,
    #[default]
    Normal,
    Medium,
    Semibold,
    Bold,
    Ultrabold,
    Heavy,
    Ultraheavy,
}

impl From<FontWeight> for pango::Weight {
    fn from(src: FontWeight) -> Self {
        match src {
            FontWeight::Thin => pango::Weight::Thin,
            FontWeight::Ultralight => pango::Weight::Ultralight,
            FontWeight::Light => pango::Weight::Light,
            FontWeight::Semilight => pango::Weight::Semilight,
            FontWeight::Book => pango::Weight::Book,
            FontWeight::Normal => pango::Weight::Normal,
            FontWeight::Medium => pango::Weight::Medium,
            FontWeight::Semibold => pango::Weight::Semibold,
            FontWeight::Bold => pango::Weight::Bold,
            FontWeight::Ultrabold => pango::Weight::Ultrabold,
            FontWeight::Heavy => pango::Weight::Heavy,
            FontWeight::Ultraheavy => pango::Weight::Ultraheavy,
        }
    }
}

