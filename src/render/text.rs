use std::borrow::Cow;
use kurbo::{Point, Rect};


//------------ Text ----------------------------------------------------------

/// A line of text prepared for rendering onto a canvas.
#[derive(Clone, Debug)]
pub struct Text<'a> {
    text: &'a str,
    font: cairo::ScaledFont,
}

impl<'a> Text<'a> {
    pub(super) fn prepare(text: &'a str, font: Font) -> Self {
        Text { text, font: font.font }
    }

    pub(super) fn text_metrics(&self, cairo: &cairo::Context) -> TextMetrics {
        cairo.set_scaled_font(&self.font);
        TextMetrics::from_cairo(cairo, self.text)
    }

    pub(super) fn fill(&self, cairo: &cairo::Context, at: Point) {
        self.prepare_text(cairo, at);
        cairo.fill().expect("cairo_fill failed");
    }

    pub(super) fn stroke(&self, cairo: &cairo::Context, at: Point) {
        self.prepare_text(cairo, at);
        cairo.stroke().expect("cairo_stroke failed");
    }

    /// Sets the font styles for text rendering.
    fn prepare_text(&self, cairo: &cairo::Context, at: Point) {
        cairo.set_scaled_font(&self.font);
        cairo.new_path();
        cairo.move_to(at.x, at.y);
        cairo.text_path(self.text);
    }
}


//------------ TextMetrics ---------------------------------------------------

/// A line of text prepared for rendering onto a canvas.
pub struct TextMetrics {
    text: cairo::TextExtents,
    font: cairo::FontExtents,
}

impl TextMetrics {
    fn from_cairo(cairo: &cairo::Context, text: &str) -> Self {
        TextMetrics {
            text: cairo.text_extents(text).expect("cairo_text_extents failed"),
            font: cairo.font_extents().expect("cairo_font_extents failed"),
        }
    }

    pub fn advance(&self) -> Point {
        Point::new(self.text.x_advance(), self.text.y_advance())
    }

    pub fn inked(&self) -> Rect {
        Rect::new(
            self.text.x_bearing(),
            self.text.y_bearing(),
            self.text.width() + self.text.x_bearing(),
            self.text.height() + self.text.y_bearing(),
        )
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
}


//------------ Font ----------------------------------------------------------

/// Description of a font.
#[derive(Clone, Debug)]
pub struct Font {
    font: cairo::ScaledFont,
}


//------------ FontBuilder ---------------------------------------------------

/// Building a font description.
#[derive(Clone, Debug, Default)]
pub struct FontBuilder {
    family: Option<FontFamily>,
    size: Option<f64>,
    features: Option<FontFeatures>,
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
        let mut matrix = cairo::Matrix::identity();
        let size = self.size.unwrap_or(12.);
        matrix.scale(size, size);
        let options = cairo::FontOptions::new().expect(
            "cairo_font_options_create failed"
        );
        if let Some(features) = self.features.as_ref() {
            options.set_variations(Some(features.features.as_ref()))
        }
        Font {
            font: cairo::ScaledFont::new(
                &cairo::FontFace::toy_create(
                    self.family.as_ref().map(|family| {
                        family.family.as_ref()
                    }).unwrap_or(""),
                    self.style.unwrap_or_default().into_cairo(),
                    self.weight.unwrap_or_default().into_cairo(),
                ).expect("cairo_toy_font_face_create failed"),
                &matrix,
                &cairo::Matrix::identity(),
                &options,
            ).expect("cairo_scaled_font_create failed"),
        }
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
    pub fn from_static(features: &'static str) -> Self {
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
    UtlraExpanded
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

impl FontStyle {
    fn into_cairo(self) -> cairo::FontSlant {
        match self {
            Self::Normal => cairo::FontSlant::Normal,
            Self::Oblique => cairo::FontSlant::Oblique,
            Self::Italic => cairo::FontSlant::Italic,
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
    AllSmallCaps,
    PetiteCaps,
    AllPetiteCaps,
    Unicase,
    TitleCaps,
}


//------------ FontWeight ----------------------------------------------------

/// The weight of the font face.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FontWeight {
    Thin,
    UltraLight,
    Light,
    SemiLight,
    Book,
    #[default]
    Normal,
    Medium,
    Semibold,
    Bold,
    UltraBold,
    Heavy,
    UltraHeavy,
}

impl FontWeight {
    fn into_cairo(self) -> cairo::FontWeight {
        match self {
            Self::Bold => cairo::FontWeight::Bold,
            _ => cairo::FontWeight::Normal,
        }
    }
}

