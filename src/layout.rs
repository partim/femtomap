//! Complex text arrangements.

use kurbo::{PathEl, Point, Rect, Vec2};
use crate::render::{Canvas, Font, OutlineIter, Sketch, Text};


//------------ Block ---------------------------------------------------------

/// A block is something that contains a layout.
pub struct Block<P> {
    content: Layout<P>,
}

impl<P> Block<P> {
    pub fn new(content: Layout<P>) -> Self {
        Block {
            content
        }
    }

    pub fn update_properties(
        &mut self, base: &P, op: impl Fn(&mut P, &P),
    ) {
        self.content.update_properties(base, &op)
    }

    pub fn shape(&self, style: &P::Style, canvas: &Canvas) -> ShapedLayout<P>
    where P: Properties {
        self.content.shape_content(style, canvas)
    }
}


//------------ Layout --------------------------------------------------------

/// A layout is an arrangement of text.
///
/// The layout can contain final content – a span, an hrule, or a vrule – or
/// a sequence of other layouts arranged horizontally or vertically – an hbox
/// or vbox, respecitvely.
///
/// For a given canvas, a layout can determine its _extent_ which describes
/// how far the layout’s content would stretch away from a central point
/// called the _anchor_ in all four directions. The extent is used to stack
/// layouts: multiple layouts are placed in such a way that their extents
/// touch.
#[derive(Clone, Debug)]
pub struct Layout<P> {
    /// The content of the layout.
    content: Content<P>,
}

#[derive(Clone, Debug)]
enum Content<P> {
    Vbox(Vbox<P>),
    Hbox(Hbox<P>),
    Span(Span<P>),
    Rule(Rule<P>),
}

impl<P> Layout<P> {
    fn new(content: Content<P>) -> Self {
        Layout { content }
    }

    pub fn vbox(
        halign: Align, vbase: Align, properties: P, lines: Vec<Layout<P>>
    ) -> Self {
        Self::new(Content::Vbox(Vbox::new(halign, vbase, properties, lines)))
    }

    pub fn hbox(
        hbase: Align, valign: Align, properties: P, rows: Vec<Layout<P>>
    ) -> Self {
        Self::new(Content::Hbox(Hbox::new(hbase, valign, properties, rows)))
    }

    pub fn span(text: String, properties: P) -> Self {
        Self::new(Content::Span(Span::new(text, properties)))
    }

    pub fn hrule(properties: P) -> Self {
        Self::new(Content::Rule(Rule::new(true, properties)))
    }

    pub fn vrule(properties: P) -> Self {
        Self::new(Content::Rule(Rule::new(false, properties)))
    }

    pub fn properties(&self) -> &P {
        match self.content {
            Content::Vbox(ref inner) => &inner.properties,
            Content::Hbox(ref inner) => &inner.properties,
            Content::Span(ref inner) => &inner.properties,
            Content::Rule(ref inner) => &inner.properties,
        }
    }

    pub fn properties_mut(&mut self) -> &mut P {
        match self.content {
            Content::Vbox(ref mut inner) => &mut inner.properties,
            Content::Hbox(ref mut inner) => &mut inner.properties,
            Content::Span(ref mut inner) => &mut inner.properties,
            Content::Rule(ref mut inner) => &mut inner.properties,
        }
    }

    fn update_properties(
        &mut self, base: &P, op: &impl Fn(&mut P, &P),
    ) {
        match self.content {
            Content::Vbox(ref mut inner) => inner.update_properties(base, op),
            Content::Hbox(ref mut inner) => inner.update_properties(base, op),
            Content::Span(ref mut inner) => inner.update_properties(base, op),
            Content::Rule(ref mut inner) => inner.update_properties(base, op),
        }
    }
}

impl<P: Properties> Layout<P> {
    fn shape_content(
        &self, style: &P::Style, canvas: &Canvas
    ) -> ShapedLayout<P> {
        match self.content {
            Content::Vbox(ref vbox) => vbox.shape(style, canvas),
            Content::Hbox(ref hbox) => hbox.shape(style, canvas),
            Content::Span(ref span) => span.shape(style, canvas),
            Content::Rule(ref rule) => rule.shape(style, canvas),
        }
    }
}


//------------ Vbox ----------------------------------------------------------

/// A box with its content stacked vertically.
#[derive(Clone, Debug)]
struct Vbox<P> {
    /// The horizontal alignment of the content.
    ///
    /// This also determines the horizontal base of the box.
    halign: Align,

    /// The vertical base of the box.
    vbase: Align,

    /// The properties of the box.
    properties: P,

    /// The content.
    lines: Vec<Layout<P>>,
}

impl<P> Vbox<P> {
    fn new(
        halign: Align, vbase: Align, properties: P, lines: Vec<Layout<P>>
    ) -> Self {
        Vbox { halign, vbase, properties, lines }
    }

    fn update_properties(
        &mut self, base: &P, op: &impl Fn(&mut P, &P),
    ) {
        op(&mut self.properties, base);
        self.lines.iter_mut().for_each(|item| {
            item.update_properties(&self.properties, op)
        })
    }

    fn shape(
        &self, style: &P::Style, canvas: &Canvas
    ) -> ShapedLayout<P>
    where P: Properties {
        // We do this in two steps: First we shape all the contained layouts.
        // Then we shift each layout to its final position.
        //
        // Pre-calculate the inner size. Grow horizontally with the layout
        // sizes and just add up the vertical size.
        let mut extents = Rect::default();

        /*
        // Remember the vertical position of the first explicit base if it
        // comes up.
        let mut base = None;
        */

        let mut children: Vec<_> = self.lines.iter().map(|layout| {
            let res = layout.shape_content(style, canvas);

            if res.outer.x0 < extents.x0 {
                extents.x0 = res.outer.x0
            }
            if res.outer.x1 > extents.x1 {
                extents.x1 = res.outer.x1
            }
            extents.y1 += res.outer.y1 - res.outer.y0;
            res
        }).collect();

        let mut inner = Rect::default();

        if !children.is_empty() {
            // Determine the final top. This depends on the vertical base.
            let mut top = match self.vbase {
                Align::Start => 0.,
                Align::Center => -extents.y1 / 2.,
                Align::Base => {
                    /*
                    match base {
                        Some(base) => -base,
                        None => children[0].outer.y0
                    }
                    */
                    children[0].outer.y0
                }
                Align::End => -extents.y1
            };

            // Now shift each child.
            children.iter_mut().for_each(|shape| {
                // Grow a horizontal rule to the width of the whole box.
                if let Shape::Rule { horizontal: true } = shape.shape {
                    shape.inner.x1 = extents.width();
                    if let Some(frame) = shape.frame.as_mut() {
                        frame.x1 += extents.width();
                    }
                    shape.outer.x1 += extents.width();
                }

                let left = match self.halign {
                    Align::Start => -shape.outer.x0,
                    Align::Center => {
                        -shape.outer.width() / 2. - shape.outer.x0
                    }
                    Align::Base => 0.,
                    Align::End => -shape.outer.x1,
                };
                shape.shift(Vec2::new(left, top - shape.outer.y0));

                top += shape.outer.height();
                inner = inner.union(shape.outer + shape.offset);
            });
        }

        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        ShapedLayout {
            offset: Vec2::default(),
            inner, frame, outer,
            shape: Shape::Box { children }, 
            properties: &self.properties,
        }
    }
}


//------------ Hbox ----------------------------------------------------------

/// A sequence of layouts stacked horizontally.
#[derive(Clone, Debug)]
struct Hbox<P> {
    hbase: Align,
    valign: Align,
    properties: P,
    columns: Vec<Layout<P>>,
}

impl<P> Hbox<P> {
    fn new(
        hbase: Align, valign: Align, properties: P, columns: Vec<Layout<P>>
    ) -> Self {
        Hbox { hbase, valign, properties, columns }
    }

    fn update_properties(
        &mut self, base: &P, op: &impl Fn(&mut P, &P),
    ) {
        op(&mut self.properties, base);
        self.columns.iter_mut().for_each(|item| {
            item.update_properties(&self.properties, op)
        })
    }

    fn shape(
        &self, style: &P::Style, canvas: &Canvas
    ) ->ShapedLayout<P>
    where P: Properties {
        // This is the same as for a vertical box except with horizontal and
        // vertical swapped. Well, d’oh.

        // Pre-calculate the inner size. Grow vertically with the layout
        // sizes and just add up the horizontal size.
        let mut extents = Rect::default();

        // Adjust for padding. Left padding is an initial advance.
        let padding = self.properties.padding(style);
        extents.x1 = padding.left;

        /*
        // Remember the horizontal position of the first explicit base if it
        // comes up.
        let mut base = None;
        */

        let mut children: Vec<_> = self.columns.iter().map(|layout| {
            let res = layout.shape_content(style, canvas);

            if res.outer.y0 < extents.y0 {
                extents.y0 = res.outer.y0
            }
            if res.outer.y1 > extents.y1 {
                extents.y1 = res.outer.y1
            }
            extents.x1 += res.outer.x1 - res.outer.x0;
            res
        }).collect();

        // Add the remaining padding.
        extents.x1 += padding.right;
        extents.y0 -= padding.bottom;
        extents.y1 += padding.top;

        let mut inner = Rect::default();

        if !children.is_empty() {
            // Determine the final left. This depends on the horizontal base.
            let mut left = match self.hbase {
                Align::Start => padding.left,
                Align::Center => -(extents.x1 / 2. - padding.left),
                Align::Base => {
                    /*
                    match base {
                        Some(base) => -base,
                        None => children[0].outer.x0
                    }
                    */
                    children[0].outer.x0
                }
                Align::End => -(extents.x1 - padding.left),
            };

            // Now shift each child.
            children.iter_mut().for_each(|shape| {
                // Grow a vertical rule to the height of the whole box.
                if let Shape::Rule { horizontal: false } = shape.shape {
                    shape.inner.y1 = extents.height();
                    if let Some(frame) = shape.frame.as_mut() {
                        frame.y1 += extents.height();
                    }
                    shape.outer.y1 += extents.height();
                }

                let top = match self.valign {
                    Align::Start => -shape.outer.y0 + padding.top,
                    Align::Center => {
                        -shape.outer.height() / 2. - shape.outer.y0
                    }
                    Align::Base => 0.,
                    Align::End => -shape.outer.y1 - padding.bottom,
                };
                shape.shift(
                    Vec2::new(left - shape.outer.x0, top)
                );

                left += shape.outer.width();
                inner = inner.union(shape.outer + shape.offset);
            });

            match self.hbase {
                Align::Start => inner.x1 += padding.right,
                Align::Center => {
                    inner.x0 -= padding.left;
                    inner.x1 += padding.right
                }
                Align::Base =>  { },
                Align::End => inner.x0 -= padding.left,
            }
            match self.valign {
                Align::Start => inner.y1 += padding.bottom,
                Align::Center => {
                    inner.y1 += padding.bottom;
                    inner.y0 -= padding.top;
                }
                Align::End => inner.y0 -= padding.top,
                _ => { }
            }
        }

        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        ShapedLayout {
            offset: Vec2::default(),
            inner, frame, outer,
            shape: Shape::Box { children },
            properties: &self.properties,
        }
    }
}


//------------ Span ----------------------------------------------------------

/// A run of text rendered with the same properties.
#[derive(Clone, Debug)]
pub struct Span<P> {
    text: String,
    properties: P,
}

impl<P> Span<P> {
    fn new(text: String, properties: P) -> Self {
        Self { text, properties }
    }

    fn update_properties(
        &mut self, base: &P, op: &impl Fn(&mut P, &P),
    ) {
        op(&mut self.properties, base);
    }

    fn shape(
        &self, style: &P::Style, canvas: &Canvas
    ) -> ShapedLayout<P>
    where P: Properties {
        let text = canvas.prepare_text(
            &self.text, self.properties.font(style)
        );
        let metrics = canvas.text_metrics(&text);

        let extents = if self.properties.packed(style) {
            metrics.inked()
        }
        else {
            metrics.logical()
            /*
            let advance = metrics.advance();

            if advance.x != 0. {
                // Horizontal text.
                //
                // The span between 0. and advance.x is the
                // width, but consider that the advance may be negative for
                // right-to-left text.
                //
                // For the height, we assume we have a single line and use
                // the ascender plus the line height.
                Rect::new(
                    if advance.x < 0. { advance.x } else { 0. },
                    -metrics.ascent(),
                    if advance.x > 0. { advance.x } else { 0. },
                    -metrics.ascent() + (
                        metrics.line_height()
                        * self.properties.line_height(style)
                    )
                )
            }
            else {
                // XXX Vertical text. I have no idea. Let’s use the advance
                // v. zero.
                Rect::new(
                    if advance.x < 0. { advance.x } else { 0. },
                    if advance.y < 0. { advance.y } else { 0. },
                    if advance.x > 0. { advance.x } else { 0. },
                    if advance.y > 0. { advance.y } else { 0. },
                )
            }
            */
        };

        let inner = self.properties.padding(style).grow_rect(extents);
        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        ShapedLayout {
            offset: Vec2::default(),
            inner, frame, outer,
            shape: Shape::Span { text },
            properties: &self.properties,
        }
    }
}


//------------ Rule ---------------------------------------------------------

/// A horizontal bar.
#[derive(Clone, Debug)]
pub struct Rule<P> {
    horizontal: bool,
    properties: P,
}

impl<P> Rule<P> {
    fn new(horizontal: bool, properties: P) -> Self {
        Self { horizontal, properties }
    }

    fn update_properties(
        &mut self, base: &P, op: &impl Fn(&mut P, &P),
    ) {
        op(&mut self.properties, base);
    }

    fn shape(
        &self, style: &P::Style, _: &Canvas
    ) -> ShapedLayout<P>
    where P: Properties {
        let inner = Rect::default();
        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        ShapedLayout {
            offset: Vec2::default(),
            inner, frame, outer,
            shape: Shape::Rule { horizontal: self.horizontal },
            properties: &self.properties,
        }
    }
}


//------------ Properties ----------------------------------------------------

/// A type that can provide properties for a layout.
///
/// Most of the properties are derived from the properties of the parent.
/// Thus the method `derive_properties` receives the parent properties plus
/// a style value of the implementers own chosing.
pub trait Properties: Sized {
    /// The style that carries more information for deriving the properties.
    type Style;

    /// The rendering stage.
    ///
    /// This type is used by `render` to identify which phase of rendering
    /// of the layout is currently performed.
    type Stage;

    /// Returns whether a span is packed.
    ///
    /// For a packed span, only the inked area is considered when
    /// calculating text extents. Otherwise, they are based on font metrics.
    ///
    /// The default implementation returns false.
    fn packed(&self, style: &Self::Style) -> bool {
        let _ = style;
        false
    }

    /// Returns the font to be used for a span with these properties.
    fn font (&self, style: &Self::Style) -> Font;

    /// Returns the line height as a factor of the font’s natural line height.
    ///
    /// The default implementation returns a factor of 1.
    fn line_height(&self, style: &Self::Style) -> f64 {
        let _ = style;
        1.
    }

    /// Returns the padding around the content.
    ///
    /// The natural inner extent will be grown by these margins.
    ///
    /// The default implementation returns no margins.
    fn padding(&self, style: &Self::Style) -> Margins {
        let _ = style;
        Margins::default()
    }

    /// Returns the layout’s frame widths.
    fn frame(&self, style: &Self::Style) -> Option<Margins> {
        let _ = style;
        None
    }

    /// Returns the layout’s margins.
    fn margins(&self, style: &Self::Style) -> Margins {
        let _ = style;
        Margins::default()
    }

    /// Renders a shaped layout.
    fn render(
        &self,
        layout: &ShapedLayout<Self>,
        style: &Self::Style,
        base: Vec2,
        stage: &Self::Stage,
        canvas: &mut Sketch,
    );
}



/// Grows the inner extents according to frame and margins.
///
/// Returns the frame and margin extents.
fn frame_and_outer_extents<P: Properties>(
    props: &P, style: &P::Style, inner: Rect
) -> (Option<Rect>, Rect) {
    match props.frame(style) {
        Some(frame) => {
            let frame = frame.grow_rect(inner);
            (Some(frame), props.margins(style).grow_rect(frame))
        }
        None => (None, props.margins(style).grow_rect(inner))
    }
}


//------------ Align ---------------------------------------------------------

/// How layouts are stacked in a box.
#[derive(Clone, Copy, Debug)]
pub enum Align {
    /// The upper or left extent is aligned.
    Start,

    /// The center of each layout is aligned.
    Center,

    /// The bases of each layout are aligned.
    ///
    /// If the content doesn’t have a base, this will resolve to the base of
    /// first item.
    Base,

    /// The lower or right extens is aligned.
    End
}


//------------ Margins -------------------------------------------------------

/// How layouts are stacked in a box.
#[derive(Clone, Copy, Debug, Default)]
pub struct Margins {
    pub top: f64,
    pub right: f64,
    pub bottom: f64,
    pub left: f64,
}

impl Margins {
    pub fn new(top: f64, right: f64, bottom: f64, left: f64) -> Self {
        Self { top, right, bottom, left }
    }

    pub fn equal(value: f64) -> Self {
        Self::new(value, value, value, value)
    }

    pub fn vh(vertical: f64, horizontal: f64) -> Self {
        Self::new(vertical, horizontal, vertical, horizontal)
    }

    pub fn grow_rect(self, rect: Rect) -> Rect {
        Rect::new(
            rect.x0 - self.left,
            rect.y0 - self.top,
            rect.x1 + self.right,
            rect.y1 + self.bottom
        )
    }
}


//============ Shaped Layouts ================================================

//------------ ShapedLayout --------------------------------------------------

/// A shaped layout ready to be rendered.
#[derive(Clone, Debug)]
pub struct ShapedLayout<'a, P> {
    /// The offset from the overall origin to the layout’s own origin.
    offset: Vec2,

    /// The extents of the contents.
    ///
    /// This includes the actual contents plus padding.
    inner: Rect,

    /// The extents of the frame.
    ///
    /// This is the inner extents grown by the frame widths.
    frame: Option<Rect>,

    /// The extents of the complete layout.
    ///
    /// This is the frame grown by the margins.
    outer: Rect,

    /// The shape of the content.
    shape: Shape<'a, P>,

    /// The properties of the underlying layout.
    properties: &'a P,
}

/// The shape of the content of a layout.
#[derive(Clone, Debug)]
enum Shape<'a, P> {
    /// The layout is a span of text.
    Span {
        /// The shaped text to be rendered.
        text: Text<'a>,
    },

    /// The layout is a rule.
    Rule {
        /// Is this a horizontal rule?
        horizontal: bool,
    },

    /// The layout is a box containing other layouts.
    Box {
        /// Those other layouts.
        children: Vec<ShapedLayout<'a, P>>,
    }
}

impl<'a, P> ShapedLayout<'a, P> {
    pub fn shift(&mut self, offset: Vec2) {
        self.offset += offset;
        if let Shape::Box { ref mut children } = self.shape {
            children.iter_mut().for_each(|child| child.shift(offset));
        }
    }

    pub fn render(
        &self, style: &P::Style,
        base: Vec2, stage: &P::Stage,
        canvas: &mut Sketch,
    )
    where P: Properties {
        self.properties.render(self, style, base, stage, canvas);
        if let Shape::Box { ref children } = self.shape {
            children.iter().for_each(|item| {
                item.render(style, base, stage, canvas)
            })
        }
    }

    pub fn is_span(&self) -> bool {
        matches!(self.shape, Shape::Span { .. })
    }

    pub fn has_frame(&self) -> bool {
        self.frame.is_some()
    }

    pub fn fill_text(&self, base: Vec2, canvas: &mut Sketch) {
        if let Shape::Span { ref text } = self.shape {
            canvas.fill_text(text, (base + self.offset).to_point());
        }
    }

    pub fn stroke_text(&self, base: Vec2, canvas: &mut Sketch) {
        if let Shape::Span { ref text } = self.shape {
            canvas.stroke_text(text, (base + self.offset).to_point());
        }
    }

    pub fn fill_background(&self, base: Vec2, canvas: &mut Sketch) {
        canvas.apply(self.inner + base + self.offset);
        canvas.fill();
    }

    pub fn fill_frame(&self, base: Vec2, canvas: &mut Sketch) {
        if let Some(frame) = self.frame {
            let frame = frame + base + self.offset;
            if
                self.inner.x0 != self.inner.x1
                || self.inner.y0 != self.inner.y1
            {
                let inner = self.inner + base + self.offset;
                canvas.apply(
                    OutlineIter([
                        PathEl::MoveTo(Point::new(frame.x0, frame.y0)),
                        PathEl::LineTo(Point::new(frame.x0, frame.y1)),
                        PathEl::LineTo(Point::new(frame.x1, frame.y1)),
                        PathEl::LineTo(Point::new(frame.x1, frame.y0)),
                        PathEl::ClosePath,
                        PathEl::MoveTo(Point::new(inner.x0, inner.y0)),
                        PathEl::LineTo(Point::new(inner.x1, inner.y0)),
                        PathEl::LineTo(Point::new(inner.x1, inner.y1)),
                        PathEl::LineTo(Point::new(inner.x0, inner.y1)),
                        PathEl::ClosePath,
                    ])
                );
            }
            else {
                canvas.apply(frame);
            }
            canvas.fill();
        }
    }

    pub fn inner(&self, base: Vec2) -> Rect {
        self.inner + base + self.offset
    }

    pub fn outer(&self, base: Vec2) -> Rect {
        self.outer + base + self.offset
    }
}

