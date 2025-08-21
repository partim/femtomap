//! Complex text arrangements.

use std::fmt;
use kurbo::{PathEl, Point, Rect, Vec2};
use crate::render::{Canvas, Font, OutlineIter, Sketch, Text};


//------------ Layout ---------------------------------------------------------

/// A layout is an arrangement of text.
#[derive(Clone, Debug)]
pub struct Layout<P: Properties> {
    content: LayoutContent<P>,
}

#[derive(Clone, Debug)]
enum LayoutContent<P: Properties> {
    Vbox(Vbox<P>),
    Hbox(Hbox<P>),
    Span(Span<P>),
}

impl<P: Properties> Layout<P> {
    fn new(content: LayoutContent<P>) -> Self {
        Layout {
            content
        }
    }

    pub fn vbox(
        halign: Align, vbase: Base, properties: P, lines: Vec<Block<P>>
    ) -> Self {
        Self::new(LayoutContent::Vbox(
            Vbox::new(halign, vbase, properties, lines))
        )
    }

    pub fn hbox(
        hbase: Base, valign: Align, properties: P, rows: Vec<Block<P>>
    ) -> Self {
        Self::new(LayoutContent::Hbox(
            Hbox::new(hbase, valign, properties, rows))
        )
    }

    pub fn span(text: P::SpanText, properties: P) -> Self {
        Self::new(LayoutContent::Span(Span::new(text, properties)))
    }

    pub fn properties(&self) -> &P {
        match &self.content {
            LayoutContent::Vbox(inner) => &inner.properties,
            LayoutContent::Hbox(inner) => &inner.properties,
            LayoutContent::Span(inner) => &inner.properties,
        }
    }

    pub fn update_properties(
        &mut self, base: &P, op: impl Fn(&mut P, &P),
    ) {
        match &mut self.content {
            LayoutContent::Vbox(inner) => inner.update_properties(base, &op),
            LayoutContent::Hbox(inner) => inner.update_properties(base, &op),
            LayoutContent::Span(inner) => inner.update_properties(base, &op),
        }
    }

    pub fn properties_mut(&mut self) -> &mut P {
        match &mut self.content {
            LayoutContent::Vbox(ref mut inner) => &mut inner.properties,
            LayoutContent::Hbox(ref mut inner) => &mut inner.properties,
            LayoutContent::Span(ref mut inner) => &mut inner.properties,
        }
    }

    pub fn shape(
        &self, base: Vec2, style: &P::Style, canvas: &Canvas
    ) -> ShapedLayout<'_, P>
    where P: Properties {
        let mut block = match &self.content {
            LayoutContent::Vbox(inner) => inner.shape(style, canvas),
            LayoutContent::Hbox(inner) => inner.shape(style, canvas),
            LayoutContent::Span(inner) => inner.shape(style, canvas),
        };
        block.shift(-block.base + base);
        ShapedLayout { block }
    }
}


//------------ Block --------------------------------------------------------

/// A block is a component of a layout.
#[derive(Clone, Debug)]
pub struct Block<P: Properties> {
    /// The content of the block.
    content: BlockContent<P>,
}

#[derive(Clone, Debug)]
enum BlockContent<P: Properties> {
    Vbox(Vbox<P>),
    Hbox(Hbox<P>),
    Span(Span<P>),
    Rule(Rule<P>),
    Anchor,
}

impl<P: Properties> Block<P> {
    fn new(content: BlockContent<P>) -> Self {
        Block { content }
    }

    pub fn vbox(
        halign: Align, vbase: Base, properties: P, lines: Vec<Block<P>>
    ) -> Self {
        Self::new(
            BlockContent::Vbox(Vbox::new(halign, vbase, properties, lines))
        )
    }

    pub fn hbox(
        hbase: Base, valign: Align, properties: P, rows: Vec<Block<P>>
    ) -> Self {
        Self::new(
            BlockContent::Hbox(Hbox::new(hbase, valign, properties, rows))
        )
    }

    pub fn span(text: P::SpanText, properties: P) -> Self {
        Self::new(BlockContent::Span(Span::new(text, properties)))
    }

    pub fn hrule(properties: P) -> Self {
        Self::new(BlockContent::Rule(Rule::new(true, properties)))
    }

    pub fn vrule(properties: P) -> Self {
        Self::new(BlockContent::Rule(Rule::new(false, properties)))
    }

    pub fn anchor() -> Self {
        Self::new(BlockContent::Anchor)
    }

    fn update_properties(
        &mut self, base: &P, op: &impl Fn(&mut P, &P),
    ) {
        match self.content {
            BlockContent::Vbox(ref mut inner) => {
                inner.update_properties(base, op)
            }
            BlockContent::Hbox(ref mut inner) => {
                inner.update_properties(base, op)
            }
            BlockContent::Span(ref mut inner) => {
                inner.update_properties(base, op)
            }
            BlockContent::Rule(ref mut inner) => {
                inner.update_properties(base, op)
            }
            BlockContent::Anchor => { }
        }
    }
}

impl<P: Properties> Block<P> {
    fn shape_content(
        &self, style: &P::Style, canvas: &Canvas
    ) -> Option<ShapedBlock<'_, P>> {
        match self.content {
            BlockContent::Vbox(ref vbox) => Some(vbox.shape(style, canvas)),
            BlockContent::Hbox(ref hbox) => Some(hbox.shape(style, canvas)),
            BlockContent::Span(ref span) => Some(span.shape(style, canvas)),
            BlockContent::Rule(ref rule) => Some(rule.shape(style, canvas)),
            BlockContent::Anchor => None
        }
    }
}

impl<P: Properties> From<Layout<P>> for Block<P> {
    fn from(src: Layout<P>) -> Self {
        Self {
            content: match src.content {
                LayoutContent::Vbox(inner) => BlockContent::Vbox(inner),
                LayoutContent::Hbox(inner) => BlockContent::Hbox(inner),
                LayoutContent::Span(inner) => BlockContent::Span(inner),
            }
        }
    }
}


//------------ Vbox ----------------------------------------------------------

/// A box with its content stacked vertically.
#[derive(Clone, Debug)]
struct Vbox<P: Properties> {
    /// The horizontal alignment of the content.
    ///
    /// This also determines the horizontal base of the box.
    halign: Align,

    /// The vertical base of the box.
    vbase: Base,

    /// The properties of the box.
    properties: P,

    /// The content.
    lines: Vec<Block<P>>,
}

impl<P: Properties> Vbox<P> {
    fn new(
        halign: Align, vbase: Base, properties: P, lines: Vec<Block<P>>
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
    ) -> ShapedBlock<'_, P>
    where P: Properties {
        let mut inner = Rect::default();
        let mut children = Vec::with_capacity(self.lines.len());
        let mut ybase = None;

        for item in &self.lines {
            let mut shape = match item.shape_content(style, canvas) {
                Some(shape) => shape,
                None => {
                    // The only non-shape block is currently anchor. So
                    // update ybase if necessary.
                    match self.vbase {
                        Base::FirstAnchor => {
                            if ybase.is_none() {
                                ybase = Some(inner.y1);
                            }
                        }
                        Base::LastAnchor => {
                            ybase = Some(inner.y1);
                        }
                        _ => { }
                    }
                    continue;
                }
            };

            // Shift vertically to the bottom of the stack. Remember that the
            // outer rect’s y0 can be non-null.
            let yoff = inner.y1 - shape.outer.y0;

            // Shift horizontally according to the alignment.
            let xoff = match self.halign {
                // The leftmost point needs to be at 0.
                Align::Start => -shape.outer.x0,

                // The center needs to be at 0.
                Align::Center => -shape.center.x,

                // The base needs to be at 0.
                Align::Base => -shape.base.x,

                // The rightmost point needs to be at 0.
                Align::End => -shape.outer.x1,
            };

            shape.shift(Vec2::new(xoff, yoff));
            inner = inner.union(shape.outer);

            // Update the vertical base if necessary.
            match self.vbase {
                Base::FirstBase => {
                    if ybase.is_none() {
                        ybase = Some(shape.base.y);
                    }
                }
                Base::LastBase => {
                    ybase = Some(shape.base.y);
                }
                _ => { }
            }

            children.push(shape);
        }

        let center = inner.center();
        let base = Vec2::new(
            0.,
            match self.vbase {
                Base::Start => inner.y0,
                Base::Center => center.y,
                Base::End => inner.y1,
                Base::LastAnchor => ybase.unwrap_or(inner.y1),
                _ => ybase.unwrap_or(0.),
            }
        );

        let padded = self.properties.padding(style).grow_rect(inner);
        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        // Grow all hrules to the full width of the box.
        for item in &mut children {
            if matches!(item.shape, Shape::Rule { horizontal: true }) {
                item.inner.x0 = inner.x0;
                item.inner.x1 = inner.x1;
                if let Some(item_frame) = item.frame.as_mut() {
                    item_frame.x0 = frame.map(|f| f.x0).unwrap_or(inner.x0);
                    item_frame.x1 = frame.map(|f| f.x1).unwrap_or(inner.x1);
                }
                item.outer.x0 = outer.x0;
                item.outer.x1 = outer.x1;
            }
        }

        ShapedBlock {
            inner: padded, frame, outer, center, base,
            shape: Shape::Box { children },
            properties: &self.properties,
        }
    }
}


//------------ Hbox ----------------------------------------------------------

/// A sequence of layouts stacked horizontally.
#[derive(Clone, Debug)]
struct Hbox<P: Properties> {
    hbase: Base,
    valign: Align,
    properties: P,
    columns: Vec<Block<P>>,
}

impl<P: Properties> Hbox<P> {
    fn new(
        hbase: Base, valign: Align, properties: P, columns: Vec<Block<P>>
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
    ) -> ShapedBlock<'_, P>
    where P: Properties {
        let mut inner = Rect::default();
        let mut children = Vec::with_capacity(self.columns.len());
        let mut xbase = None;

        for item in &self.columns {
            let mut shape = match item.shape_content(style, canvas) {
                Some(shape) => shape,
                None => {
                    // The only non-shape block is currently anchor. So
                    // update ybase if necessary.
                    match self.hbase {
                        Base::FirstAnchor => {
                            if xbase.is_none() {
                                xbase = Some(inner.x1);
                            }
                        }
                        Base::LastAnchor => {
                            xbase = Some(inner.x1);
                        }
                        _ => { }
                    }
                    continue;
                }
            };

            // Shift horizontally to the right end of the stack.
            let xoff = inner.x1 - shape.outer.x0;

            // Shift horizontally according to the alignment.
            let yoff = match self.valign {
                // The topmost point needs to be at 0.
                Align::Start => -shape.outer.y0,

                // The center needs to be at 0.
                Align::Center => -shape.center.y,

                // The base needs to be at 0.
                Align::Base => -shape.base.y,

                // The bottommost point needs to be at 0.
                Align::End => -shape.outer.y1,
            };

            shape.shift(Vec2::new(xoff, yoff));
            inner = inner.union(shape.outer);

            // Update the vertical base if necessary.
            match self.hbase {
                Base::FirstBase => {
                    if xbase.is_none() {
                        xbase = Some(shape.base.x);
                    }
                }
                Base::LastBase => {
                    xbase = Some(shape.base.x);
                }
                _ => { }
            }

            children.push(shape);
        }

        // Grow all vrules to the full height of the box.
        for item in &mut children {
            if matches!(item.shape, Shape::Rule { horizontal: false }) {
                item.inner.y0 = inner.y0;
                item.inner.y1 = inner.y1;
            }
        }

        let center = inner.center();
        let base = Vec2::new(
            match self.hbase {
                Base::Start => inner.x0,
                Base::Center => center.x,
                Base::End => inner.x1,
                Base::LastAnchor => xbase.unwrap_or(inner.x1),
                _ => xbase.unwrap_or(0.),
            },
            0.
        );

        let inner = self.properties.padding(style).grow_rect(inner);
        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        ShapedBlock {
            inner, frame, outer, center, base,
            shape: Shape::Box { children },
            properties: &self.properties,
        }
    }
}


//------------ Span ----------------------------------------------------------

/// A run of text rendered with the same properties.
#[derive(Clone, Debug)]
pub struct Span<P: Properties> {
    text: P::SpanText,
    properties: P,
}

impl<P: Properties> Span<P> {
    fn new(text: P::SpanText, properties: P) -> Self {
        Self { text, properties }
    }

    fn update_properties(
        &mut self, base: &P, op: &impl Fn(&mut P, &P),
    ) {
        op(&mut self.properties, base);
    }

    fn shape(
        &self, style: &P::Style, canvas: &Canvas
    ) -> ShapedBlock<'_, P>
    where P: Properties {
        let text = canvas.prepare_text(
            self.properties.span_text(&self.text, style),
            self.properties.font(style),
        );
        let metrics = canvas.text_metrics(&text);

        let extents = if self.properties.packed(style) {
            metrics.inked()
        }
        else {
            metrics.logical()
        };

        let inner = self.properties.padding(style).grow_rect(extents);
        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        ShapedBlock {
            inner, frame, outer,
            base: Vec2::default(),
            center: inner.center(),
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
    ) -> ShapedBlock<'_, P>
    where P: Properties {
        let inner = Rect::default();
        let (frame, outer) = frame_and_outer_extents(
            &self.properties, style, inner
        );

        ShapedBlock {
            inner, frame, outer,
            base: Vec2::default(),
            center: Point::default(),
            shape: Shape::Rule { horizontal: self.horizontal },
            properties: &self.properties,
        }
    }
}


//------------ Properties ----------------------------------------------------

/// A type that can provide properties for a layout.
pub trait Properties: Sized {
    /// The style that carries more information for deriving the properties.
    type Style;

    /// The rendering stage.
    ///
    /// This type is used by `render` to identify which phase of rendering
    /// of the layout is currently performed.
    type Stage;

    /// The type for the text held by a span.
    //
    //  This is required to be Clone and Debug so we can derive those all
    //  over the place here. XXX Might want to do this properly later.
    type SpanText: Clone + fmt::Debug;

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

    /// Returns the span text for a span with this style.
    fn span_text<'a>(
        &self, text: &'a Self::SpanText, style: &Self::Style
    ) -> &'a str;

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
        layout: &ShapedBlock<Self>,
        style: &Self::Style,
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


//------------ Base ----------------------------------------------------------

/// Where the base of a box is located.
#[derive(Clone, Copy, Debug)]
pub enum Base {
    /// The upper or left extent forms the base.
    Start,

    /// The base of the first item forms the base.
    FirstBase,

    /// The base is the first anchor item of the box.
    FirstAnchor,

    /// The center of the layout forms the base.
    Center,

    /// The base is the last anchor item of the box.
    LastAnchor,

    /// The base of the last item forms the base.
    LastBase,

    /// The lower or right extent forms the base
    End,
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


//============ Shaped Objects ===============================================

//------------ ShapedLayout -------------------------------------------------

/// A shaped layout ready to be rendered.
#[derive(Clone, Debug)]
pub struct ShapedLayout<'a, P> {
    block: ShapedBlock<'a, P>,
}

impl<'a, P: Properties> ShapedLayout<'a, P> {
    pub fn render(
        &self, style: &P::Style, stage: &P::Stage, canvas: &mut Sketch,
    ) {
        self.block.render(style, stage, canvas);
    }
}

//------------ ShapedBlock --------------------------------------------------

/// A shaped block ready to be rendered.
#[derive(Clone, Debug)]
pub struct ShapedBlock<'a, P> {
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

    /// The center of the layout.
    center: Point,

    /// The base of the layout.
    base: Vec2,

    /// The shape of the content.
    shape: Shape<'a, P>,

    /// The properties of the underlying layout.
    properties: &'a P,
}

/// The shape of the content of a layout.
#[derive(Clone, Debug)]
enum Shape<'a, P> {
    /// The shape is a span of text.
    Span {
        /// The shaped text to be rendered.
        text: Text<'a>,
    },

    /// The shape is a rule.
    Rule {
        /// Is this a horizontal rule?
        horizontal: bool,
    },

    /// The shape is a box containing other blocks.
    Box {
        /// Those other layouts.
        children: Vec<ShapedBlock<'a, P>>,
    }
}

impl<'a, P> ShapedBlock<'a, P> {
    pub fn shift(&mut self, offset: Vec2) {
        self.inner = self.inner + offset;
        if let Some(frame) = self.frame.as_mut() {
            *frame = *frame + offset;
        }
        self.outer = self.outer + offset;
        self.center += offset;
        self.base += offset;

        if let Shape::Box { ref mut children } = self.shape {
            children.iter_mut().for_each(|child| child.shift(offset));
        }
    }

    fn render(
        &self, style: &P::Style, stage: &P::Stage, canvas: &mut Sketch,
    )
    where P: Properties {
        self.properties.render(self, style, stage, canvas);
        if let Shape::Box { ref children } = self.shape {
            children.iter().for_each(|item| {
                item.render(style, stage, canvas)
            })
        }
    }

    pub fn is_span(&self) -> bool {
        matches!(self.shape, Shape::Span { .. })
    }

    pub fn has_frame(&self) -> bool {
        self.frame.is_some()
    }

    pub fn fill_text(&self, canvas: &mut Sketch) {
        if let Shape::Span { ref text } = self.shape {
            canvas.fill_text(text, self.base.to_point());
        }
    }

    pub fn stroke_text(&self, canvas: &mut Sketch) {
        if let Shape::Span { ref text } = self.shape {
            canvas.stroke_text(text, self.base.to_point());
        }
    }

    pub fn fill_background(&self, canvas: &mut Sketch) {
        canvas.apply(self.inner);
        canvas.fill();
    }

    pub fn fill_frame(&self, canvas: &mut Sketch) {
        if let Some(frame) = self.frame {
            if
                self.inner.x0 != self.inner.x1
                || self.inner.y0 != self.inner.y1
            {
                let inner = self.inner;
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

    pub fn inner(&self) -> Rect {
        self.inner
    }

    pub fn outer(&self) -> Rect {
        self.outer
    }
}

