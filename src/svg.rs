use std::fmt::Display;

struct Tags {
    tags: Vec<(String, String)>,
}

impl Tags {
    fn new() -> Self {
        Self { tags: Vec::new() }
    }
    fn push<D: std::fmt::Display>(&mut self, tag: &str, value: D) {
        self.tags.push((tag.to_owned(), value.to_string()));
    }

    fn as_string(&self) -> String {
        self.tags
            .iter()
            .filter(|(_, value)| !value.is_empty())
            .map(|(name, value)| format!("{name}=\"{value}\""))
            .collect::<Vec<_>>()
            .join(" ")
    }
}

pub struct SVGRoot {
    width: u32,
    height: u32,
    xmlns: String,
    children: Vec<Box<dyn ToSvgString>>,
}

impl SVGRoot {}

impl ToSvgString for SVGRoot {
    fn svg_open(&mut self) -> String {
        let mut tags = Tags::new();
        tags.push("viewbox", format!("0 0 {} {}", self.width, self.height));
        tags.push("xmlns", &self.xmlns);
        let tags = tags.as_string();
        format!("<svg {tags} style=\"width:100%\">")
    }
    fn svg_close(&mut self) -> String {
        "</svg>".to_owned()
    }

    fn children(&self) -> &Vec<Box<dyn ToSvgString>> {
        &self.children
    }

    fn children_mut(&mut self) -> &mut Vec<Box<dyn ToSvgString>> {
        &mut self.children
    }
}

mod path {
    pub enum PathMovement {
        MoveTo(i64, i64),
        LineTo(i64, i64),
        H(i64),
        V(i64),
    }

    impl PathMovement {
        pub fn as_svg(&self) -> String {
            match self {
                PathMovement::MoveTo(x, y) => format!("M {x} {y}"),
                PathMovement::LineTo(x, y) => format!("L {x} {y}"),
                PathMovement::H(h) => format!("h {h}"),
                PathMovement::V(v) => format!("v {v}"),
            }
        }
    }
}

#[derive(Default)]
pub struct SvgPath {
    movements: Vec<path::PathMovement>,
    fill: String,
    stroke: String,
    children: Vec<Box<dyn ToSvgString>>,
}

impl SvgPath {
    pub fn move_to(mut self, x: i64, y: i64) -> Self {
        self.movements.push(path::PathMovement::MoveTo(x, y));
        self
    }
    pub fn line_to(mut self, x: i64, y: i64) -> Self {
        self.movements.push(path::PathMovement::LineTo(x, y));
        self
    }
    pub fn h(mut self, h: i64) -> Self {
        self.movements.push(path::PathMovement::H(h));
        self
    }
    pub fn v(mut self, v: i64) -> Self {
        self.movements.push(path::PathMovement::V(v));
        self
    }
    pub fn fill<S: Display>(mut self, fill: S) -> Self {
        self.fill = fill.to_string();
        self
    }
    pub fn stroke<S: Display>(mut self, stroke: S) -> Self {
        self.stroke = stroke.to_string();
        self
    }
}

pub fn path() -> SvgPath {
    SvgPath::default()
}
impl SVGRoot {}

impl ToSvgString for SvgPath {
    fn svg_open(&mut self) -> String {
        let mut tags = Tags::new();
        tags.push(
            "d",
            self.movements
                .iter()
                .map(|s| s.as_svg())
                .collect::<Vec<_>>()
                .join(" "),
        );
        tags.push("stroke", self.stroke.clone());
        tags.push("fill", self.fill.clone());
        let tags = tags.as_string();
        format!("<path {tags}>")
    }
    fn svg_close(&mut self) -> String {
        "</path>".to_owned()
    }

    fn children(&self) -> &Vec<Box<dyn ToSvgString>> {
        &self.children
    }

    fn children_mut(&mut self) -> &mut Vec<Box<dyn ToSvgString>> {
        &mut self.children
    }
}

pub trait ToSvgString {
    fn children_mut(&mut self) -> &mut Vec<Box<dyn ToSvgString>>;
    fn children(&self) -> &Vec<Box<dyn ToSvgString>>;
    fn svg_open(&mut self) -> String;
    fn svg_close(&mut self) -> String;
    fn render_children(&mut self, indent: usize) -> String {
        self.children_mut()
            .iter_mut()
            .map(|child| child.svg_render(indent + 1))
            .collect::<Vec<_>>()
            .join(&format!("\n{}", "  ".repeat(indent)))
    }
    fn svg_render(&mut self, indent: usize) -> String {
        let tabs = "  ".repeat(indent);
        format!(
            "\n{tabs}{}{}\n{tabs}{}",
            self.svg_open(),
            self.render_children(indent + 1),
            self.svg_close(),
        )
    }
}

pub trait ToSvgStringExt {
    fn push<T: ToSvgString + 'static>(&mut self, child: T);
    fn with<T: ToSvgString + 'static>(self, child: T) -> Self;
}

impl<K: ToSvgString> ToSvgStringExt for K {
    fn push<T: ToSvgString + 'static>(&mut self, child: T) {
        self.children_mut().push(Box::new(child));
    }
    fn with<T: ToSvgString + 'static>(mut self, child: T) -> Self {
        self.push(child);
        self
    }
}

pub fn root(width: u32, height: u32) -> SVGRoot {
    SVGRoot {
        width,
        height,
        xmlns: "http://www.w3.org/2000/svg".to_owned(),
        children: vec![],
    }
}

#[cfg(test)]
mod test {
    use crate::svg::{path, ToSvgStringExt};

    use super::{root, ToSvgString};

    #[test]
    fn test_root() {
        println!(
            "{}",
            root(1000, 50)
                .with(
                    path()
                        .move_to(0, 50)
                        .h(200)
                        .v(-25)
                        .h(25)
                        .v(25)
                        .h(45)
                        .v(-25)
                        .h(800)
                        .stroke("#000")
                        .fill("transparent")
                )
                .svg_render(0)
        );
    }
}
