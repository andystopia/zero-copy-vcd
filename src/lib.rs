pub mod svg;

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    convert::{identity, Infallible},
    str::FromStr,
};

use ariadne::{sources, Color, Label, Report};
use chumsky::{
    container::Container,
    error::RichPattern,
    extra,
    prelude::Rich,
    primitive::{any, choice, end, just},
    recursive::recursive,
    span::{SimpleSpan, Span},
    text::{self, newline, whitespace},
    Parser,
};
type Err<'s> = extra::Err<Rich<'s, char, SimpleSpan<usize>>>;

pub fn take_until<'s>(
    parser: impl Parser<'s, &'s str, &'s str, Err<'s>> + Clone + 's,
) -> impl Parser<'s, &'s str, (&'s str, &'s str), Err<'s>> + Clone {
    any()
        .and_is(parser.clone().not())
        .repeated()
        .map_slice(identity)
        .then(parser.map_slice(identity))
}

pub fn handle_errors<'s>(errs: Vec<Rich<'s, char, SimpleSpan>>, source: String, src: String) {
    errs.into_iter().for_each(|e| {
        Report::build(ariadne::ReportKind::Error, source.clone(), e.span().start)
            .with_message(e.to_string())
            .with_label(
                Label::new((source.clone(), e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new((source.clone(), span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .print(sources([(source.clone(), src.clone())]))
            .unwrap()
    });
}

pub fn parse_wire_definition<'s>(
) -> impl Parser<'s, &'s str, VariableDeclaration<'s>, extra::Err<Rich<'s, char, SimpleSpan<usize>>>>
       + Clone {
    just("$var")
        .then_ignore(whitespace())
        .ignore_then(
            any()
                .labelled("wire or register")
                .and_is(just(" ").not())
                .repeated()
                .at_least(1)
                .map_slice(identity),
        )
        .then_ignore(whitespace())
        .then(
            text::digits(10)
                .map_slice(identity)
                .try_map(|s: &str, span| s.parse::<u128>().map_err(|e| Rich::custom(span, e)))
                .labelled("the number of bits in this variable"),
        )
        .then_ignore(whitespace())
        .then(
            any()
                .and_is(just(" ").not())
                .repeated()
                .at_least(1)
                .map_slice(identity),
        )
        .map(|((a, b), c)| (a, b, c))
        .then_ignore(whitespace())
        .then(
            any()
                .and_is(just(" ").not())
                .repeated()
                .at_least(1)
                .map_slice(identity),
        )
        .map(|((a, b, c), d)| (a, b, c, d))
        .map(|(kind, bits, symb, name)| VariableDeclaration {
            kind,
            bits,
            name,
            symb,
        })
        .then_ignore(whitespace())
        .then_ignore(just("$end"))
}

pub fn parse_tag<'s>(
    tag: &'static str,
) -> impl Parser<'s, &'s str, &'s str, extra::Err<Rich<'s, char, SimpleSpan<usize>>>> + Clone {
    just("$")
        .then_ignore(just(tag).labelled(tag))
        .labelled(tag)
        .ignore_then(take_until(just("$end")).map(|(a, _)| a))
}

#[derive(Clone, Debug)]
pub struct Scope<'s> {
    kind: &'s str,
    name: &'s str,
    entries: Vec<ScopeEntry<'s>>,
}

#[derive(Clone, Debug)]
pub struct VariableDeclaration<'s> {
    kind: &'s str,
    bits: u128,
    name: &'s str,
    symb: &'s str,
}

#[derive(Clone, Debug)]
pub enum ScopeEntry<'s> {
    Variable(VariableDeclaration<'s>),
    Comment(&'s str),
    Scope(Box<Scope<'s>>),
}

use chumsky::IterParser;
use termtree::Tree;

pub fn parse_scope<'s>() -> impl Parser<'s, &'s str, Scope<'s>, Err<'s>> {
    recursive(|parser| {
        just("$scope")
            .then_ignore(whitespace())
            .ignore_then(any().and_is(just(" ").not()).repeated().map_slice(identity))
            .then_ignore(whitespace())
            .then(any().and_is(just(" ").not()).repeated().map_slice(identity))
            .then_ignore(whitespace())
            .then_ignore(just("$end").labelled("end"))
            .then_ignore(whitespace())
            .map(|(kind, name)| Scope {
                kind,
                name,
                entries: Vec::new(),
            })
            .then(
                parse_wire_definition()
                    .labelled("wire")
                    .map(ScopeEntry::Variable)
                    .or(parser
                        .map(Box::new)
                        .map(ScopeEntry::Scope)
                        .labelled("nested scope"))
                    .or(parse_tag("comment").map(|res| ScopeEntry::Comment(res)))
                    .padded_by(whitespace())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(mut scope, other)| {
                scope.entries = other;
                scope
            })
            .then_ignore(parse_upscope().labelled("upscope"))
    })
}

pub fn parse_upscope<'s>() -> impl Parser<'s, &'s str, (), Err<'s>> + Clone {
    just("$upscope")
        .then_ignore(whitespace())
        .then_ignore(just("$end"))
        .ignored()
}

#[derive(Clone, Debug)]
pub enum BitValue {
    True,
    False,
    Unset,
}

#[derive(Clone, Debug)]
pub enum VCDEntry<'s> {
    Comment(&'s str),
    Date(&'s str),
    Version(&'s str),
    Timescale(&'s str),
    Scope(Scope<'s>),
    EndDefs,
}

#[derive(Debug)]
pub enum VariableChanges<'s> {
    Time(u128),
    Change(VarChange<'s>),
}

pub enum InferredChange {
    Reals(Vec<f64>),
    Bools(Vec<bool>),
    Bits(Vec<u128>),
}

#[derive(Debug)]
pub struct VariableChange<'s> {
    times: Vec<u128>,
    bits: u128,
    name: &'s str,
    internal_symbol: &'s str,
    change_kinds: Vec<ChangeType>,
    changes: Vec<&'s str>,
}

impl<'s> VariableChange<'s> {
    pub fn new(bits: u128, name: &'s str, internal_symbol: &'s str) -> Self {
        Self {
            times: Default::default(),
            bits,
            name,
            internal_symbol,
            change_kinds: Default::default(),
            changes: Default::default(),
        }
    }

    fn parse_f32(str: &str, kind: ChangeType) -> f32 {
        match kind {
            ChangeType::Real => str.parse().unwrap(),
            ChangeType::Bit => str.parse::<bool>().unwrap().into(),
            ChangeType::Binary => str.parse::<u32>().unwrap() as f32,
        }
    }
    fn parse_f64(str: &str, kind: ChangeType) -> f64 {
        match kind {
            ChangeType::Real => str.parse().unwrap(),
            ChangeType::Bit => str.parse::<bool>().unwrap().into(),
            ChangeType::Binary => str.parse::<u32>().unwrap() as f64,
        }
    }

    fn parse_unsigned(str: &str, kind: ChangeType) -> u128 {
        match kind {
            ChangeType::Real => str.parse::<f64>().unwrap() as u128,
            ChangeType::Bit => str.parse::<bool>().unwrap().into(),
            ChangeType::Binary => str.parse::<u128>().unwrap(),
        }
    }

    fn parse_bool(str: &str, kind: ChangeType) -> bool {
        match kind {
            ChangeType::Real => str.parse::<f64>().unwrap() != 0.0,
            ChangeType::Bit => str.parse::<bool>().unwrap(),
            ChangeType::Binary => str.parse::<u128>().unwrap() != 0,
        }
    }

    pub fn as_f32(&self) -> Vec<f32> {
        self.changes
            .iter()
            .zip(self.change_kinds.iter())
            .map(|(str, kind)| Self::parse_f32(str, kind.clone()))
            .collect()
    }

    pub fn as_f64(&self) -> Vec<f64> {
        self.changes
            .iter()
            .zip(self.change_kinds.iter())
            .map(|(str, kind)| Self::parse_f64(str, kind.clone()))
            .collect()
    }

    pub fn as_unsigned(&self) -> Vec<u128> {
        self.changes
            .iter()
            .zip(self.change_kinds.iter())
            .map(|(str, kind)| Self::parse_unsigned(str, kind.clone()))
            .collect()
    }

    pub fn as_bool(&self) -> Vec<bool> {
        self.changes
            .iter()
            .zip(self.change_kinds.iter())
            .map(|(str, kind)| Self::parse_bool(str, kind.clone()))
            .collect()
    }

    pub fn as_inferred(&self) -> InferredChange {
        match self.change_kinds.first().unwrap() {
            ChangeType::Real => InferredChange::Reals(self.as_f64()),
            ChangeType::Bit => InferredChange::Bools(self.as_bool()),
            ChangeType::Binary => InferredChange::Bits(self.as_unsigned()),
        }
    }

    pub fn times(&self) -> &[u128] {
        self.times.as_ref()
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn internal_symbol(&self) -> &str {
        self.internal_symbol
    }

    pub fn bits(&self) -> u128 {
        self.bits
    }
}

pub fn parse_variable_changes<'s>() -> impl Parser<'s, &'s str, Vec<VariableChanges<'s>>, Err<'s>> {
    parse_binary_vcd()
        .map(VariableChanges::Change)
        .or(parse_bit_vcd().map(VariableChanges::Change))
        .or(parse_time_interval().map(VariableChanges::Time))
        .padded()
        .repeated()
        .collect()
}
pub fn parse_vcd<'s>() -> impl Parser<'s, &'s str, Vec<VCDEntry<'s>>, Err<'s>> {
    parse_tag("comment")
        .map(VCDEntry::Comment)
        .or(parse_tag("date").map(VCDEntry::Date))
        .or(parse_tag("version").map(VCDEntry::Version))
        .or(parse_tag("timescale").map(VCDEntry::Timescale))
        .or(parse_scope().map(VCDEntry::Scope))
        .or(parse_tag("enddefinitions").to(VCDEntry::EndDefs))
        .padded()
        .repeated()
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChangeType {
    Real,
    Bit,
    Binary,
}

#[derive(Debug, Clone)]
pub struct VarChange<'s> {
    change: &'s str,
    variable: &'s str,
    kind: ChangeType,
}

pub fn parse_binary_vcd<'s>() -> impl Parser<'s, &'s str, VarChange<'s>, Err<'s>> {
    just("b")
        .ignore_then(
            choice((just("0"), just("1"), just("x")))
                .repeated()
                .at_least(1)
                .map_slice(identity),
        )
        .then_ignore(whitespace())
        .then(any().and_is(newline().not()).repeated().map_slice(identity))
        .map(|(change, variable)| VarChange {
            change,
            variable,
            kind: ChangeType::Binary,
        })
}

fn parse_bit_vcd<'s>() -> impl Parser<'s, &'s str, VarChange<'s>, Err<'s>> {
    any()
        .filter(|v: &char| v.is_ascii_digit() || *v == 'x')
        .map_slice(identity)
        .then(
            any()
                .and_is(any().filter(|v: &char| !v.is_whitespace()))
                .repeated()
                .map_slice(identity)
                .labelled("variable name or value"),
        )
        .map(|(change, variable)| VarChange {
            change,
            variable,
            kind: ChangeType::Bit,
        })
}

#[derive(Debug, Clone)]
pub struct PreludeScope<'s> {
    kind: &'s str,
    name: &'s str,
    empty: bool,
    subscopes: HashMap<&'s str, PreludeScope<'s>>,
    // there's really no reason to store
    // a whole variable here, since we're just going
    // to store it at the top level anyways, so just
    // store them as their unique symbol.
    variables: HashSet<&'s str>,
}

impl<'s> PreludeScope<'s> {
    fn from_scope(vcd: &mut VCDPrelude<'s>, scope: Scope<'s>) -> Self {
        let mut subscopes = HashMap::new();
        let mut variables = HashSet::new();
        let mut empty = true;
        for entry in scope.entries {
            match entry {
                ScopeEntry::Variable(variable) => {
                    vcd.variables.insert(
                        variable.symb,
                        VariableChange::new(variable.bits, variable.name, variable.symb),
                    );
                    variables.insert(variable.symb);
                    empty = false;
                }
                ScopeEntry::Scope(scope) => {
                    let scope = PreludeScope::from_scope(vcd, *scope);
                    if scope.empty {
                        empty = false;
                    }
                    subscopes.insert(scope.kind, scope);
                }
                // skip comments
                ScopeEntry::Comment(_) => {}
            }
        }
        Self {
            kind: scope.kind,
            name: scope.name,
            empty,
            subscopes,
            variables,
        }
    }
}

#[derive(Default, Debug)]
struct VCDPrelude<'s> {
    date: Option<&'s str>,
    version: Option<&'s str>,
    timescale: Option<&'s str>,
    comments: Vec<&'s str>,
    scopes: HashMap<&'s str, PreludeScope<'s>>,
    variables: HashMap<&'s str, VariableChange<'s>>,
    max_time: u128,
}

impl<'s> VCDPrelude<'s> {
    fn push_tag(&mut self, tag: VCDEntry<'s>) {
        match tag {
            VCDEntry::Comment(comment) => self.comments.push(comment),
            VCDEntry::Date(date) => self.timescale = Some(date),
            VCDEntry::Version(version) => self.version = Some(version),
            VCDEntry::Timescale(ts) => self.timescale = Some(ts),
            VCDEntry::Scope(scope) => {
                let prelude_scope = PreludeScope::from_scope(self, scope);
                self.scopes.insert(prelude_scope.name, prelude_scope);
            }
            VCDEntry::EndDefs => (),
        }
    }

    fn populated_scope_tree_root(
        &'s self,
        scope: &'s HashMap<&'s str, PreludeScope<'s>>,
    ) -> impl Iterator<Item = termtree::Tree<Cow<'s, str>>> + 's {
        scope.values().filter(|val| !val.empty).map(|value| {
            termtree::Tree::new(Cow::Borrowed(value.name)).with_leaves(
                self.populated_scope_tree_root(&value.subscopes).chain(
                    value
                        .variables
                        .iter()
                        .map(|s| {
                            let var = self.variables.get(s).unwrap();
                            let kind = match var.change_kinds.first() {
                                Some(ChangeType::Bit) => "boolean",
                                Some(ChangeType::Binary) => "unsigned",
                                Some(ChangeType::Real) => "float",
                                None => "unknown",
                            };
                            format!("{} ({} bits : {kind})", var.name(), var.bits())
                        })
                        .map(Cow::Owned)
                        .map(termtree::Tree::new),
                ),
            )
        })
    }

    pub fn populated_scope_tree(&'s self) -> termtree::Tree<Cow<'s, str>> {
        Tree::new(Cow::Borrowed(".")).with_leaves(self.populated_scope_tree_root(&self.scopes))
    }

    pub fn push_variable(&mut self, var: &'s str, time: u128, change: ChangeType, value: &'s str) {
        self.max_time = self.max_time.max(time);
        self.variables.entry(var).and_modify(|v| {
            v.times.push(time);
            v.change_kinds.push(change);
            v.changes.push(value);
        });
    }
}

/// What I want is something like vcd.scope("testbench").var("c1") -> ([100, 200, 300], [3, 2, 1])

fn parse_time_interval<'s>() -> impl Parser<'s, &'s str, u128, Err<'s>> + Clone {
    just("#").ignore_then(
        text::int(10).try_map(|f: &str, span| f.parse().map_err(|e| Rich::custom(span, e))),
    )
}

#[cfg(test)]
mod test {
    use chumsky::{
        extra,
        prelude::Rich,
        primitive::{end, just},
        span::SimpleSpan,
        text::whitespace,
        Parser,
    };

    use crate::{
        handle_errors, parse_bit_vcd, parse_scope, parse_tag, parse_variable_changes, parse_vcd,
        parse_wire_definition, take_until, VCDPrelude,
    };
    #[test]

    fn test_parse_bit() {
        wrap_errors(parse_bit_vcd(), "c 1");
    }

    #[test]
    fn test() {
        let res = take_until(just("$end")).parse("$start and then $nd");

        handle_errors(
            res.errors().cloned().collect::<Vec<_>>(),
            "source.vcd".to_owned(),
            "$start and then $nd".to_owned(),
        );
    }

    pub fn wrap_errors<'s, T>(
        parser: impl Parser<'s, &'s str, T, extra::Err<Rich<'s, char, SimpleSpan<usize>>>>,
        input: &'s str,
    ) -> Option<T> {
        let parse = parser.parse(input);
        handle_errors(
            parse.errors().cloned().collect::<Vec<_>>(),
            "<>".to_owned(),
            input.to_owned(),
        );
        if parse.has_output() {
            parse.into_output()
        } else {
            None
        }
    }
    #[test]
    fn test_wire() {
        wrap_errors(parse_wire_definition(), "$var wire 32 % en $end");
    }

    #[test]
    fn test_tag() {
        wrap_errors(parse_tag("upscope"), "$upscope $end");
    }

    #[test]
    fn test_most_vcd() {
        let (res, changes) = wrap_errors(
            parse_vcd()
                .then_ignore(whitespace())
                .then(parse_variable_changes())
                .then_ignore(end()),
            include_str!("../hexbench2.vcd"),
        )
        .unwrap();

        let mut vcd = VCDPrelude::default();

        res.into_iter().for_each(|v| vcd.push_tag(v));

        let mut time = 0;
        for change in changes {
            match change {
                crate::VariableChanges::Time(t) => time = t,
                crate::VariableChanges::Change(ch) => {
                    vcd.push_variable(ch.variable, time, ch.kind, ch.change);
                }
            }
        }
        println!("{}", vcd.populated_scope_tree());
    }

    #[test]
    fn test_scope() {
        wrap_errors(
            parse_scope(),
            "$scope module logic $end
            $scope module logic $end
            $var wire 8 # data $end
            $var wire 1 $ data_valid $end
            $var wire 1 % en $end
            $var wire 1 & rx_en $end
            $var wire 1 ' tx_en $end
            $var wire 1 ( empty $end
            $var wire 1 ) underrun $end
            $upscope $end
            $upscope $end
            ",
        );
    }
}
