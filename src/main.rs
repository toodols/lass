use std::io::Write;

use clap::{Parser, command};
use nom::{
    IResult, Parser as NomParser,
    branch::alt,
    bytes::complete::{tag, take},
    combinator::opt,
    number,
    sequence::{delimited, preceded, terminated},
};

#[derive(Clone, Debug)]
struct StyleDeclaration {
    property: String,
    value: String,
}

#[derive(Clone, Debug)]
struct StyleRule {
    priority: f64,
    selector: Selector,
    declarations: Vec<StyleDeclaration>,
}

#[derive(Clone, Debug)]
enum Selector {
    Class(String), // .class
    Id(String),    // #id
    Tag(String),   // tag
    Parent,        // &
    PseudoClass(Box<Selector>, String),
    Operation {
        left: Box<Selector>,
        operator: Combinator,
        right: Box<Selector>,
    },
    Compound(Vec<Selector>),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Combinator {
    Or,
    Child,
    Descendant,
    Phantom,
}

impl Combinator {
    fn min_precedence() -> u8 {
        0
    }
    fn max_precedence() -> u8 {
        2
    }
    fn precedence(&self) -> u8 {
        match self {
            Combinator::Or => 0,
            Combinator::Descendant => 1,
            Combinator::Child => 1,
            Combinator::Phantom => 2,
        }
    }
}

#[derive(Clone, Debug)]
enum Statement {
    Root,
    Selector(Selector),
    StyleDeclaration(StyleDeclaration),
    PriorityAttribute(f64),
}

#[derive(Parser, Debug)]
#[command()]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    out: Option<String>,

    file: Vec<String>,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum EnforcedIndentStyle {
    Unknown,
    Spaces,
    Tabs,
}

fn indent_level(
    line: &str,
    style: EnforcedIndentStyle,
) -> Result<(usize, EnforcedIndentStyle), &str> {
    let mut chars = line.chars();
    let mut count = 0;

    while let Some(c) = chars.next() {
        match c {
            ' ' => {
                if style == EnforcedIndentStyle::Tabs {
                    return Err("Mixed indentation is not allowed: found spaces after tabs");
                }
                count += 1;
            }
            '\t' => {
                if style == EnforcedIndentStyle::Spaces {
                    return Err("Mixed indentation is not allowed: found tabs after spaces");
                }
                count += 1;
            }
            _ => break,
        }
    }

    let detected_style = if count > 0 {
        match line.chars().next().unwrap() {
            ' ' => EnforcedIndentStyle::Spaces,
            '\t' => EnforcedIndentStyle::Tabs,
            _ => EnforcedIndentStyle::Unknown,
        }
    } else {
        EnforcedIndentStyle::Unknown
    };

    if let EnforcedIndentStyle::Unknown = style {
        Ok((count, detected_style))
    } else if style == detected_style || detected_style == EnforcedIndentStyle::Unknown {
        Ok((count, style))
    } else {
        Err("Indent style does not match enforced style")
    }
}

fn is_blank(line: &str) -> bool {
    line.chars().all(|c| c.is_whitespace())
}

// This was bugging me off cause is_a was being weird (it errors if the parser leaves no unconsumed input)
fn identifier(input: &str) -> IResult<&str, &str> {
    let mut chars = input.char_indices().peekable();

    let first = match chars.next() {
        Some((i, c)) if c.is_ascii_alphabetic() || c == '-' || c == '_' => i,

        _ => return Err(nom::Err::Incomplete(nom::Needed::new(1))),
    };

    let mut end = input.len();
    for (i, c) in chars {
        if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
            continue;
        } else {
            end = i;
            break;
        }
    }

    Ok((&input[end..], &input[first..end]))
}

fn simple_selector(input: &str) -> IResult<&str, Selector> {
    let (input, selector) = alt((
        preceded(tag("."), identifier).map(|name| Selector::Class(name.to_string())),
        preceded(tag("#"), identifier).map(|name| Selector::Id(name.to_string())),
        identifier.map(|name: &str| Selector::Tag(name.to_string())),
        tag("&").map(|_| Selector::Parent),
    ))
    .parse(input)?;
    if let (input, Some(pseudo_class)) = opt(preceded(tag(":"), identifier)).parse(input)? {
        Ok((
            input,
            Selector::PseudoClass(Box::new(selector), pseudo_class.to_string()),
        ))
    } else {
        Ok((input, selector))
    }
}

fn ws(input: &str) -> IResult<&str, &str> {
    take(input.chars().take_while(|c| c.is_whitespace()).count()).parse(input)
}

fn compound_selector(input: &str) -> IResult<&str, Selector> {
    let mut selectors = Vec::new();

    let (input, selector) = simple_selector(input)?;
    selectors.push(selector);
    let mut input = input;
    loop {
        if let Ok((next_input, selector)) = simple_selector(input) {
            input = next_input;
            selectors.push(selector);
        } else {
            break;
        };
    }
    Ok((input, Selector::Compound(selectors)))
}

fn comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("//").parse(input)?;
    Ok(("", input))
}

fn selector_combinator(input: &str) -> IResult<&str, Combinator> {
    alt((tag(">"), tag(">>"), tag(","), tag("::")))
        .map(|s: &str| match s {
            ">" => Combinator::Child,
            ">>" => Combinator::Descendant,
            "," => Combinator::Or,
            "::" => Combinator::Phantom,
            _ => unreachable!(),
        })
        .parse(input)
}

fn selector(input: &str) -> IResult<&str, Selector> {
    let (mut input, left) = compound_selector(input)?;
    let mut simple_selectors: Vec<Selector> = vec![left];
    let mut operators: Vec<Combinator> = vec![];
    loop {
        let (next_input, opt_combinator) =
            opt(delimited(ws, selector_combinator, ws)).parse(input)?;
        if let Some(combinator) = opt_combinator {
            let (next_input, right) = compound_selector(next_input)?;
            input = next_input;
            operators.push(combinator);
            simple_selectors.push(right);
        } else {
            break;
        }
    }
    for precedence in (Combinator::min_precedence()..=Combinator::max_precedence()).rev() {
        let mut i = 0;
        while i < operators.len() {
            if operators[i].precedence() == precedence {
                let left = simple_selectors.remove(i);
                let right = simple_selectors.remove(i);
                let operator = operators.remove(i);
                let combined = Selector::Operation {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
                simple_selectors.insert(i, combined);
            } else {
                i += 1;
            }
        }
    }
    return Ok((input, simple_selectors.remove(0)));
}

// We can do this because Or has the highest precedence and since there are no parentheses allowed in css, Or will only ever appear at the outside of the expression tree
fn flatten_selector_disjunctions(selector: Selector) -> Vec<Selector> {
    match selector {
        Selector::Operation {
            left,
            operator: Combinator::Or,
            right,
        } => {
            let mut left = flatten_selector_disjunctions(*left);
            let mut right = flatten_selector_disjunctions(*right);
            left.append(&mut right);
            left
        }
        other => vec![other],
    }
}

fn selector_disjunctions_product(left: Vec<Selector>, right: Vec<Selector>) -> Vec<Selector> {
    let mut result = Vec::new();
    for l in left.iter() {
        for r in right.iter() {
            result.push(Selector::Operation {
                left: Box::new(l.clone()),
                operator: Combinator::Descendant,
                right: Box::new(r.clone()),
            });
        }
    }
    result
}

fn fold_selector_disjunctions(selectors: Vec<Selector>) -> Selector {
    let mut iter = selectors.into_iter();
    let first = iter.next().unwrap();
    iter.fold(first, |acc, sel| Selector::Operation {
        left: Box::new(acc),
        operator: Combinator::Or,
        right: Box::new(sel),
    })
}

fn priority_attribute(input: &str) -> IResult<&str, Statement> {
    let (input, value) =
        delimited((tag("@priority"), ws, tag("(")), number::double(), tag(")")).parse(input)?;
    Ok((input, Statement::PriorityAttribute(value)))
}

fn declaration(input: &str) -> IResult<&str, StyleDeclaration> {
    let (input, prop) = terminated(identifier, (tag(":"), ws)).parse(input)?;
    Ok((
        "",
        StyleDeclaration {
            property: prop.to_string(),
            value: input.to_string(),
        },
    ))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    let res = priority_attribute(input);
    if let Ok(_) = res {
        return res;
    }

    let res = declaration.map(Statement::StyleDeclaration).parse(input);
    if let Ok(_) = res {
        return res;
    }

    selector.map(Statement::Selector).parse(input)
}

#[derive(Debug)]
struct Tree {
    statement: Statement,
    children: Vec<Tree>,
}

fn selector_tree_to_style_rules(
    tree: &Tree,
    priority: f64,
    parent_selector: Option<Selector>,
) -> Vec<StyleRule> {
    let mut selector = match &tree.statement {
        Statement::Selector(sel) => sel.clone(),
        _ => panic!("Expected selector statement"),
    };

    if let Some(parent_selector) = parent_selector {
        selector = fold_selector_disjunctions(selector_disjunctions_product(
            flatten_selector_disjunctions(parent_selector),
            flatten_selector_disjunctions(selector),
        ))
    }

    let mut children_iter = tree.children.iter();
    let mut style_rules = Vec::new();

    let mut main_style_rule = StyleRule {
        priority,
        selector: selector.clone(),
        declarations: Vec::new(),
    };
    while let Some(child) = children_iter.next() {
        match &child.statement {
            Statement::StyleDeclaration(decl) => {
                main_style_rule.declarations.push(decl.clone());
            }
            Statement::PriorityAttribute(child_priority) => {
                let v = children_iter
                    .next()
                    .expect("Expected selector after priority attribute");
                if let Statement::Selector(_) = v.statement {
                    if !main_style_rule.declarations.is_empty() {
                        style_rules.push(main_style_rule.clone());
                        main_style_rule.declarations = Vec::new();
                    }
                    style_rules.append(&mut selector_tree_to_style_rules(
                        v,
                        *child_priority,
                        Some(selector.clone()),
                    ));
                } else {
                    panic!("Expected selector after priority attribute");
                }
            }
            Statement::Selector(_) => {
                if !main_style_rule.declarations.is_empty() {
                    style_rules.push(main_style_rule.clone());
                    main_style_rule.declarations = Vec::new();
                }
                style_rules.append(&mut selector_tree_to_style_rules(
                    child,
                    0.0,
                    Some(selector.clone()),
                ));
            }
            Statement::Root => {
                unreachable!()
            }
        }
    }
    if !main_style_rule.declarations.is_empty() {
        style_rules.push(main_style_rule.clone());
    }
    style_rules
}

fn root_tree_to_style_rules(tree: &Tree) -> Vec<StyleRule> {
    let mut style_rules = Vec::new();

    let mut children_iter = tree.children.iter();
    while let Some(child) = children_iter.next() {
        match &child.statement {
            Statement::PriorityAttribute(priority) => {
                let v = children_iter
                    .next()
                    .expect("Expected selector after priority attribute");
                if let Statement::Selector(_) = v.statement {
                    style_rules.append(&mut selector_tree_to_style_rules(v, *priority, None));
                } else {
                    panic!("Expected selector after priority attribute");
                }
            }
            Statement::Selector(_) => {
                style_rules.append(&mut selector_tree_to_style_rules(child, 0.0, None));
            }
            Statement::StyleDeclaration(_) => {
                panic!("Style declarations must be under selectors");
            }
            Statement::Root => {
                style_rules.append(&mut root_tree_to_style_rules(child));
            }
        }
    }
    style_rules
}

fn codegen_selector<W: Write>(selector: &Selector, writer: &mut W) {
    match selector {
        Selector::Class(name) => write!(writer, ".{name}").unwrap(),
        Selector::Id(name) => write!(writer, "#{name}").unwrap(),
        Selector::Tag(name) => write!(writer, "{name}").unwrap(),
        Selector::Parent => write!(writer, "&").unwrap(),
        Selector::Compound(selectors) => {
            for selector in selectors {
                codegen_selector(selector, &mut *writer);
            }
        }
        Selector::PseudoClass(selector, name) => {
            codegen_selector(selector, &mut *writer);
            write!(writer, ":{name}").unwrap();
        }
        Selector::Operation {
            left,
            operator,
            right,
        } => {
            codegen_selector(left, &mut *writer);
            match operator {
                Combinator::Or => write!(writer, ", ").unwrap(),
                Combinator::Child => write!(writer, " > ").unwrap(),
                Combinator::Descendant => write!(writer, " >> ").unwrap(),
                Combinator::Phantom => write!(writer, "::").unwrap(),
            }
            codegen_selector(right, &mut *writer);
        }
    }
}

fn codegen_style_rule<W: Write>(style_rule: &StyleRule, num: usize, writer: &mut W) {
    let name = format!("style_rule_{}", num);
    write!(writer, "\nlocal {name} = Instance.new \"StyleRule\"\n").unwrap();
    write!(writer, "{name}.Parent = style_sheet\n").unwrap();
    write!(writer, "table.insert(style_rules, style_rule_{num})\n").unwrap();
    write!(writer, "{name}.Selector = \"").unwrap();
    codegen_selector(&style_rule.selector, &mut *writer);
    write!(writer, "\"\n").unwrap();
    write!(
        writer,
        "{name}.Priority = {priority}\n",
        priority = style_rule.priority
    )
    .unwrap();
    write!(writer, "{name}:SetProperties{{\n").unwrap();
    for (num, declaration) in style_rule.declarations.iter().enumerate() {
        let trailing_comma = if num == style_rule.declarations.len() - 1 {
            ""
        } else {
            ", "
        };
        write!(
            writer,
            "\t[\"{property}\"] = {value}{trailing_comma}\n",
            property = declaration.property,
            value = declaration.value
        )
        .unwrap();
    }
    write!(writer, "}}\n").unwrap();
}

fn codegen<W: Write>(writer: &mut W, rules: Vec<StyleRule>) {
    write!(writer, "local style_sheet = Instance.new \"StyleSheet\"\n").unwrap();
    write!(writer, "local style_rules = {{}}\n").unwrap();

    for (num, rule) in rules.iter().enumerate() {
        codegen_style_rule(rule, num, writer);
    }
    write!(writer, "\nstyle_sheet:SetStyleRules(style_rules)\n").unwrap();
    write!(writer, "\nreturn style_sheet").unwrap();
}

fn main() {
    let args = Args::parse();

    let mut stack: Vec<(usize, Tree)> = Vec::new();
    let mut root: Tree = Tree {
        statement: Statement::Root,
        children: Vec::new(),
    };

    for file in args.file {
        let file = std::fs::read_to_string(file).expect("Failed to read file");

        let mut indent_style = EnforcedIndentStyle::Unknown;
        for line in file.lines() {
            if is_blank(line) {
                continue;
            }
            match indent_level(line, indent_style) {
                Ok((level, new_indent_style)) => {
                    indent_style = new_indent_style;
                    if comment(line.trim()).is_ok() {
                        continue;
                    };
                    let (junk, stmt) = statement(line.trim()).expect("Failed to parse statement");
                    assert!(junk.is_empty(), "Unparsed input remaining: {}", junk);

                    while let Some((last_level, ..)) = stack.last() {
                        if *last_level >= level {
                            let (_, tree) = stack.pop().unwrap();
                            if let Some(&mut (_, ref mut top)) = stack.last_mut() {
                                top.children.push(tree);
                            } else {
                                root.children.push(tree);
                            }
                        } else {
                            break;
                        }
                    }

                    stack.push((
                        level,
                        Tree {
                            statement: stmt,
                            children: Vec::new(),
                        },
                    ));
                }
                Err(e) => panic!("{e}"),
            }
        }

        // Empty remaining stack
        while let Some((_, tree)) = stack.pop() {
            if let Some(&mut (_, ref mut top)) = stack.last_mut() {
                top.children.push(tree);
            } else {
                root.children.push(tree);
            }
        }
    }
    let style_rules = root_tree_to_style_rules(&root);

    let mut file = std::fs::File::create(args.out.unwrap_or("out.lua".to_owned()))
        .expect("Failed to create output file");
    codegen(&mut file, style_rules);
}
