use std::{collections::HashMap, io::Write, path::PathBuf};

use clap::Parser;
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
    children: Vec<StyleRule>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Selector {
    Class(String),         // .class
    Id(String),            // #id
    Tag(String),           // tag
    Parent(Box<Selector>), // &
    Empty,
    PseudoClass(Box<Selector>, String),
    Operation {
        left: Box<Selector>,
        operator: Combinator,
        right: Box<Selector>,
    },
    Compound(Vec<Selector>),
    PseudoInstance(Box<Selector>, String),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Combinator {
    Or,
    Child,
    Descendant,
}

impl Combinator {
    fn min_precedence() -> u8 {
        0
    }
    fn max_precedence() -> u8 {
        1
    }
    fn precedence(&self) -> u8 {
        match self {
            Combinator::Or => 0,
            Combinator::Descendant => 1,
            Combinator::Child => 1,
        }
    }
}

#[derive(Clone, Debug)]
enum Statement {
    Root,
    Selector(Selector),
    StyleDeclaration(StyleDeclaration),
    PriorityAttribute(f64),
    MixinDefinition(String),
    Include(String),
}

#[derive(Parser, Debug)]
#[command()]
struct Args {
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
    ))
    .parse(input)
    .unwrap_or((input, Selector::Empty));

    if let Ok((input, Some(pseudo_class))) = opt(preceded(tag(":"), identifier)).parse(input) {
        Ok((
            input,
            Selector::PseudoClass(Box::new(selector), pseudo_class.to_string()),
        ))
    } else if let Ok((input, Some(pseudo_instance))) =
        opt(preceded(tag("::"), identifier)).parse(input)
    {
        Ok((
            input,
            Selector::PseudoInstance(Box::new(selector), pseudo_instance.to_string()),
        ))
    } else {
        if selector == Selector::Empty {
            Err(nom::Err::Incomplete(nom::Needed::new(1)))
        } else {
            Ok((input, selector))
        }
    }
}

fn ws(input: &str) -> IResult<&str, &str> {
    take(input.chars().take_while(|c| c.is_whitespace()).count()).parse(input)
}

fn compound_selector(input: &str) -> IResult<&str, Selector> {
    let (input, parent) = opt(tag("&")).parse(input)?;
    let mut selectors = Vec::new();

    let (input, selector) = simple_selector(input).unwrap_or((input, Selector::Empty));
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
    if selectors.len() == 1 {
        if parent.is_some() {
            Ok((input, Selector::Parent(Box::new(selectors.pop().unwrap()))))
        } else if selectors[0] == Selector::Empty {
            Err(nom::Err::Incomplete(nom::Needed::new(1)))
        } else {
            Ok((input, selectors.pop().unwrap()))
        }
    } else {
        if parent.is_some() {
            Ok((
                input,
                Selector::Parent(Box::new(Selector::Compound(selectors))),
            ))
        } else {
            Ok((input, Selector::Compound(selectors)))
        }
    }
}

fn comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("//").parse(input)?;
    Ok(("", input))
}

fn selector_combinator(input: &str) -> IResult<&str, Combinator> {
    alt((tag(">>"), tag(">"), tag(",")))
        .map(|s: &str| match s {
            ">" => Combinator::Child,
            ">>" => Combinator::Descendant,
            "," => Combinator::Or,
            _ => unreachable!(),
        })
        .parse(input)
}

fn selector(input: &str) -> IResult<&str, Selector> {
    let (mut input, left) = compound_selector(input).unwrap_or((input, Selector::Empty));
    let mut compound_selectors: Vec<Selector> = vec![left];
    let mut operators: Vec<Combinator> = vec![];
    loop {
        let (after_ws, ws_str) = ws.parse(input)?;

        // Try to parse explicit combinator
        if let Ok((after_combinator, combinator)) = selector_combinator.parse(after_ws) {
            let (after_combinator_ws, _) = ws.parse(after_combinator)?;
            if let Ok((next_input, right)) = compound_selector(after_combinator_ws) {
                input = next_input;
                operators.push(combinator);
                compound_selectors.push(right);
            } else {
                break;
            }
        } else if !ws_str.is_empty() {
            // We have whitespace but no explicit combinator, check if there's another selector
            if let Ok((next_input, right)) = compound_selector(after_ws) {
                input = next_input;
                operators.push(Combinator::Descendant);
                compound_selectors.push(right);
            } else {
                break;
            }
        } else {
            break;
        }
    }
    if compound_selectors.len() == 1 && matches!(compound_selectors[0], Selector::Empty) {
        return Err(nom::Err::Incomplete(nom::Needed::new(1)));
    }
    for precedence in (Combinator::min_precedence()..=Combinator::max_precedence()).rev() {
        let mut i = 0;
        while i < operators.len() {
            if operators[i].precedence() == precedence {
                let left = compound_selectors.remove(i);
                let right = compound_selectors.remove(i);
                let operator = operators.remove(i);
                let combined = Selector::Operation {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
                compound_selectors.insert(i, combined);
            } else {
                i += 1;
            }
        }
    }
    return Ok((input, compound_selectors.remove(0)));
}

fn priority_attribute(input: &str) -> IResult<&str, Statement> {
    let (input, value) = delimited(
        (tag("@priority"), ws, tag("("), ws),
        number::double(),
        (ws, tag(")")),
    )
    .parse(input)?;
    Ok((input, Statement::PriorityAttribute(value)))
}

fn mixin_definition(input: &str) -> IResult<&str, Statement> {
    let (input, name) = preceded((tag("@mixin"), ws), identifier).parse(input)?;
    Ok((input, Statement::MixinDefinition(name.to_string())))
}

fn include_statement(input: &str) -> IResult<&str, Statement> {
    let (input, name) = preceded((tag("@include"), ws), identifier).parse(input)?;
    Ok((input, Statement::Include(name.to_string())))
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
    if let Ok((input, _)) = res {
        if input.trim().len() == 0 {
            return res;
        }
    }

    let res = mixin_definition(input);
    if let Ok((input, _)) = res {
        if input.trim().len() == 0 {
            return res;
        }
    }

    let res = include_statement(input);
    if let Ok((input, _)) = res {
        if input.trim().len() == 0 {
            return res;
        }
    }

    let res = selector.map(Statement::Selector).parse(input);
    if let Ok((mut input, stmt)) = res {
        input = input.trim_start();
        if let Ok((after_colon, _)) = tag::<&str, &str, nom::error::Error<&str>>(":").parse(input) {
            input = after_colon;
        }
        if input.trim().len() == 0 {
            return Ok((input, stmt));
        }
    }

    declaration.map(Statement::StyleDeclaration).parse(input)
}

#[derive(Debug)]
struct Tree {
    statement: Statement,
    children: Vec<Tree>,
}

fn collect_mixins(tree: &Tree, mixins: &mut HashMap<String, Vec<StyleDeclaration>>) {
    for child in &tree.children {
        if let Statement::MixinDefinition(name) = &child.statement {
            let decls = child
                .children
                .iter()
                .filter_map(|c| match &c.statement {
                    Statement::StyleDeclaration(d) => Some(d.clone()),
                    _ => None,
                })
                .collect();
            mixins.insert(name.clone(), decls);
        }
        collect_mixins(child, mixins);
    }
}

fn selector_tree_to_style_rules(
    tree: &Tree,
    priority: f64,
    mixins: &HashMap<String, Vec<StyleDeclaration>>,
) -> StyleRule {
    let selector = match &tree.statement {
        Statement::Selector(sel) => sel.clone(),
        _ => panic!("Expected selector statement"),
    };

    let mut children_iter = tree.children.iter();

    let mut main_style_rule = StyleRule {
        priority,
        selector: selector.clone(),
        declarations: Vec::new(),
        children: Vec::new(),
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
                    main_style_rule
                        .children
                        .push(selector_tree_to_style_rules(v, *child_priority, mixins));
                } else {
                    panic!("Expected selector after priority attribute");
                }
            }
            Statement::Selector(_) => {
                main_style_rule
                    .children
                    .push(selector_tree_to_style_rules(child, 0.0, mixins));
            }
            Statement::Include(name) => {
                let decls = mixins
                    .get(name)
                    .unwrap_or_else(|| panic!("Unknown mixin: {name}"));
                main_style_rule.declarations.extend(decls.clone());
            }
            Statement::MixinDefinition(_) => {}
            Statement::Root => {
                unreachable!()
            }
        }
    }

    main_style_rule
}

fn root_tree_to_style_rules(
    tree: &Tree,
    mixins: &HashMap<String, Vec<StyleDeclaration>>,
) -> Vec<StyleRule> {
    let mut style_rules = Vec::new();

    let mut children_iter = tree.children.iter();
    while let Some(child) = children_iter.next() {
        match &child.statement {
            Statement::PriorityAttribute(priority) => {
                let v = children_iter
                    .next()
                    .expect("Expected selector after priority attribute");
                if let Statement::Selector(_) = v.statement {
                    style_rules.push(selector_tree_to_style_rules(v, *priority, mixins));
                } else {
                    panic!("Expected selector after priority attribute");
                }
            }
            Statement::Selector(_) => {
                style_rules.push(selector_tree_to_style_rules(child, 0.0, mixins));
            }
            Statement::StyleDeclaration(_) => {
                panic!("Style declarations must be under selectors");
            }
            Statement::Include(_) => {
                panic!("@include must be used within a selector");
            }
            Statement::MixinDefinition(_) => {}
            Statement::Root => {
                style_rules.append(&mut root_tree_to_style_rules(child, mixins));
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
        Selector::Parent(inner) => {
            codegen_selector(inner, &mut *writer);
        }
        Selector::Empty => {}
        Selector::Compound(selectors) => {
            for selector in selectors {
                codegen_selector(selector, &mut *writer);
            }
        }
        Selector::PseudoClass(selector, name) => {
            codegen_selector(selector, &mut *writer);
            write!(writer, ":{name}").unwrap();
        }
        Selector::PseudoInstance(selector, name) => {
            codegen_selector(selector, &mut *writer);
            write!(writer, "::{name}").unwrap();
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
                Combinator::Descendant => write!(writer, " >> ").unwrap(), // Use >> not space
            }
            codegen_selector(right, &mut *writer);
        }
    }
}

fn codegen_style_rule<W: Write>(
    ctx: &mut Context,
    style_rule: &StyleRule,
    parent: String,
    writer: &mut W,
) {
    let mut name = None;
    if matches!(style_rule.selector, Selector::PseudoClass(ref left, ref right) if **left == Selector::Empty && right == "root")
    {
        write!(writer, "\n").unwrap();
        for declaration in &style_rule.declarations {
            if !declaration.property.starts_with("--") {
                panic!("Expected variable in :root");
            }
            write!(
                writer,
                "style_sheet:SetAttribute(\"{property}\", {value})\n",
                property = &declaration.property[2..],
                value = declaration.value
            )
            .unwrap();
        }
    } else if !style_rule.declarations.is_empty() || !style_rule.children.is_empty() {
        name.get_or_insert_with(|| ctx.next_name());
        let name = name.as_ref().unwrap();
        write!(writer, "\nlocal {name} = Instance.new \"StyleRule\"\n").unwrap();
        write!(writer, "{name}.Parent = {parent}\n").unwrap();
        write!(writer, "{name}.Selector = \"").unwrap();
        codegen_selector(&style_rule.selector, &mut *writer);
        write!(writer, "\"\n").unwrap();
        if style_rule.priority != 0.0 {
            write!(
                writer,
                "{name}.Priority = {priority}\n",
                priority = style_rule.priority
            )
            .unwrap();
        }
        let mut prop_decls = Vec::new();
        let mut var_decls = Vec::new();
        let mut transitions = Vec::new();
        for declaration in &style_rule.declarations {
            if declaration.property == "Transition" {
                transitions.extend(parse_transition_value(&declaration.value));
            } else if declaration.property.starts_with("--") {
                var_decls.push(declaration.clone());
            } else {
                prop_decls.push(declaration.clone());
            }
        }
        for var_decl in var_decls {
            write!(
                writer,
                "{name}:SetAttribute(\"{property}\", {value})\n",
                property = &var_decl.property[2..],
                value = var_decl.value
            )
            .unwrap();
        }
        if !prop_decls.is_empty() {
            write!(writer, "{name}:SetProperties{{\n").unwrap();
            for (num, declaration) in prop_decls.iter().enumerate() {
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
        if !transitions.is_empty() {
            write!(writer, "{name}:SetPropertyTransition({{\n").unwrap();
            for transition in &transitions {
                write!(
                    writer,
                    "\t[\"{property}\"] = TweenInfo.new({duration}, Enum.EasingStyle.{style}, Enum.EasingDirection.{direction}),\n",
                    property = transition.property,
                    duration = transition.duration,
                    style = transition.easing_style,
                    direction = transition.easing_direction,
                )
                .unwrap();
            }
            write!(writer, "}})\n").unwrap();
        }
    }

    if style_rule.children.is_empty() {
        return;
    }
    name.get_or_insert_with(|| ctx.next_name());
    let name = name.unwrap();
    for child in &style_rule.children {
        codegen_style_rule(ctx, child, name.clone(), writer);
    }
}

struct PropertyTransition {
    property: String,
    duration: f64,
    easing_style: String,
    easing_direction: String,
}

fn parse_transition_value(value: &str) -> Vec<PropertyTransition> {
    value
        .split(',')
        .map(|entry| {
            let mut tokens = entry.split_whitespace();
            let property = tokens
                .next()
                .expect("Transition entry must have a property")
                .to_string();

            let mut duration = None;
            let mut easing_style = None;
            let mut easing_direction = None;

            for token in tokens {
                if let Some(ms) = token.strip_suffix("ms") {
                    if let Ok(v) = ms.parse::<f64>() {
                        duration = Some(v / 1000.0);
                        continue;
                    }
                }
                if let Some(s) = token.strip_suffix("s") {
                    if let Ok(v) = s.parse::<f64>() {
                        duration = Some(v);
                        continue;
                    }
                }
                if matches!(
                    token,
                    "Linear"
                        | "Sine"
                        | "Back"
                        | "Quad"
                        | "Quart"
                        | "Quint"
                        | "Bounce"
                        | "Elastic"
                        | "Exponential"
                        | "Circular"
                        | "Cubic"
                ) {
                    easing_style = Some(token.to_string());
                    continue;
                }
                match token {
                    "EaseIn" => {
                        easing_direction = Some("In");
                        continue;
                    }
                    "EaseOut" => {
                        easing_direction = Some("Out");
                        continue;
                    }
                    "EaseInOut" => {
                        easing_direction = Some("InOut");
                        continue;
                    }
                    _ => (),
                }
                easing_style = Some(token.to_string());
            }

            PropertyTransition {
                property,
                duration: duration.unwrap_or_else(|| 1.0),
                easing_style: easing_style.unwrap_or_else(|| "Quad".to_string()),
                easing_direction: easing_direction.unwrap_or_else(|| "Out").to_string(),
            }
        })
        .collect()
}

struct Context {
    name_count: u32,
}
impl Context {
    fn next_name(&mut self) -> String {
        let name = format!("style_rule_{}", self.name_count);
        self.name_count += 1;
        name
    }
}
fn codegen<W: Write>(writer: &mut W, rules: Vec<StyleRule>) {
    let mut ctx = Context { name_count: 0 };
    let name = "style_sheet".to_owned();
    write!(writer, "local {name} = Instance.new \"StyleSheet\"\n").unwrap();
    for rule in rules {
        codegen_style_rule(&mut ctx, &rule, name.clone(), writer);
    }
    write!(writer, "\nreturn {name}").unwrap();
}

fn parse_to_tree(file: &str, file_path: &PathBuf, stack: &mut Vec<(usize, Tree)>, root: &mut Tree) {
    let mut indent_style = EnforcedIndentStyle::Unknown;
    for (line_num, line) in file.lines().enumerate() {
        if is_blank(line) {
            continue;
        }
        match indent_level(line, indent_style) {
            Ok((level, new_indent_style)) => {
                indent_style = new_indent_style;
                if comment(line.trim()).is_ok() {
                    continue;
                };
                let (junk, stmt) = statement(line.trim()).expect(
                    format!(
                        "Failed to parse statement {}:{}",
                        file_path.to_string_lossy(),
                        line_num + 1
                    )
                    .as_str(),
                );
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

fn main() {
    let args = Args::parse();

    let mut stack: Vec<(usize, Tree)> = Vec::new();
    let mut root: Tree = Tree {
        statement: Statement::Root,
        children: Vec::new(),
    };

    if args.file.is_empty() {
        eprintln!("No files specified");
        return;
    }

    for file_pat in args.file {
        for file in glob::glob(&file_pat).expect("Failed to read glob") {
            let file_path = file.unwrap();
            let file = std::fs::read_to_string(file_path.clone()).expect("Failed to read file");
            parse_to_tree(&file, &file_path, &mut stack, &mut root);
        }
    }
    let mut mixins = HashMap::new();
    collect_mixins(&root, &mut mixins);
    let style_rules = root_tree_to_style_rules(&root, &mixins);
    let mut file = std::fs::File::create(args.out.unwrap_or("out.lua".to_owned()))
        .expect("Failed to create output file");
    codegen(&mut file, style_rules);
}

#[test]
fn test() {
    let inputs = [
        r#"@priority(1)
.btn >> .text::UICorner
"#,
        r#".btn.active"#,
        r#".btn > .text"#,
        r#".btn::UICorner
    Text: "Hello" 
"#,
        include_str!("../example.lass"),
    ];
    for input in inputs {
        parse_to_tree(
            input,
            &PathBuf::from("test_input"),
            &mut Vec::new(),
            &mut Tree {
                statement: Statement::Root,
                children: Vec::new(),
            },
        );
    }
}
