use std::cell::RefCell;

use chrono::NaiveDate;
use comrak::nodes::{AstNode, NodeValue};
use nom::{
    IResult, Parser,
    bytes::{
        complete::{tag, take},
        take_until1,
    },
    character::complete::{multispace0, multispace1, u32},
    combinator::{map, map_res, opt},
    multi::{many1, separated_list0},
    sequence::pair,
};
use tracing::warn;

use super::Todo;
use crate::notes::{
    Tag,
    note::{
        Interval, IntervalUnit, StatementBody, Status, TodoBody,
        parse::{common::parse_symbol, tags::parse_tag},
    },
};

pub(super) fn parse_todo<'a>(node: &'a AstNode<'a>) -> Option<Todo<'a>> {
    if let NodeValue::TaskItem(done) = node.data.borrow().value {
        let text = node
            .first_child()
            .map(|c| c.first_child())
            .flatten()
            .and_then(|n| n.data.borrow().value.text().cloned())
            .unwrap_or_default();

        let status = done.map(parse_status).unwrap_or_default();

        let (text, title) = parse_title(&text)
            .inspect_err(|e| warn!(?e, "unable to parse todo title"))
            .ok()?;

        let params = parse_params(text);

        let mut todo_body = TodoBody {
            title,
            status,
            delay: None,
            advance: None,
            every: None,
            since: None,
            adhoc: false,
            statements: None,
            tags: None,
        };

        for param in params.into_iter().flatten() {
            match param {
                Param::Delay(interval) => todo_body.delay = Some(interval),
                Param::Advance(interval) => todo_body.advance = Some(interval),
                Param::Every(interval) => todo_body.every = Some(interval),
                Param::Since(date) => todo_body.since = Some(date),
                Param::Statement(s) => {
                    if todo_body.statements.is_none() {
                        todo_body.statements = Some(Vec::new());
                    }

                    if let Some(statements) = &mut todo_body.statements {
                        statements.push(s);
                    }
                }
                Param::Tags(tags) => {
                    if !tags.is_empty() {
                        todo_body.tags = Some(tags.into_iter().collect());
                    }
                }
                Param::Adhoc => todo_body.adhoc = true,
            }
        }

        Some(Todo {
            node: node.into(),
            removed: Default::default(),
            body: RefCell::new(todo_body),
        })
    } else {
        None
    }
}

fn parse_title(input: &str) -> IResult<&str, String> {
    match map(take_until1(" @"), |s: &str| s.to_owned()).parse(input) {
        Err(nom::Err::Incomplete(_)) => Ok(("", input.to_owned())),
        x => x,
    }
}

fn parse_status(input: char) -> Status {
    match input {
        'x' => Status::Done,
        's' => Status::Skipped,
        'c' => Status::Completed,
        _ => Status::Open,
    }
}

#[derive(Debug)]
enum Param {
    Delay(Interval),
    Advance(Interval),
    Every(Interval),
    Since(NaiveDate),
    Statement(StatementBody),
    Tags(Vec<Tag>),
    Adhoc,
}

fn parse_params(input: &str) -> Option<Vec<Param>> {
    map(
        pair(
            tag(" @"),
            separated_list0(pair(tag(","), multispace0), parse_param),
        ),
        |(_, params)| params,
    )
    .parse(input)
    .map(|(_, params)| Some(params))
    .unwrap_or_default()
}

fn parse_param(input: &str) -> IResult<&str, Param> {
    let mut parser = map(
        (tag("every"), multispace1, parse_interval),
        |(_, _, interval)| Param::Every(interval),
    )
    .or(map(
        (tag("delay"), multispace1, parse_interval),
        |(_, _, interval)| Param::Delay(interval),
    ))
    .or(map(
        (tag("advance"), multispace1, parse_interval),
        |(_, _, interval)| Param::Advance(interval),
    ))
    .or(map(tag("adhoc"), |_| Param::Adhoc))
    .or(map(
        (tag("since"), multispace1, parse_date),
        |(_, _, date)| Param::Since(date),
    ))
    .or(map(
        (tag("tags"), multispace1, many1(parse_tag)),
        |(_, _, tags)| Param::Tags(tags),
    ))
    .or(map(parse_statement, Param::Statement));

    parser.parse(input)
}

fn parse_statement(input: &str) -> IResult<&str, StatementBody> {
    map(
        (
            opt(tag("#")),
            parse_symbol,
            multispace1,
            separated_list0(multispace1, parse_symbol),
        ),
        |(_, name, _, args)| StatementBody {
            name: name.to_owned(),
            args: args.into_iter().map(str::to_owned).collect(),
        },
    )
    .parse(input)
}

fn parse_interval(input: &str) -> IResult<&str, Interval> {
    map(
        pair(opt(pair(u32, multispace1)), parse_unit),
        |(opt_num, unit)| {
            let value = opt_num.map(|(n, _)| n).unwrap_or(1);
            Interval { value, unit }
        },
    )
    .parse(input)
}

fn parse_unit(input: &str) -> IResult<&str, IntervalUnit> {
    map(pair(tag("day"), opt(tag("s"))), |_| IntervalUnit::Day)
        .or(map(pair(tag("month"), opt(tag("s"))), |_| {
            IntervalUnit::Month
        }))
        .or(map(pair(tag("year"), opt(tag("s"))), |_| {
            IntervalUnit::Year
        }))
        .or(map(pair(tag("week"), opt(tag("s"))), |_| {
            IntervalUnit::Week
        }))
        .parse(input)
}

fn parse_date(input: &str) -> IResult<&str, NaiveDate> {
    map_res(take(10usize), |s| NaiveDate::parse_from_str(s, "%Y-%m-%d")).parse(input)
}
