use adventlib::read_input_lines;
use regex::Regex;
use std::collections::HashMap;
use adventlib::CountMap;
use itertools::Itertools;

extern crate adventlib;
extern crate regex;
extern crate itertools;
#[macro_use] extern crate lazy_static;

#[derive(Debug)]
enum GuardAction {
    BeginShift(u32),
    FallAsleep,
    WakeUp,
}

#[derive(Debug)]
struct Time {
    month: u8,
    day: u8,
    minute: u8
}

#[derive(Debug)]
struct GuardEvent(Time, GuardAction);

fn parse_guard_event(s: &str) -> Option<GuardEvent> {
    lazy_static! {
        static ref MAIN_REGEX: Regex = regex::Regex::new(r"\[\d+-(\d+)-(\d+) \d+:(\d+)\] (.+)").unwrap();
        static ref SHIFT_REGEX: Regex = regex::Regex::new(r"Guard #(\d+).*").unwrap();
    }
    let captures = MAIN_REGEX.captures(s)?;

    let month: u8 = captures.get(1)?.as_str().parse().ok()?;
    let day: u8 = captures.get(2)?.as_str().parse().ok()?;
    let minute: u8 = captures.get(3)?.as_str().parse().ok()?;

    let time = Time {month, day, minute};

    let action = captures.get(4)?.as_str();

    return match action {
        "wakes up" => Some(GuardEvent(time, GuardAction::WakeUp)),
        "falls asleep" => Some(GuardEvent(time, GuardAction::FallAsleep)),
        shift_begin => {
            let captures = SHIFT_REGEX.captures(shift_begin)?;
            let guard_id: u32 = captures.get(1)?.as_str().parse().ok()?;
            Some(GuardEvent(time, GuardAction::BeginShift(guard_id)))
        }
    }
}

#[derive(Debug)]
struct Day {
    date: (u8, u8),
    guard_on_duty: u32,
    asleep: Vec<(u8, u8)>
}

fn build_days(events: &[GuardEvent]) -> Vec<Day> {
    let mut days = Vec::new();
    let mut last_fell_asleep = None;

    let mut current_day = None;
    for event in events {
        let GuardEvent(time, action) = event;
        match action {
            GuardAction::BeginShift(guard_id) => {
                last_fell_asleep = None;
                if let Some(day) = current_day {
                    days.push(day);
                }
                current_day = Some(Day {
                    date: (time.month, time.day),
                    guard_on_duty: *guard_id,
                    asleep: Vec::new()
                })
            },
            GuardAction::FallAsleep => {
                assert!(last_fell_asleep.is_none(), "We fell asleep twice!");
                last_fell_asleep = Some(time);
            },
            GuardAction::WakeUp => {
                current_day.as_mut().unwrap().asleep.push((last_fell_asleep.expect("We woke up without falling asleep").minute, time.minute));
                last_fell_asleep = None;
            }
        }
    }
    return days;
}

fn minutes_asleep(days: &[Day]) -> CountMap<u32> {
    let mut counts = CountMap::new();
    for day in days {
        for (sleep, wake) in day.asleep.iter().cloned() {
            let sleep_time: u32 = wake as u32 - sleep as u32;
            counts.add(day.guard_on_duty, sleep_time as usize);
        }
    }
    return counts;
}

fn guard_minutes(days: &[Day], guard_id: u32) -> CountMap<u32> {
    let mut minutes: CountMap<u32> = CountMap::new();
    for i in 0..60 {
        minutes.add(i, 0);
    }
    for day in days.iter().filter(|day| day.guard_on_duty == guard_id) {
        for (sleep, wake) in day.asleep.iter().cloned() {
            for minute in sleep..wake {
                minutes.increment(minute.into());
            }
        }
    }
    return minutes;
}

fn main() {
    let mut input_lines = read_input_lines("input.txt");
    input_lines.sort();

    let guard_events = input_lines.iter()
        .map(|s| parse_guard_event(s))
        .collect::<Option<Vec<GuardEvent>>>()
        .expect("Failed to parse guard events!");

    let days = build_days(&guard_events);

    let asleep = minutes_asleep(&days);
    let (most_asleep, _) = asleep.greatest().next().expect("No guard ever slept");
    let minutes = guard_minutes(&days, *most_asleep);

    let (least_minute, _) = minutes.greatest().next().expect("This guard never slept");

    println!("Guard {} is most asleep at minute {} = {}", most_asleep, least_minute, most_asleep * least_minute);

    let guards: Vec<u32> = days.iter().map(|day| day.guard_on_duty).unique().collect();
    let mut guard_scores: HashMap<u32, CountMap<u32>> = HashMap::new();
    for guard in guards {
        guard_scores.insert(guard, guard_minutes(&days, guard));
    }

    let (guard, score) = guard_scores.iter()
        .map(|(guard, score)| (*guard, score.greatest().next().unwrap()) )
        .max_by_key(|(guard, (minute, high_score))| *high_score )
        .expect("There was no high score");

    println!("{:?}, {:?}", guard, score);
}
