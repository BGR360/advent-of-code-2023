advent_of_code::solution!(20);

use itertools::Itertools;
use std::collections::VecDeque;

use advent_of_code::debugln;

mod types {

    use std::fmt;

    use aoc_helpers::fmt::FmtExt;

    use super::*;

    #[derive(Debug, Clone, Default, PartialEq)]
    pub struct Headquarters<'a> {
        // modules: HashMap<&'a str, Module<'a>>,
        modules: Vec<Module<'a>>,
    }

    impl<'a> Headquarters<'a> {
        pub fn add_module(&mut self, module: Module<'a>) {
            // self.modules.insert(module.name, module);
            self.modules.push(module);
            self.modules.sort_by_key(|module| module.name);
        }

        pub fn connect(&mut self, module: &'a str, to_output: &'a str) {
            self.module_mut(module).unwrap().register_output(to_output);

            if let Some(output) = self.module_mut(to_output) {
                output.register_input(module);
            }
        }

        /// Returns the total number of (low, high) pulses sent.
        pub fn push_button(
            &mut self,
            mut stop_condition: impl FnMut(&Pulse<'_>) -> bool,
        ) -> (usize, usize, bool) {
            let mut low_pulses_sent = 0;
            let mut high_pulses_sent = 0;

            let mut pulses_to_send: VecDeque<Pulse<'a>> = VecDeque::new();
            pulses_to_send.push_back(Pulse {
                src: "button",
                dest: "broadcaster",
                high: false,
            });

            while let Some(pulse) = pulses_to_send.pop_front() {
                debugln!("{pulse}");

                if pulse.high {
                    high_pulses_sent += 1;
                } else {
                    low_pulses_sent += 1;
                }

                if (stop_condition)(&pulse) {
                    return (low_pulses_sent, high_pulses_sent, true);
                }

                if let Some(dest) = self.module_mut(pulse.dest) {
                    dest.process_pulse(&pulse, &mut pulses_to_send);
                }
            }

            (low_pulses_sent, high_pulses_sent, false)
        }

        pub fn module_mut(&mut self, name: &'a str) -> Option<&mut Module<'a>> {
            // self.modules.get_mut(name)
            self.modules.iter_mut().find(|module| module.name == name)
        }

        /// Returns a copy of this headquarters with only the requested modules,
        /// preserving their connections but resetting their states to the default.
        pub fn subset<'b>(&self, modules: impl IntoIterator<Item = &'b str>) -> Self {
            let mut requested_modules = modules.into_iter().collect_vec();
            requested_modules.sort();

            let mut copy = Headquarters::default();

            for module in &self.modules {
                let name = module.name;
                if requested_modules.contains(&name) {
                    match module.kind {
                        ModuleKind::Broadcaster => {
                            copy.add_module(Module::new_broadcaster());
                        }
                        ModuleKind::FlipFlop { .. } => copy.add_module(Module::new_flip_flop(name)),
                        ModuleKind::Conjuction { .. } => {
                            copy.add_module(Module::new_conjunction(name))
                        }
                    }
                }
            }

            for module in &self.modules {
                let name = module.name;
                if requested_modules.contains(&name) {
                    for output_name in &module.outputs {
                        if requested_modules.contains(output_name) {
                            copy.connect(name, output_name);
                        }
                    }
                }
            }

            copy
        }
    }

    impl fmt::Display for Headquarters<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.modules.iter().separated_by('\n'))
        }
    }

    #[derive(PartialEq)]
    pub struct Module<'a> {
        name: &'a str,
        kind: ModuleKind<'a>,
        outputs: Vec<&'a str>,
        // recv_callback: Option<Box<dyn Fn(&Self, &Pulse<'a>) + 'a>>,
        // send_callback: Option<Box<dyn Fn(&Self, &Pulse<'a>) + 'a>>,
    }

    impl fmt::Debug for Module<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Module")
                .field("name", &self.name)
                .field("kind", &self.kind)
                .field("outputs", &self.outputs)
                .finish()
        }
    }

    impl Clone for Module<'_> {
        fn clone(&self) -> Self {
            Self {
                name: self.name,
                kind: self.kind.clone(),
                outputs: self.outputs.clone(),
                // recv_callback: None,
                // send_callback: None,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum ModuleKind<'a> {
        Broadcaster,
        FlipFlop {
            state: bool,
        },
        Conjuction {
            // input_states: HashMap<&'a str, bool>,
            input_states: Vec<(&'a str, bool)>,
        },
    }

    impl<'a> Module<'a> {
        pub fn new_broadcaster() -> Self {
            Self::new("broadcaster", ModuleKind::Broadcaster)
        }

        pub fn new_flip_flop(name: &'a str) -> Self {
            Self::new(name, ModuleKind::FlipFlop { state: false })
        }

        pub fn new_conjunction(name: &'a str) -> Self {
            Self::new(
                name,
                ModuleKind::Conjuction {
                    // input_states: HashMap::new(),
                    input_states: Vec::new(),
                },
            )
        }

        fn new(name: &'a str, kind: ModuleKind<'a>) -> Self {
            Self {
                name,
                kind,
                outputs: Vec::new(),
                // send_callback: None,
                // recv_callback: None,
            }
        }

        pub fn name(&self) -> &'a str {
            self.name
        }

        pub fn register_input(&mut self, input: &'a str) {
            match &mut self.kind {
                ModuleKind::Broadcaster | ModuleKind::FlipFlop { .. } => {}
                ModuleKind::Conjuction { input_states } => {
                    // input_states.insert(input, false);
                    input_states.push((input, false));
                    input_states.sort_by_key(|(name, _)| *name);
                }
            }
        }

        pub fn register_output(&mut self, output: &'a str) {
            self.outputs.push(output);
        }

        pub fn process_pulse(&mut self, pulse: &Pulse, output_pulses: &mut VecDeque<Pulse<'a>>) {
            match &mut self.kind {
                ModuleKind::Broadcaster => {
                    // When it receives a pulse, it sends the same pulse to all
                    // of its destination modules.

                    self.send_pulse(output_pulses, pulse.high);
                }

                ModuleKind::FlipFlop { state } => {
                    // If a flip-flop module receives a high pulse, it is
                    // ignored and nothing happens.
                    if !pulse.high {
                        // However, if a flip-flop module receives a low pulse,
                        // it flips between on and off. If it was off, it turns
                        // on and sends a high pulse. If it was on, it turns off
                        // and sends a low pulse.
                        let new_state = !*state;

                        *state = new_state;

                        self.send_pulse(output_pulses, new_state);
                    }
                }
                ModuleKind::Conjuction { input_states } => {
                    // When a pulse is received, the conjunction module first
                    // updates its memory for that input.
                    input_states
                        .iter_mut()
                        .find(|(name, _)| *name == pulse.src)
                        .unwrap()
                        .1 = pulse.high;

                    // Then, if it remembers high pulses for all inputs, it
                    // sends a low pulse; otherwise, it sends a high pulse.
                    let high = !input_states.iter().all(|(_, high)| *high);

                    self.send_pulse(output_pulses, high);
                }
            }
        }

        fn send_pulse(&self, pulses: &mut VecDeque<Pulse<'a>>, high: bool) {
            for dest in &self.outputs {
                let pulse = Pulse {
                    src: self.name,
                    dest,
                    high,
                };

                // if let Some(callback) = &self.send_callback {
                //     (callback)(self, &pulse);
                // }

                pulses.push_back(pulse);
            }
        }

        // pub fn add_send_callback(&mut self, callback: impl Fn(&Self, &Pulse<'a>) + 'a) {
        //     self.send_callback = Some(Box::new(callback))
        // }

        // pub fn add_recv_callback(&mut self, callback: impl Fn(&Self, &Pulse<'a>) + 'a) {
        //     self.recv_callback = Some(Box::new(callback))
        // }
    }

    impl fmt::Display for Module<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}: ", self.name)?;
            match &self.kind {
                ModuleKind::Broadcaster => {}
                ModuleKind::FlipFlop { state } => write!(f, "{}", *state as u8)?,
                ModuleKind::Conjuction { input_states } => {
                    write!(
                        f,
                        "{}",
                        input_states
                            .iter()
                            .map(|s| s.with_fmt(|f, (name, state)| {
                                write!(f, "{name}={}", *state as u8)
                            }))
                            .separated_by(',')
                    )?;
                }
            };
            Ok(())
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Pulse<'a> {
        pub src: &'a str,
        pub dest: &'a str,
        pub high: bool,
    }

    impl fmt::Display for Pulse<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{} -{}-> {}",
                self.src,
                if self.high { "high" } else { "low" },
                self.dest
            )
        }
    }
}
use types::*;

pub fn part_one(input: &str) -> Option<usize> {
    let mut hq = parsing::parse_input(input);

    debugln!("{hq:#?}");

    let mut low_pulses_sent = 0;
    let mut high_pulses_sent = 0;

    for _ in 0..1000 {
        let (low, high, _stopped) = hq.push_button(|_| false);
        low_pulses_sent += low;
        high_pulses_sent += high;
    }

    Some(low_pulses_sent * high_pulses_sent)
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Duration {
    button_pushes: usize,
    pulses: usize,
}

impl std::ops::Sub for Duration {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Duration {
            button_pushes: self.button_pushes - rhs.button_pushes,
            pulses: self.pulses - rhs.pulses,
        }
    }
}

#[cfg(test)]
fn part_two_slow(mut hq: Headquarters) -> usize {
    debugln!("{hq:#?}");

    const PRINT_FREQ: usize = 10_000_000;

    let mut now = Duration::default();
    let mut next_print = PRINT_FREQ;

    loop {
        now.button_pushes += 1;

        let (_low, _high, stopped) = hq.push_button(|pulse| {
            now.pulses += 1;
            // if !pulse.high && matches!(pulse.src, "cq" | "vp" | "rv" | "dc") {
            //     println!("{} low after {pulses_sent}", pulse.src);
            // }
            if pulse.high && pulse.dest == "ns" {
                println!("@{}: {pulse:?}", now.pulses);
            }

            pulse.dest == "rx" && !pulse.high
        });

        if now.pulses >= next_print {
            println!("{}", now.pulses);
            next_print = now.pulses + PRINT_FREQ;
        }

        if stopped {
            break;
        }
    }

    now.button_pushes
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Cycle {
    lead_up: Duration,
    period: Duration,
}

/// Returns the cadence at which the given headquarters sends low pulses to "rx".
fn find_cycle(mut hq: Headquarters<'_>, to: &Pulse<'_>) -> Cycle {
    let mut now = Duration {
        button_pushes: 0,
        pulses: 0,
    };

    // None until the first time a low pulse is sent to "rx".
    let mut lead_up: Option<Duration> = None;
    // None until the second time a low pulse is sent to "rx".
    let mut period: Option<Duration> = None;

    loop {
        debugln!();
        debugln!("==============================");

        debugln!("{hq}");
        debugln!();

        now.button_pushes += 1;

        let (_low, _high, stopped) = hq.push_button(|pulse| {
            now.pulses += 1;

            if pulse.dest == to.dest && pulse.high == to.high {
                println!();
                println!("**********************************************");
                println!("**********************************************");
                println!("{pulse}");
                println!("after {} button pushes", now.button_pushes);
                println!("after {} pulses", now.pulses);
                println!("**********************************************");
                println!("**********************************************");

                if let Some(lead_up) = lead_up {
                    if let Some(period) = period {
                        // Third+ time we saw a low pulse.
                        // Assert the period is the same.
                        let test_period = now - period - lead_up;

                        assert_eq!(test_period, period);

                        // Done now!
                        return true;
                    } else {
                        // Second time we saw a low pulse.
                        // Record the period.
                        period = Some(now - lead_up);
                    }
                } else {
                    // First time we saw a low pulse.
                    // Record the lead-up.
                    lead_up = Some(now);
                }
            }

            false
        });

        debugln!();
        debugln!("NOW: {now:#?}");

        if stopped {
            break;
        }
    }

    Cycle {
        lead_up: lead_up.unwrap(),
        period: period.unwrap(),
    }
}

fn least_common_multiple<I: num::Integer>(numbers: impl IntoIterator<Item = I>) -> I {
    numbers
        .into_iter()
        .fold(I::one(), |accum, n| num::integer::lcm(accum, n))
}

fn part_two_fast<'a>(hq: Headquarters<'a>, subsets: Vec<Vec<&'a str>>) -> usize {
    // Break the original hq into subsets.
    let subsets: Vec<Headquarters<'a>> = subsets
        .into_iter()
        .map(|modules| hq.subset(modules))
        .collect();

    // Find the cadence at which each independent subset sends high pulses to TX
    let cycles: Vec<Cycle> = subsets
        .iter()
        .map(|subset| {
            find_cycle(
                subset.clone(),
                &Pulse {
                    src: "*",
                    dest: "TX",
                    high: true,
                },
            )
        })
        .collect();

    debugln!("\nCYCLES: {cycles:#?}");

    // Find the least common multiple of those cycles. That is how many button
    // pushes it will take for "rx" to receive a low pulse.
    let total_button_pushes =
        least_common_multiple(cycles.iter().map(|cycle| cycle.period.button_pushes));

    debugln!("\nFINAL BUTTON PUSHES: {total_button_pushes}");

    total_button_pushes
}

pub fn part_two(input: &str) -> Option<usize> {
    let hq = parsing::parse_input(input);

    let subsets = vec![
        vec![
            "broadcaster",
            "AA",
            "AB",
            "AC",
            "AD",
            "AE",
            "AF",
            "AG",
            "AH",
            "AI",
            "AJ",
            "AK",
            "AL",
            "AX",
            "AZ",
            "TX",
            "rx",
        ],
        vec![
            "broadcaster",
            "BA",
            "BB",
            "BC",
            "BD",
            "BE",
            "BF",
            "BG",
            "BH",
            "BI",
            "BJ",
            "BK",
            "BL",
            "BX",
            "BZ",
            "TX",
            "rx",
        ],
        vec![
            "broadcaster",
            "CA",
            "CB",
            "CC",
            "CD",
            "CE",
            "CF",
            "CG",
            "CH",
            "CI",
            "CJ",
            "CK",
            "CL",
            "CX",
            "CZ",
            "TX",
            "rx",
        ],
        vec![
            "broadcaster",
            "DA",
            "DB",
            "DC",
            "DD",
            "DE",
            "DF",
            "DG",
            "DH",
            "DI",
            "DJ",
            "DK",
            "DL",
            "DX",
            "DZ",
            "TX",
            "rx",
        ],
    ];
    Some(part_two_fast(hq, subsets))
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn module(input: &str) -> IResult<&str, Module<'_>> {
        alt((
            tag("broadcaster").map(|_| Module::new_broadcaster()),
            preceded(char('%'), word).map(Module::new_flip_flop),
            preceded(char('&'), word).map(Module::new_conjunction),
        ))
        .parse(input)
    }

    fn outputs(input: &str) -> IResult<&str, Vec<&str>> {
        separated_list1(tag(", "), word).parse(input)
    }

    fn line(input: &str) -> IResult<&str, (Module<'_>, Vec<&str>)> {
        separated_pair(module, tag(" -> "), outputs).parse(input)
    }

    pub fn parse_input(input: &str) -> Headquarters<'_> {
        let modules_and_outputs =
            final_parser(line_separated(line))(input).expect("input should be valid");

        let mut hq = Headquarters::default();

        for (module, _outputs) in modules_and_outputs.iter() {
            hq.add_module(module.clone());
        }

        for (module, outputs) in modules_and_outputs.iter() {
            let name = module.name();
            for output in outputs {
                hq.connect(name, output);
            }
        }

        hq
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use aoc_helpers::fmt::FmtExt;

    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(32000000));
    }

    #[cfg(no)]
    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    struct ButtonPush {
        pub pulses: usize,
        pub rx_pulse: bool,
    }

    fn do_test(input: &str, num_rx_pulses: usize) -> Vec<ButtonPush> {
        let mut hq = parsing::parse_input(input);

        let mut button_pushes = Vec::new();
        let mut pulses_sent = 0;
        let mut rx_pulses = 0;
        let mut pulses_before_rx_pulse = None;

        loop {
            debugln!();
            debugln!("==============================");

            debugln!("{hq}");
            debugln!();

            let mut pulses_this_push = 0;

            let (_low, _high, stopped) = hq.push_button(|pulse| {
                pulses_sent += 1;
                pulses_this_push += 1;

                // if !pulse.high && pulse.dest == "rx" {
                //     println!("@{pulses_sent}: {pulse:?}");
                // }

                if pulse.dest == "rx" && !pulse.high {
                    pulses_before_rx_pulse = Some((pulses_this_push, pulses_sent));
                    rx_pulses += 1;
                }

                rx_pulses >= num_rx_pulses
            });

            button_pushes.push(ButtonPush {
                pulses: pulses_this_push,
                rx_pulse: pulses_before_rx_pulse.is_some(),
            });

            debugln!();
            debugln!("Button pushes:\t{}", button_pushes.len());
            debugln!("Total pulses:\t{pulses_sent}");
            debugln!("Pulses this push:\t{}", pulses_this_push);

            if let Some((pulses_this_push, pulses_total)) = pulses_before_rx_pulse {
                println!();
                println!("**********************************************");
                println!("**********************************************");
                println!("rx recevied low pulse:");
                println!("on {}th button push", button_pushes.len());
                println!("after {pulses_total} pulses ({pulses_this_push} pulses during press)");
                println!("**********************************************");
                println!("**********************************************");

                pulses_before_rx_pulse = None;
            }

            if stopped {
                break;
            }
        }

        println!();
        println!(
            "Button pushes: {}",
            button_pushes
                .iter()
                .map(|bp| bp.with_fmt(|f, bp| {
                    if bp.rx_pulse {
                        write!(f, "*{}*", bp.pulses)
                    } else {
                        write!(f, "{}", bp.pulses)
                    }
                }))
                .separated_by(", ")
        );
        // for (idx, bp) in button_pushes
        //     .iter()
        //     .enumerate()
        //     .filter(|(_, bp)| bp.rx_pulse)
        // {}

        button_pushes
    }

    fn assert_all_eq<I>(all: I)
    where
        I: IntoIterator,
        I::Item: PartialEq + Clone + fmt::Debug,
    {
        let all = all.into_iter().collect_vec();
        let all_eq = all.iter().tuple_windows().all(|(a, b)| a == b);
        assert!(all_eq, "Not all elements are equal:\n{all:#?}");
    }

    #[test]
    fn test_example_a() {
        let a1 = do_test(
            &advent_of_code::template::read_file_part("examples", DAY, 11),
            2,
        );
        let a2 = do_test(
            &advent_of_code::template::read_file_part("examples", DAY, 12),
            2,
        );
        let a3 = do_test(
            &advent_of_code::template::read_file_part("examples", DAY, 13),
            2,
        );
        let a4 = do_test(
            &advent_of_code::template::read_file_part("examples", DAY, 14),
            2,
        );

        assert_all_eq([a1, a2, a3, a4]);
    }

    #[test]
    fn test_example_b() {
        let b1 = do_test(
            &advent_of_code::template::read_file_part("examples", DAY, 21),
            2,
        );
    }

    #[test]
    fn test_example_cycle_b() {
        let input = advent_of_code::template::read_file_part("examples", DAY, 21);
        let hq = parsing::parse_input(&input);
        let cycle = find_cycle(
            hq,
            &Pulse {
                src: "*",
                dest: "rx",
                high: false,
            },
        );

        debugln!();
        debugln!("CYCLE: {cycle:#?}");
    }

    /*
    CYCLE: Cycle {
        lead_up: Duration {
            button_pushes: 21,
            pulses: 268,
        },
        period: Duration {
            button_pushes: 21,
            pulses: 296,
        },
    }
     */
    #[test]
    fn test_example_c_cycle_a() {
        let input = advent_of_code::template::read_file_part("examples", DAY, 31);
        let hq = parsing::parse_input(&input);
        let cycle = find_cycle(
            hq,
            &Pulse {
                src: "*",
                dest: "TX",
                high: true,
            },
        );

        debugln!();
        debugln!("CYCLE: {cycle:#?}");
    }

    /*
    ANSWER: Duration {
        button_pushes: 21,
        pulses: 273,
    }
     */
    #[test]
    fn test_example_c_slow_a() {
        let input = advent_of_code::template::read_file_part("examples", DAY, 31);
        let hq = parsing::parse_input(&input);

        let pulses = part_two_slow(hq);
        debugln!();
        debugln!("ANSWER: {pulses:#?}");
    }

    /*
    CYCLE: Cycle {
        lead_up: Duration {
            button_pushes: 23,
            pulses: 327,
        },
        period: Duration {
            button_pushes: 23,
            pulses: 356,
        },
    }
     */
    #[test]
    fn test_example_c_cycle_b() {
        let input = advent_of_code::template::read_file_part("examples", DAY, 32);
        let hq = parsing::parse_input(&input);
        let cycle = find_cycle(
            hq,
            &Pulse {
                src: "*",
                dest: "TX",
                high: true,
            },
        );

        debugln!();
        debugln!("CYCLE: {cycle:#?}");
    }

    /*
    ANSWER: Duration {
        button_pushes: 23,
        pulses: 330,
    }
     */
    #[test]
    fn test_example_c_slow_b() {
        let input = advent_of_code::template::read_file_part("examples", DAY, 32);
        let hq = parsing::parse_input(&input);

        let pulses = part_two_slow(hq);
        debugln!();
        debugln!("ANSWER: {pulses:#?}");
    }

    #[test]
    fn test_example_c_subset() {
        let a_input = advent_of_code::template::read_file_part("examples", DAY, 31);
        let b_input = advent_of_code::template::read_file_part("examples", DAY, 32);
        let ab_input = advent_of_code::template::read_file_part("examples", DAY, 33);

        let a = parsing::parse_input(&a_input);
        let b = parsing::parse_input(&b_input);
        let ab = parsing::parse_input(&ab_input);

        let a_subset = ab.subset([
            "broadcaster",
            "AA",
            "AB",
            "AC",
            "AD",
            "AL",
            "AX",
            "AZ",
            "TX",
            "rx",
        ]);
        let b_subset = ab.subset([
            "broadcaster",
            "BA",
            "BB",
            "BC",
            "BD",
            "BL",
            "BX",
            "BZ",
            "TX",
            "rx",
        ]);

        assert_eq!(a_subset, a);
        assert_eq!(b_subset, b);
    }

    /*
    ANSWER: Duration {
        button_pushes: 483,
        pulses: 13757,
    }

    483 / 21 = 23
        - 23 * 295 = 6785
        * 23 * 296 = 6808
        - 23 * 297 = 6831


    483 / 23 = 21
        * 21 * 356

    */
    #[test]
    fn test_slow_and_fast() {
        let input = advent_of_code::template::read_file_part("examples", DAY, 33);
        let hq = parsing::parse_input(&input);

        let slow = part_two_slow(hq.clone());

        println!("ANSWER: {slow:#?}");

        let subsets = vec![
            vec![
                "broadcaster",
                "AA",
                "AB",
                "AC",
                "AD",
                "AL",
                "AX",
                "AZ",
                "TX",
                "rx",
            ],
            vec![
                "broadcaster",
                "BA",
                "BB",
                "BC",
                "BD",
                "BL",
                "BX",
                "BZ",
                "TX",
                "rx",
            ],
        ];

        let fast = part_two_fast(hq, subsets);

        assert_eq!(fast, slow);
    }
}
