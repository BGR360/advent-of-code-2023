advent_of_code::solution!(20);

use std::collections::HashMap;
use std::collections::VecDeque;

use advent_of_code::debugln;

mod types {

    use super::*;

    #[derive(Debug, Clone, Default)]
    pub struct Headquarters<'a> {
        modules: HashMap<&'a str, Module<'a>>,
    }

    impl<'a> Headquarters<'a> {
        pub fn add_module(&mut self, module: Module<'a>) {
            self.modules.insert(module.name, module);
        }

        pub fn connect(&mut self, module: &'a str, to_output: &'a str) {
            self.modules
                .get_mut(module)
                .unwrap()
                .register_output(to_output);

            if let Some(output) = self.modules.get_mut(to_output) {
                output.register_input(module);
            }
        }

        /// Returns the total number of (low, high) pulses sent.
        pub fn push_button(&mut self) -> (usize, usize) {
            let mut low_pulses_sent = 0;
            let mut high_pulses_sent = 0;

            let mut pulses_to_send: VecDeque<Pulse<'a>> = VecDeque::new();
            pulses_to_send.push_back(Pulse {
                src: "button",
                dest: "broadcaster",
                high: false,
            });

            while let Some(pulse) = pulses_to_send.pop_front() {
                debugln!(
                    "{} -{}-> {}",
                    pulse.src,
                    if pulse.high { "high" } else { "low" },
                    pulse.dest
                );

                if pulse.high {
                    high_pulses_sent += 1;
                } else {
                    low_pulses_sent += 1;
                }

                if let Some(dest) = self.modules.get_mut(pulse.dest) {
                    dest.process_pulse(&pulse, &mut pulses_to_send);
                }
            }

            (low_pulses_sent, high_pulses_sent)
        }
    }

    #[derive(Debug, Clone)]
    pub struct Module<'a> {
        name: &'a str,
        kind: ModuleKind<'a>,
        outputs: Vec<&'a str>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum ModuleKind<'a> {
        Broadcaster,
        FlipFlop {
            state: bool,
        },
        Conjuction {
            input_states: HashMap<&'a str, bool>,
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
                    input_states: HashMap::new(),
                },
            )
        }

        fn new(name: &'a str, kind: ModuleKind<'a>) -> Self {
            Self {
                name,
                kind,
                outputs: Vec::new(),
            }
        }

        pub fn name(&self) -> &'a str {
            self.name
        }

        pub fn register_input(&mut self, input: &'a str) {
            match &mut self.kind {
                ModuleKind::Broadcaster | ModuleKind::FlipFlop { .. } => {}
                ModuleKind::Conjuction { input_states } => {
                    input_states.insert(input, false);
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
                    *input_states.get_mut(pulse.src).unwrap() = pulse.high;

                    // Then, if it remembers high pulses for all inputs, it
                    // sends a low pulse; otherwise, it sends a high pulse.
                    let high = !input_states.values().all(|&high| high);

                    self.send_pulse(output_pulses, high);
                }
            }
        }

        fn send_pulse(&self, pulses: &mut VecDeque<Pulse<'a>>, high: bool) {
            for dest in &self.outputs {
                pulses.push_back(Pulse {
                    src: self.name,
                    dest,
                    high,
                });
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Pulse<'a> {
        pub src: &'a str,
        pub dest: &'a str,
        pub high: bool,
    }
}
use types::*;

pub fn part_one(input: &str) -> Option<usize> {
    let mut hq = parsing::parse_input(input);

    debugln!("{hq:#?}");

    let mut low_pulses_sent = 0;
    let mut high_pulses_sent = 0;

    for _ in 0..1000 {
        let (low, high) = hq.push_button();
        low_pulses_sent += low;
        high_pulses_sent += high;
    }

    Some(low_pulses_sent * high_pulses_sent)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
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
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(32000000));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
