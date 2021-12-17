#![allow(unused)]

use std::error::Error;
use std::fmt;
use std::str::FromStr;

struct Parser<'a> {
    bits: &'a str,
    cursor: usize, // where in the bits we're at with parsing
    state: ParserState,
}

#[derive(Debug, PartialEq)]
enum ParserState {
    ParseVersion,
    ParseTypeID,
    ParseLengthTypeID,
    ParseLength,
    ParseLiteralValue,
    ParseSubPacketsByLength(usize),
    ParseSubPacketsByCount(usize),
    CalculateOperationValue,
    Finished,
}

#[derive(Debug)]
enum Packet {
    Uninitialized,
    Literal(LiteralPacket),
    Operator(OperatorPacket),
}

#[derive(Debug)]
struct LiteralPacket {
    version: usize,
    type_id: usize,
    value: Option<usize>,
    length: Option<usize>, // total bits in this packet
}

#[derive(Debug)]
struct OperatorPacket {
    version: usize,
    type_id: usize,
    length_type_id: Option<usize>,
    length: Option<usize>, // total bits in this packet
    packets: Vec<Box<Packet>>,
}

const LENGTH_TYPE_LENGTH: usize = 0;
const LENGTH_TYPE_COUNT: usize = 1;

const TYPE_ID_LITERAL: usize = 4;

// const DEBUG: bool = true;
const DEBUG: bool = false;

fn packet_length(packet: &Packet) -> usize {
    match packet {
        Packet::Literal(p) => p.length.expect("Packet must have a length"),
        Packet::Operator(p) => p.length.expect("Packet must have a length"),
        Packet::Uninitialized => 0,
    }
}

fn packet_slice<'a>(s: &'a str, cursor: &mut usize, amount: usize) -> &'a str {
    let slice = &s[*cursor..*cursor + amount];

    *cursor += amount;

    slice
}

fn parse_packet<'a>(bits: &'a str) -> Result<Packet, Box<dyn Error>> {
    use ParserState::*;

    if DEBUG {
        println!("Beginning parsing for {}", bits);
    }

    let mut packet = Packet::Uninitialized;
    let mut version = 0;

    let mut parser = Parser {
        bits,
        cursor: 0,
        state: ParseVersion,
    };

    let state = &mut parser.state;
    let cursor = &mut parser.cursor;

    while *state != Finished {
        if DEBUG {
            println!("Main Parser Loop: {:?}", state);
        }

        match state {
            // parse first 3 bits (version)
            ParseVersion => {
                version = bin_to_int(&packet_slice(&parser.bits, cursor, 3))?;
                *state = ParseTypeID;
            }

            // parse second 3 bits (type id) - concretes packet kind
            ParseTypeID => {
                let type_id = bin_to_int(&packet_slice(&parser.bits, cursor, 3))?;

                match type_id {
                    t if t == TYPE_ID_LITERAL => {
                        packet = Packet::Literal(LiteralPacket {
                            version,
                            type_id,
                            value: None,
                            length: None,
                        });

                        *state = ParseLiteralValue;
                    }
                    _ => {
                        packet = Packet::Operator(OperatorPacket {
                            version,
                            type_id,
                            length_type_id: None,
                            length: None,
                            packets: Vec::new(),
                        });

                        *state = ParseLengthTypeID;
                    }
                }
            }

            ParseLengthTypeID => {
                if let Packet::Operator(ref mut o) = packet {
                    o.length_type_id = Some(bin_to_int(&packet_slice(&parser.bits, cursor, 1))?);
                }

                *state = ParseLength;
            }

            ParseLength => {
                if let Packet::Operator(ref mut o) = packet {
                    match o.length_type_id {
                        // next 15 bits are a number that represents the total length in bits contained by this packet
                        Some(l) if l == LENGTH_TYPE_LENGTH => {
                            let length_bit_string = &packet_slice(&parser.bits, cursor, 15);
                            *state = ParseSubPacketsByLength(bin_to_int(&length_bit_string)?);
                        }
                        // next 11 bits are a number that represents the number of sub-packets immediately contained by this packet
                        Some(l) if l == LENGTH_TYPE_COUNT => {
                            let length_bit_string = &packet_slice(&parser.bits, cursor, 11);
                            *state = ParseSubPacketsByCount(bin_to_int(&length_bit_string)?);
                        }

                        _ => unreachable!(),
                    }
                }
            }

            ParseLiteralValue => {
                if let Packet::Literal(ref mut p) = packet {
                    let mut slice;
                    let mut bin_string = String::new();

                    loop {
                        slice = packet_slice(&parser.bits, cursor, 5);
                        bin_string.push_str(&slice.chars().skip(1).collect::<String>());

                        if slice.chars().nth(0) == Some('0') {
                            break;
                        }
                    }

                    p.length = Some(*cursor);
                    p.value = Some(bin_to_int(&bin_string)?);
                }

                *state = Finished;
            }

            ParseSubPacketsByLength(total_length) => {
                if let Packet::Operator(ref mut o) = packet {
                    let mut parsed_length: usize = 0;

                    // parse sub packets
                    while parsed_length < *total_length {
                        let mut remaining_packets: &str = &parser.bits[*cursor..];
                        let parsed_packet = remaining_packets.parse::<Packet>()?;

                        if DEBUG {
                            println!("Parsed Packet: {:?}", parsed_packet);
                        }

                        let packet_len = packet_length(&parsed_packet);
                        parsed_length += packet_len;
                        *cursor += packet_len;
                        o.packets.push(Box::new(parsed_packet));
                    }

                    o.length = Some(*cursor);
                }

                *state = Finished;
            }

            ParseSubPacketsByCount(total_count) => {
                if let Packet::Operator(ref mut o) = packet {
                    let mut parsed_count: usize = 0;

                    while parsed_count < *total_count {
                        let parsed_packet = parser.bits[*cursor..].parse::<Packet>()?;
                        parsed_count += 1;
                        *cursor += packet_length(&parsed_packet);

                        o.packets.push(Box::new(parsed_packet));
                    }

                    o.length = Some(*cursor);
                }

                *state = Finished;
            }

            Finished => unreachable!(),

            _ => panic!("Unknown parser state!"),
        }
    }

    Ok(packet)
}

// TODO(shawk): add additional error metadata
#[derive(Debug)]
pub struct PacketParseError;

impl Error for PacketParseError {}

impl fmt::Display for PacketParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse packet")
    }
}

impl FromStr for Packet {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Packet, Self::Err> {
        if is_bin_str(s) {
            parse_packet(s)
        } else {
            parse_packet(
                &s.chars()
                    .map(|c| hex_to_bin(c.to_string()))
                    .collect::<Result<Vec<String>, Box<dyn Error>>>()?
                    .join(""),
            )
        }
    }
}

fn is_bin_str(s: &str) -> bool {
    s.chars().all(|c| c == '0' || c == '1')
}

fn hex_to_bin(s: String) -> Result<String, Box<dyn Error>> {
    Ok(format!("{:0>4b}", usize::from_str_radix(&s, 16)?))
}

fn bin_to_int(s: &str) -> Result<usize, Box<dyn Error>> {
    let bit_string: String = s
        .chars()
        .map(|b| b.to_string())
        .collect::<Vec<String>>()
        .join("");

    Ok(usize::from_str_radix(&bit_string, 2)?)
}

#[aoc_generator(day16)]
fn input_generator(input: &str) -> Result<Packet, Box<dyn Error>> {
    Ok(input.parse()?)
}

#[aoc(day16, part1)]
fn part1(packet: &Packet) -> usize {
    version_sum(&packet)
}

#[aoc(day16, part2)]
fn part2(packet: &Packet) -> usize {
    0
}

fn version_sum(packet: &Packet) -> usize {
    match packet {
        Packet::Literal(p) => p.version,
        Packet::Operator(o) => o.version + o.packets.iter().map(|p| version_sum(p)).sum::<usize>(),
        Packet::Uninitialized => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal() {
        let input = "D2FE28";
        let packet: Packet = input.parse().unwrap();

        if let Packet::Literal(p) = &packet {
            assert_eq!(p.version, 6);
            assert_eq!(p.type_id, 4);
            assert_eq!(p.value, Some(2021));
        }

        assert_eq!(version_sum(&packet), 6);
    }

    #[test]
    fn operator() {
        let input = "38006F45291200";
        let packet: Packet = input.parse().unwrap();

        if let Packet::Operator(o) = &packet {
            assert_eq!(o.version, 1);
            assert_eq!(o.type_id, 6);
        }

        assert_eq!(version_sum(&packet), 9);
    }

    #[test]
    fn example1() {
        // 8A004A801A8002F478 represents an operator packet (version 4) which
        // contains an operator packet (version 1) which contains an operator
        // packet (version 5) which contains a literal value (version 6); this
        // packet has a version sum of 16.
        let input = "8A004A801A8002F478";
        let packet: Packet = input.parse().unwrap();

        if let Packet::Operator(o) = &packet {
            assert_eq!(o.version, 4);
        }

        assert_eq!(version_sum(&packet), 16);
    }

    #[test]
    fn example2() {
        // 620080001611562C8802118E34 represents an operator packet (version 3)
        // which contains two sub-packets; each sub-packet is an operator packet
        // that contains two literal values. This packet has a version sum of 12.
        let input = "620080001611562C8802118E34";
        let packet: Packet = input.parse().unwrap();

        if let Packet::Operator(o) = &packet {
            assert_eq!(o.version, 3);
        }

        assert_eq!(version_sum(&packet), 12);
    }

    #[test]
    fn example3() {
        // C0015000016115A2E0802F182340 has the same structure as the previous
        // example, but the outermost packet uses a different length type ID.
        // This packet has a version sum of 23.
        let input = "C0015000016115A2E0802F182340";
        let packet: Packet = input.parse().unwrap();

        if let Packet::Operator(o) = &packet {
            assert_eq!(o.version, 6);
        }

        assert_eq!(version_sum(&packet), 23);
    }

    #[test]
    fn example4() {
        // A0016C880162017C3686B18A3D4780 is an operator packet that contains an
        // operator packet that contains an operator packet that contains five
        // literal values; it has a version sum of 31.
        let input = "A0016C880162017C3686B18A3D4780";
        let packet: Packet = input.parse().unwrap();

        if let Packet::Operator(o) = &packet {
            assert_eq!(o.version, 5);
        }

        assert_eq!(version_sum(&packet), 31);
    }
}
