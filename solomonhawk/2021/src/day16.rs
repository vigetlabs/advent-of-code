use std::error::Error;
use std::fmt;
use std::str::FromStr;

struct Parser<'a> {
    bits: &'a str,
    cursor: usize,
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

#[derive(Debug, PartialEq)]
enum PacketType {
    Unknown,
    Literal,
    Operator(OperatorType),
}

#[derive(Debug)]
enum LengthType {
    Length = 0,
    Count = 1,
    Unknown = 2,
}

impl LengthType {
    fn from_usize(value: usize) -> LengthType {
        match value {
            0 => LengthType::Length,
            1 => LengthType::Count,
            2 => LengthType::Unknown,
            _ => panic!("Unknown length type value: {}", value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum OperatorType {
    Sum = 0,
    Product = 1,
    Min = 2,
    Max = 3,
    Literal = 4,
    Gt = 5,
    Lt = 6,
    Eq = 7,
}

impl OperatorType {
    fn from_usize(value: usize) -> OperatorType {
        match value {
            0 => OperatorType::Sum,
            1 => OperatorType::Product,
            2 => OperatorType::Min,
            3 => OperatorType::Max,
            4 => OperatorType::Literal,
            5 => OperatorType::Gt,
            6 => OperatorType::Lt,
            7 => OperatorType::Eq,
            _ => panic!("Unknown operator type value: {}", value),
        }
    }
}

#[derive(Debug)]
struct Packet {
    version: usize,
    packet_type: PacketType,
    length_type: LengthType,
    length: Option<usize>,
    packets: Vec<Box<Packet>>,
    value: Option<usize>,
}

fn parse_packet(bits: &str) -> Result<Packet, Box<dyn Error>> {
    use ParserState::*;

    let mut packet = Packet {
        version: 0,
        packet_type: PacketType::Unknown,
        length_type: LengthType::Unknown,
        length: None,
        packets: vec![],
        value: None,
    };

    let mut parser = Parser {
        bits,
        cursor: 0,
        state: ParseVersion,
    };

    let state = &mut parser.state;
    let cursor = &mut parser.cursor;

    while *state != Finished {
        match state {
            // 3 bits (version)
            ParseVersion => {
                packet.version = bin_to_int(&slice_bits(&parser.bits, cursor, 3))?;
                *state = ParseTypeID;
            }

            // 3 bits (type id)
            ParseTypeID => {
                let operator_type =
                    OperatorType::from_usize(bin_to_int(&slice_bits(&parser.bits, cursor, 3))?);

                match operator_type {
                    OperatorType::Literal => {
                        packet.packet_type = PacketType::Literal;
                        *state = ParseLiteralValue;
                    }

                    t => {
                        packet.packet_type = PacketType::Operator(t);
                        *state = ParseLengthTypeID;
                    }
                }
            }

            // 1 bit (length type id)
            ParseLengthTypeID => {
                packet.length_type =
                    LengthType::from_usize(bin_to_int(&slice_bits(&parser.bits, cursor, 1))?);
                *state = ParseLength;
            }

            // 11 or 15 bits, depending on length type id
            ParseLength => {
                match packet.length_type {
                    // next 15 bits are a number that represents the total length in bits contained by this packet
                    LengthType::Length => {
                        let length_bit_string = &slice_bits(&parser.bits, cursor, 15);
                        *state = ParseSubPacketsByLength(bin_to_int(&length_bit_string)?);
                    }

                    // next 11 bits are a number that represents the number of sub-packets immediately contained by this packet
                    LengthType::Count => {
                        let length_bit_string = &slice_bits(&parser.bits, cursor, 11);
                        *state = ParseSubPacketsByCount(bin_to_int(&length_bit_string)?);
                    }

                    _ => unreachable!(),
                }
            }

            // variable length, chunks of 5, last chunk denoted by leading 0
            ParseLiteralValue => {
                let mut slice;
                let mut bit_string = String::new();

                loop {
                    slice = slice_bits(&parser.bits, cursor, 5);
                    bit_string.push_str(&slice.chars().skip(1).collect::<String>());

                    if slice.chars().nth(0) == Some('0') {
                        break;
                    }
                }

                packet.length = Some(*cursor);
                packet.value = Some(bin_to_int(&bit_string)?);

                *state = Finished;
            }

            // sub packets with known length (recurse)
            ParseSubPacketsByLength(total_length) => {
                let mut parsed_length = 0;

                while parsed_length < *total_length {
                    let parsed_packet: Packet = parser.bits[*cursor..].parse()?;
                    let packet_len = &parsed_packet.length.expect("Packet must have a length");

                    parsed_length += packet_len;
                    *cursor += packet_len;

                    packet.packets.push(Box::new(parsed_packet));
                }

                packet.length = Some(*cursor);

                *state = CalculateOperationValue;
            }

            // sub packets with known count (recurse)
            ParseSubPacketsByCount(total_count) => {
                let mut parsed_count = 0;

                while parsed_count < *total_count {
                    let parsed_packet: Packet = parser.bits[*cursor..].parse()?;
                    parsed_count += 1;
                    *cursor += &parsed_packet.length.expect("Packet must have a length");

                    packet.packets.push(Box::new(parsed_packet));
                }

                packet.length = Some(*cursor);

                *state = CalculateOperationValue;
            }

            // after parsing, a value can be computed for operator type packets
            CalculateOperationValue => {
                match packet.packet_type {
                    PacketType::Operator(OperatorType::Sum) => {
                        packet.value = Some(packet.packets.iter().map(|p| deref(p)).sum());
                    }

                    PacketType::Operator(OperatorType::Product) => {
                        packet.value = Some(packet.packets.iter().map(|p| deref(p)).product());
                    }

                    PacketType::Operator(OperatorType::Min) => {
                        packet.value = packet.packets.iter().map(|p| deref(p)).min()
                    }

                    PacketType::Operator(OperatorType::Max) => {
                        packet.value = packet.packets.iter().map(|p| deref(p)).max()
                    }

                    PacketType::Operator(OperatorType::Gt) => {
                        packet.value = if deref(&packet.packets[0]) > deref(&packet.packets[1]) {
                            Some(1)
                        } else {
                            Some(0)
                        }
                    }

                    PacketType::Operator(OperatorType::Lt) => {
                        packet.value = if deref(&packet.packets[0]) < deref(&packet.packets[1]) {
                            Some(1)
                        } else {
                            Some(0)
                        }
                    }

                    PacketType::Operator(OperatorType::Eq) => {
                        packet.value = if deref(&packet.packets[0]) == deref(&packet.packets[1]) {
                            Some(1)
                        } else {
                            Some(0)
                        }
                    }

                    _ => unreachable!(),
                }

                *state = Finished;
            }

            // "This should never happen." -Solomon Hawk, 12/16/2021
            Finished => unreachable!(),
        }
    }

    Ok(packet)
}

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

fn deref(packet: &Packet) -> usize {
    match packet.value {
        Some(p) => p,
        None => panic!("Cannot deref a packet without a value"),
    }
}

/**
 * Returns a string slice of a packet binary string starting at `cursor` and
 * ending at `cursor + amount`. Also increments `cursor` by `amount`.
 */
fn slice_bits<'a>(s: &'a str, cursor: &mut usize, amount: usize) -> &'a str {
    *cursor += amount;
    &s[*cursor - amount..*cursor]
}

fn version_sum(packet: &Packet) -> usize {
    packet.version + packet.packets.iter().map(|p| version_sum(p)).sum::<usize>()
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
    deref(packet)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal() {
        let input = "D2FE28";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.version, 6);
        assert_eq!(packet.packet_type, PacketType::Literal);
        assert_eq!(packet.value, Some(2021));

        assert_eq!(version_sum(&packet), 6);
    }

    #[test]
    fn operator() {
        let input = "38006F45291200";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.version, 1);
        assert_eq!(packet.packet_type, PacketType::Operator(OperatorType::Lt));

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

        assert_eq!(packet.version, 4);

        assert_eq!(version_sum(&packet), 16);
    }

    #[test]
    fn example2() {
        // 620080001611562C8802118E34 represents an operator packet (version 3)
        // which contains two sub-packets; each sub-packet is an operator packet
        // that contains two literal values. This packet has a version sum of 12.
        let input = "620080001611562C8802118E34";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.version, 3);

        assert_eq!(version_sum(&packet), 12);
    }

    #[test]
    fn example3() {
        // C0015000016115A2E0802F182340 has the same structure as the previous
        // example, but the outermost packet uses a different length type ID.
        // This packet has a version sum of 23.
        let input = "C0015000016115A2E0802F182340";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.version, 6);

        assert_eq!(version_sum(&packet), 23);
    }

    #[test]
    fn example4() {
        // A0016C880162017C3686B18A3D4780 is an operator packet that contains an
        // operator packet that contains an operator packet that contains five
        // literal values; it has a version sum of 31.
        let input = "A0016C880162017C3686B18A3D4780";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.version, 5);

        assert_eq!(version_sum(&packet), 31);
    }

    #[test]
    fn sum_op() {
        // C200B40A82 finds the sum of 1 and 2, resulting in the value 3
        let input = "C200B40A82";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(3));
    }

    #[test]
    fn product_op() {
        // 04005AC33890 finds the product of 6 and 9, resulting in the value 54
        let input = "04005AC33890";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(54));
    }

    #[test]
    fn min_op() {
        // 880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7
        let input = "880086C3E88112";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(7));
    }

    #[test]
    fn max_op() {
        // CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9
        let input = "CE00C43D881120";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(9));
    }

    #[test]
    fn gt_op() {
        // D8005AC2A8F0 produces 1, because 5 is less than 15
        let input = "D8005AC2A8F0";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(1));
    }

    #[test]
    fn lt_op() {
        // F600BC2D8F produces 0, because 5 is not greater than 15
        let input = "F600BC2D8F";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(0));
    }

    #[test]
    fn eq_op() {
        // 9C005AC2F8F0 produces 0, because 5 is not equal to 15.
        let input = "9C005AC2F8F0";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(0));
    }

    #[test]
    fn eq_nested_op() {
        // 9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2
        let input = "9C0141080250320F1802104A08";
        let packet: Packet = input.parse().unwrap();

        assert_eq!(packet.value, Some(1));
    }
}
