import day_2.{Command, Location, Forward, Down, Up}

pub fn parse_direction_test() {
  assert Forward = day_2.parse_direction("forward")
  assert Up = day_2.parse_direction("up")
  assert Down = day_2.parse_direction("down")
}

pub fn parse_command_test() {
  assert Command(dir: Forward, magnitude: 100) =
    day_2.parse_command("forward 100")
}

pub fn apply_command_test() {
  assert Location(x: 10, y: 0) =
    day_2.apply_command(Location(0, 0), Command(Forward, 10))

  assert Location(x: 10, y: 0) =
    day_2.apply_command(Location(10, 10), Command(Down, 10))

  assert Location(x: 0, y: 100) =
    day_2.apply_command(Location(0, 0), Command(Up, 100))
}

