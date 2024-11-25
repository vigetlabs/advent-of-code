import gleam/should
import day_1.{count_increases}

pub fn count_increases_test() {
  let counts = [
    1,
    2,
    3
  ]
  count_increases(counts)
  |> should.equal(2)

  let counts = [
    199,
    200,
    208,
    210,
    200,
    207,
    240,
    269,
    260,
    263
  ]

  count_increases(counts)
  |> should.equal(7)
}
