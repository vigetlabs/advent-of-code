require 'pry'

DIMENSION = 10

class Day10
  attr_reader :positions, :debug, :flash_count

  def initialize(filename, debug)
    @debug = debug
    @positions = []
    @flash_count = 0
    load_positions(filename)
  end

  def run
    iterate
  end

  def iterate
    flashers = []

    debug && p("before step")
    debug && print_positions


    DIMENSION.times.each do |y|
      DIMENSION.times.each do |x|
        position = positions[y][x]

        position.value += 1
        if position.flashed?
          @flash_count += 1
          flashers << position
        end
      end
    end

    debug && p("just bumped")
    debug && print_positions

    while flashers.length > 0
      debug && p("flashers: #{flashers}")

      flasher = flashers.shift

      debug && p("flashing: #{flasher.inspect}")

      flasher.neighbors(positions).each do |position|
        next if position.flashed?

        position.value += 1
        if position.flashed?
          @flash_count += 1
          flashers << position
        end
      end

      debug && print_positions
    end

    debug && p("just checked flashers")
    debug && print_positions
  end

  def print_positions
    DIMENSION.times.each do |y|
      DIMENSION.times.each do |x|
        to_print = if positions[y][x].value == 0
          " . "
        else
          " #{positions[y][x].value} "
        end

        print(debug ? to_print : to_print.strip)
      end
      puts
    end
    puts
  end


  private

  def load_positions(filename)
    lines = File.read(filename).split("\n")
    lines.each_with_index do |line, y|
      positions[y] = []

      line.split("").each_with_index do |value, x|
        position = Position.new(x, y, value)
        positions[y].append(position)
      end
    end
  end
end

class Position
  attr_reader :x, :y, :value

  def initialize(x, y, value)
    @x, @y, @value = x, y, value.to_i
  end

  DIRECTIONS = [
    [0, 1], [0, -1], [ 1, 0], [-1,  0], # cardinal
    [1, 1], [1, -1], [-1, 1], [-1, -1]  # diagonals
  ]

  def value=(new_value)
    if new_value > 9
      # Flashing
      @value = 0
    else
      @value = new_value
    end
  end

  def flashed?
    value == 0
  end

  def neighbors(positions)
    DIRECTIONS.map do |y_dir, x_dir|
      if valid_coordinates?(y + y_dir, x + x_dir)
        positions[y + y_dir][x + x_dir]
      end
    end.compact
  end

  def valid_coordinates?(xx, yy)
    [xx, yy].all? do |z|
      0 <= z && z <= 9
    end
  end

  def inspect
    "[#{x},#{y}]"
  end
end

filename = "input.txt"
debug = false
@d = Day10.new(filename, debug)
100.times { @d.run }
@d.print_positions
puts "Flash count: #{@d.flash_count}"
