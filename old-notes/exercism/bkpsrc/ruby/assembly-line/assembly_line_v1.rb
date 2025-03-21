##
# Solution 1 using if/else like explained in the page.
#
class AssemblyLine
  CARS_PER_HOUR = 221

  def initialize(speed)
    @speed = speed
  end

  def production_rate_per_hour
    if @speed <= 4
      @speed * AssemblyLine::CARS_PER_HOUR * 1.0
    elsif 5 <= @speed && @speed <= 8
      @speed * AssemblyLine::CARS_PER_HOUR * 0.9
    elsif @speed == 9
      @speed * AssemblyLine::CARS_PER_HOUR * 0.8
    else
      @speed * AssemblyLine::CARS_PER_HOUR * 0.77
    end
  end

  def working_items_per_minute
    (production_rate_per_hour / 60).to_i
  end
end
