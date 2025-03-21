module RANGES
  FOR_ONE_HUNDRED_PERCENT = proc { |speed| speed <= 4 }
  FOR_NINETY_PERCENT = proc { |speed| 5 <= speed && speed <= 8 }
  FOR_EIGHTY_PERCENT = proc { |speed| speed == 9 }
  FOR_SEVENTY_SEVEN_PERCENT = proc { |speed| speed > 9 }
end

class AssemblyLine
  include RANGES

  CARS_PER_HOUR = 221

  def initialize(speed)
    @speed = speed
  end

  def production_rate_per_hour
    case @speed
    when FOR_ONE_HUNDRED_PERCENT
      @speed * AssemblyLine::CARS_PER_HOUR * 1.0
    when FOR_NINETY_PERCENT
      @speed * AssemblyLine::CARS_PER_HOUR * 0.9
    when FOR_EIGHTY_PERCENT
      @speed * AssemblyLine::CARS_PER_HOUR * 0.8
    when FOR_SEVENTY_SEVEN_PERCENT
      @speed * AssemblyLine::CARS_PER_HOUR * 0.77
    end
  end

  def working_items_per_minute
    (production_rate_per_hour / 60).to_i
  end
end
