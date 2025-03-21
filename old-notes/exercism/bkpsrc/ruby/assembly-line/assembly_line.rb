##
# Idea from Lapizistik in discord ruby server.
##

class AssemblyLine
  CARS_PER_HOUR = 221

  def initialize(speed)
    @speed = speed
  end

  def production_rate_per_hour
    factor = case @speed
      when 1..4
        1.0
      when 5..8
        0.9
      when 9
        0.8
      when 10
        0.77
      end

    @speed * CARS_PER_HOUR * factor
  end

  def working_items_per_minute
    (production_rate_per_hour / 60).to_i
  end
end
