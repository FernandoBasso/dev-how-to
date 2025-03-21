#
# Solution by elGatoMantocko:
#
# • https://exercism.org/tracks/ruby/exercises/meetup/solutions/elGatoMantocko
#
require 'date'

class Meetup
  VERSION = 6

  SCHEDULE = %i[first second third fourth].freeze

  def initialize(month, year)
    @days_of_month = Date.new(year, month, 1)..Date.new(year, month, -1)
  end

  def day(weekday, schedule)
    days = @days_of_month.select(&:"#{weekday}?")

    case schedule
    when :teenth
      days.find do |date|
        date.day.between?(13, 19)
      end
    when :last
      days[-1]
    else
      days[SCHEDULE.index(schedule)]
    end
  end
end
