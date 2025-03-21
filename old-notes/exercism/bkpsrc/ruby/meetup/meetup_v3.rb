require 'date'

class Meetup
  VERSION = 3

  DAYS_OF_WEEK = {
    0 => :sunday,
    1 => :monday,
    2 => :tuesday,
    3 => :wednesday,
    4 => :thursday,
    5 => :friday,
    6 => :saturday,
  }.freeze

  def initialize(month, year)
    @month = month
    @year = year
    @days_in_month = (Date.new(year, 12, 31) << (12 - month)).day

    @calendar = {
      first: [*1..7],
      second: [*8..14],
      third: [*15..21],
      fourth: [*22..28],
      teenth: [*13..19],
      last: [*(@days_in_month - 6)..@days_in_month],
    }
  end

  def day(weekday, schedule)
    section_of_month = @calendar[schedule]

    is_weekday = lambda do |n|
      DAYS_OF_WEEK[Date.new(@year, @month, n).wday] == weekday
    end

    Date.new(@year, @month, section_of_month.find(&is_weekday))
  end
end
