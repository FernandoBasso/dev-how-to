##
# This solution is not mine. Got it here:
#
# • https://exercism.org/tracks/ruby/exercises/meetup/solutions/kkchu791
#
# What I did was to change some bits, but the main idea is the one from the
# link above.
#

require 'date'

class Meetup
  VERSION = 2

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
      Date.new(@year, @month, n).send("#{weekday}?")
    end

    Date.new(@year, @month, section_of_month.find(&is_weekday))
  end
end
