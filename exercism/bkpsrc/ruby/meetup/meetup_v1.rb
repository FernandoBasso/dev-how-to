#
# tags: date, range
#

require 'date'

class Meetup
  VERSION = 1

  DAYS_OF_WEEK = {
    sunday: 0,
    monday: 1,
    tuesday: 2,
    wednesday: 3,
    thursday: 4,
    friday: 5,
    saturday: 6,
  }.freeze

  START_DAYS = {
    first: 1,
    second: 8,
    teenth: 13,
    third: 15,
    fourth: 22,
    #
    # We don't know when last starts. Last day of the month
    # could be one of 28, 29, 30, or 31. We use `last_start_day`
    # to help figure this out.
    #
  }.freeze

  def initialize(month, year)
    @month = month
    @year = year
  end

  ##
  # @param day_of_week [Symbol]
  # @param nth [Integer]
  #
  def day(day_of_week, which)
    date = Date.new(@year, @month, start_day(which))

    6.times do
      break if date.wday == DAYS_OF_WEEK[day_of_week]

      date = date.next_day
    end

    date
  end

  private

  def last_start_day
    Date.new(@year, @month).next_month - 7
  end

  def start_day(which)
    return last_start_day.mday if which == :last

    START_DAYS[which]
  end
end
