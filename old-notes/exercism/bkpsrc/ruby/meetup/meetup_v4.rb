#
# Solution by Lapizistik on Ruby Discord server.
#
# https://discord.com/channels/518658712081268738/650031651845308419/1080239306356035705
#

require 'date'

class Meetup
  VERSION = 4

  ##
  # Map the symbols to the week numbers.
  #
  DAYS_OF_WEEK = %i[
    sunday monday tuesday wednesday thursday friday saturday
  ].map.with_index { |d, i| [d, i] }.to_h

  def initialize(month, year)
    @month = month
    @year = year
    days_in_month = (Date.new(year, month).next_month - 1).day

    @schedules = {
      first: 1,
      second: 8,
      third: 15,
      fourth: 22,
      teenth: 13,
      last: days_in_month - 6,
    }
  end

  def day(weekday, schedule)
    start = @schedules[schedule]
    wday = DAYS_OF_WEEK[weekday]

    d = Date.new(@year, @month, start)

    d + (wday - d.wday) + (wday < d.wday ? 7 : 0)
  end
end

#
# If the day of week targeted is after the start day of the schedule
# then I just use it. So if my schedule starts with Tuesday (2) and I
# want a Wednesday (3) I just need to add one day (3 - 1). But if my
# schedule starts on Friday (5) and I want a Monday (1) I would need to
# go back 4 days (5 - 1) but then I would end up before my start day, so
# I just add a full week (7) to get the next Monday.
#
