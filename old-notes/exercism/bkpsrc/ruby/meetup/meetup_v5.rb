#
# Solution by Lapizistik on Ruby Discord server.
#
# https://discord.com/channels/518658712081268738/650031651845308419/1080239306356035705
#

require 'date'

class Meetup
  VERSION = 5

  CALENDAR_SCHEDULES = {
    first: 1,
    second: 8,
    third: 15,
    fourth: 22,
    teenth: 13,
  }.freeze

  DAYS_OF_WEEK = %i[
    sunday monday tuesday wednesday thursday friday saturday
  ].map.with_index { |d, i| [d, i] }.to_h

  def initialize(month, year)
    @month = month
    @year = year

    ##
    # The last schedule is month-specific.
    #
    days_in_month = (Date.new(year, month).next_month - 1).day
    @last_schedule = days_in_month - 6
  end

  # 2024, 2, :second
  # 2024, 2,
  def day(weekday, schedule)
    date_schedule_starts = Date.new(@year, @month, schedule_start(schedule))
    wday = DAYS_OF_WEEK[weekday]

    # <1>
    wday += 7 if wday < date_schedule_starts.wday
    date_schedule_starts + (wday - date_schedule_starts.wday)
  end

  def schedule_start(schedule)
    return @last_schedule if schedule == :last

    CALENDAR_SCHEDULES[schedule] or
      raise "Unknown schedule: “#{schedule.inspect}”"
  end
end


#
# <1> If the day of week we target is >= the start day of the
# schedule then I just use it.
#
# So if my schedule starts with Tuesday (2) and I want a Wednesday (3) I
# just need to add one day (3 - 1).
#
# But if my schedule starts on Friday (5) and I want a Monday (1) I
# would need to go back 4 days (5-1) but then I would end up before my
# start day, so I just add a full week (7) to get the next Monday.
#
