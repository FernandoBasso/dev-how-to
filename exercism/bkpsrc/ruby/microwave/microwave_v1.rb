Microwave = Class.new do
  def initialize(seconds)
    @seconds = seconds
  end

  ##
  # Convert an input in seconds to a timer display hh:mm.
  #
  # @return [String]
  #
  def timer
    secs = @seconds.to_s

    if secs.size <= 2
      hours, seconds = @seconds.divmod(60)
      return pad(hours) << ':' << pad(seconds)
    else
      overflow_hours, remaining_seconds = seconds(secs).divmod(60)
      pad(hours(secs) + overflow_hours) << ':' << pad(remaining_seconds)
    end
  end

  private

  ##
  # Get the seconds of a three-or-more-digit string.
  #
  # @param secs [String]
  # @return [Integer]
  #
  def seconds(secs)
    #
    # This range returns the last two digits of the digit string,
    # which are the seconds. For example, in '257', 2 is the hour,
    # and 57 is the seconds.
    #
    secs[-2 .. -1].to_i
  end

  ##
  # Get the hours from a three-or-more-digit string.
  #
  def hours(secs)
    hours_len = secs.size - 2

    ##
    # If we have seconds like '257', and 57 is the seconds, then we
    # have a range from 0...1, which returns 2. Since 257 has three
    # digits, 3 - 2 is 1, which we use in the range.
    #
    # If we have seconds like '1001', then the seconds is 01 and the
    # hours is the range from 0...2, which will return 10. Since
    # 1001 has four digits, 4 - 2 is 2, which we use in the range.
    #
    secs[0...hours_len].to_i
  end

  ##
  # Pad n with 0 to make sure it is always a two digit string.
  #
  # Simply return the number as string if no padding needed.
  #
  # @param n [Integer]
  # @return [String]
  #
  def pad(n)
    s = n.to_s

    s.size < 2 ? '0' << s : s
  end
end
