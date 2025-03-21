class LogLineParser
  attr_reader :log_level, :message

  def initialize(line)
    ##
    # The first named capturing group matches the log level without
    # its surrounding ‘[’ and ‘]’ characters.
    #
    # The second named capturing group matches the log message, which
    # is everything after the ‘:<spaces>’.
    #
    m = /\[(?<level>[^\]]+)\]:\s*(?<text>.*)/.match(line)

    #
    # Strip takes care of spaces, tabs and newlines at both ends
    # of the string.
    #

    @log_level = m[:level].strip.downcase
    @message = m[:text].strip
  end

  def reformat
    "#{message} (#{log_level})"
  end
end
