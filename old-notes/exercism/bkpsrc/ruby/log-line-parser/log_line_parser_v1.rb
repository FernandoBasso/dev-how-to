class LogLineParser
  def initialize(line)
    @line = line
  end

  def message
    ##
    # The first `gsub` replaces tabs and newlines with a single
    # space.
    #
    # The second `gsub` matches an initial literal ‘[’ followed by
    # character that is NOT a closing (literal) `]` one or more
    # times. Then matches the char sequence ‘]: ’ (not the space)
    # and replaces the entire match nothing, effectively “deleting”
    # whatever was matched.
    #
    @line.gsub(/(\t|\r\n?)/, ' ').gsub(/^\[[^\]]+\]:\s+/, '').strip
  end

  def log_level
    @line.match(/\[([^\]]+)\]/)[1].downcase
  end

  def reformat
    "#{message} (#{log_level})"
  end
end
