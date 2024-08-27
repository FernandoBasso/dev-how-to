class Acronym
  VERSION = 2

  class << self
    #
    # Given a string, produces an acronym.
    #
    # @param s [String]
    # @return [String]
    #
    def abbreviate(s)
      ##
      # `scan` will more or less do what `split` and
      # `map` would do if used in combination.
      #
      s.scan(/\b\w/).join.upcase
      #       ----   ----
      #         |      \
      #         |       \
      #         |        +--> Same as `join('')`.
      #         v
      # Matches a char at the beginning
      # of a word boundary.
      ##
    end
  end
end

##
# >> 'ab cd ef'.split(/ |-/).map { |s| s[0] }
# => ["a", "c", "e"]
#
# >> 'ab cd ef'.scan(/\b\w/)
# => ["a", "c", "e"]
##
