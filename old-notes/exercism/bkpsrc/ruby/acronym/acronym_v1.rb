class Acronym
  VERSION = 1

  class << self
    #
    # Given a string, produces an acronym.
    # @param s [String]
    # @return [String]
    #
    def abbreviate(s)
      to_acronym = ->(acc, w) { acc << w[0].upcase }

      #
      # • Substitute any non-ascii with a space.
      # • Split words on spaces or ‘-’.
      # • Extract the first letter and uppercase it.
      #
      s
        .sub(/\W+/, ' ')
        .split(/ |-/)
        .inject('', &to_acronym)
    end
  end
end
