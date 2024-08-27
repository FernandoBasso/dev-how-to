class ResistorColorDuo
   COLORS = {
    'black' => 0,
    'brown' => 1,
    'red' => 2,
    'orange' => 3,
    'yellow' => 4,
    'green' => 5,
    'blue' => 6,
    'violet' => 7,
    'grey' => 8,
    'white' => 9,
  }

  class << self
    ##
    # Get the color values for the given pair (two-element array) of
    # color names.
    #
    # @param color_names '{Array<String>} An array of two color
    #   name string.
    # @return String A string with the two color values.
    #
    def value(color_names)
      color_names.take(2).map(&COLORS).join('').to_i
    end
  end
end
