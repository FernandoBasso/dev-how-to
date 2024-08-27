=begin
Write your code for the 'Resistor Color Duo' exercise in this file. Make the
tests in `resistor_color_duo_test.rb` pass.

To get started with TDD, see the `README.md` file in your
`ruby/resistor-color-duo` directory.
=end

class ResistorColorDuo
  @color_names = [
    'black',
    'brown',
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'violet',
    'grey',
    'white',
  ]

  class << self
    ##
    # Return an array of indexes from the color names. Example:
    #
    # ['black', 'red', 'grey'] -> [0, 2]
    #
    # At most two colors are taken into consideration. Excess
    # colors are simply ignored.
    #
    def to_index_array(colors)
      colors.take(2).collect do |color|
        @color_names.index(color)
      end
    end

    ##
    # Get the color values for the given pair (two-element array)
    # of color names.
    #
    # @param color_names '{Array<String>} An array of two color
    #   name string.
    # @return String A string with the two color values.
    #
    def value(colors)
      to_index_array(colors).join('').to_i
    end
  end
end
