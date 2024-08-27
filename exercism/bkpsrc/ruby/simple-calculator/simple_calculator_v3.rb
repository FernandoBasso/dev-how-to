##
# Improvements made from code review and mentoring from the @kotp.
##

module SimpleCalculatorExceptions
  class UnsupportedOperation < ArgumentError
    def initialize(msg = 'A valid operation must be provided.')
      super
    end
  end
end

class SimpleCalculator
  include SimpleCalculatorExceptions

  ##
  # The operations currently handled by this class.
  #
  OPS = ALLOWED_OPERATIONS = ['+', '/', '*'].freeze

  OP = OPERATE = {
    '+' => ->(operand1, operand2) { operand1 + operand2 },
    '*' => ->(operand1, operand2) { operand1 * operand2 },
    '/' => ->(dividend, divisor) { dividend / divisor },
  }

  private_constant :OP

  ##
  # Applies the operator to the operands and returns the result.
  #
  # @param operand1 [Integer]
  # @param operand2 [Integer]
  # @param operator ['+', '*', '/']
  #
  def self.calculate(operand1, operand2, operator)
    raise ArgumentError.new('Operands must be integers.') unless
      [operand1, operand2].all? { |operand| operand.is_a?(Integer) }

    raise UnsupportedOperation unless ALLOWED_OPERATIONS.member?(operator)

    begin
      '%i %s %i = %i' %
        [
          operand1,
          operator,
          operand2,
          OP[operator].call(operand1, operand2),
        ]
    rescue ZeroDivisionError => err
      'Division by zero is not allowed.'
    end
  end
end
