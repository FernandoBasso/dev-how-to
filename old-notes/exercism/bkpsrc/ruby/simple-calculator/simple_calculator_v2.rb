module SimpleCalculatorExceptions
  class UnsupportedOperation < ArgumentError
    def initialize(msg = 'A valid operation must be provided.')
      super
    end
  end
end

class SimpleCalculator
  include SimpleCalculatorExceptions

  OPS = ALLOWED_OPERATIONS = ['+', '/', '*'].freeze

  OP = OPERATION = {
    '+' => ->(a, b) { a + b },
    '*' => ->(a, b) { a * b },
    '/' => ->(a, b) { a / b },
  }

  private_constant :OP

  def self.calculate(left_operand, right_operand, operator)
    raise ArgumentError.new('Operands must be integers.') unless
      [left_operand, right_operand].all? { |operand| operand.is_a?(Integer) }

    raise UnsupportedOperation unless ALLOWED_OPERATIONS.member?(operator)

    begin
      '%{left_operand} %{operator} %{right_operand} = %{result}' %
        {
          left_operand: left_operand,
          operator: operator,
          right_operand: right_operand,
          result: OP[operator].call(left_operand, right_operand),
        }
    rescue ZeroDivisionError => err
      'Division by zero is not allowed.'
    end
  end
end
