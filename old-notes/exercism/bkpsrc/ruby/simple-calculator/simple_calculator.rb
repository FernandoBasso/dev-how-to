##
# Improvements made from code review and mentoring from the @kotp.
##

module CalculatorExceptions
  class UnsupportedOperationError < ArgumentError
    def initialize(message = 'A valid operation must be provided.')
      super
    end
  end

  UnsupportedOperation = UnsupportedOperationError
end

module SimpleCalculatorExceptions
  class IntegerOperandError < ArgumentError
    def initialize(message = 'Operands must be integers.')
      super
    end
  end
end

class SimpleCalculator
  include CalculatorExceptions, SimpleCalculatorExceptions

  OPERATE = {
    '+' => ->(operand1, operand2) { operand1 + operand2 },
    '*' => ->(operand1, operand2) { operand1 * operand2 },
    '/' => ->(dividend, divisor) { dividend / divisor },
  }

  REPORT = '%<operand1>i %<operator>s %<operand2>i = %<result>i'

  def self.operation_allowed?(operator)
    OPERATE.keys.member?(operator)
  end

  private_class_method :operation_allowed?

  ##
  # Applies the operator to the operands and returns a string
  # representing the entire expression with the answer or an
  # error message if some invalid input is provided.
  #
  # @param operand1 [Integer]
  # @param operand2 [Integer]
  # @param operator ['+', '*', '/']
  #
  def self.calculate(operand1, operand2, operator)
    raise UnsupportedOperation unless operation_allowed?(operator)
    raise IntegerOperandError unless
      [operand1, operand2].all? { |operand| operand.is_a?(Integer) }

    new(operand1, operand2, operator).to_s
  rescue ZeroDivisionError
    'Division by zero is not allowed.'
  end

  private

  attr_reader :operand1, :operand2, :operator, :report

  def initialize(operand1, operand2, operator, report: REPORT)
    @operand1 = operand1
    @operand2 = operand2
    @operator = operator
    @report = report
  end

  def operate
    OPERATE[operator].call(operand1, operand2)
  end

  public

  def to_s
    report % { operand1:, operator:, operand2:, result: operate }
  end
end


if $PROGRAM_NAME == __FILE__

  # expected normal use
  puts SimpleCalculator.calculate(1, 2, '+')
  puts SimpleCalculator.new(1, 2, '+')

  puts

  # examination as a debugging example
  p SimpleCalculator.new(1, 2, '+').to_s
  p SimpleCalculator.new(1, 2, '+')

  puts

  # additions from outside the class as a user of the library!
  SimpleCalculator::OPERATE.merge!({'**' => ->(base, power) { base ** power }})

  puts SimpleCalculator.calculate(2, 8, '**')

  puts

  # an example of a custom report string provided by user
  # an example of "single quote heredoc"
  my_report = <<~eos
    %<operand1>4i
    ———— = %<result>s
    %<operand2>4i
  eos

  puts SimpleCalculator.new(1024, 4, '/', report: my_report)

  puts

  # another example of adding operation from outside of the class as
  # a user of the library
  SimpleCalculator::OPERATE.merge!(
    {'-' => ->(operand1, operand2) { operand1 - operand2 }}
  )

  puts SimpleCalculator.calculate(3, 2, '-')

end
