class SimpleCalculator
  ALLOWED_OPS = ['+', '/', '*'].freeze

  OPS = {
    '+' => proc { |a, b| a + b },
    '*' => proc { |a, b| a * b },
    '/' => proc { |a, b| a / b },
  }

  class UnsupportedOperation < StandardError; end

  def self.calculate(a, b, op)
    raise ArgumentError.new('Operands must be integers.') unless
      [a, b].all? { |v| v.is_a?(Integer) }

    raise UnsupportedOperation.new unless
      OPS.member?(op)

    begin
      '%s %s %s = %s' % [a, op, b, OPS[op].call(a, b)]
    rescue ZeroDivisionError => err
      'Division by zero is not allowed.'
    end
  end
end
