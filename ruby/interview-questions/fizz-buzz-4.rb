#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#

def fizzbuzz(range, triggers)
  range.each do |num|
    result = ''
    triggers.each do |(text, divisor)|
      result << text if (num % divisor).zero?
    end
    puts result == '' ? num : result
  end
end

fizzbuzz(1..25, [['Fizz', 3], ['Buzz', 5]])

