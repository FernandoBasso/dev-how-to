#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#

def fizzbuzz(range, trigger)
  range.map do |num|
    parts = trigger.select { |(_, predicate)| predicate.call(num) }
    # puts parts
    parts.size > 0 ? parts.map(&:first).join : num
  end
end

puts fizzbuzz(1..25, [
  ['Fizz', -> (n){ n % 3 == 0 }],
  ['Buzz', -> (n){ n % 5 == 0 }],
  ['Zazz', -> (n){ n < 10 }],
])

