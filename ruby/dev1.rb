def a_method
  foo = :foo
  bar = :bar
  p foo, bar
  1.times do |i ;foo ,bar|
    foo = :inside_foo
    puts "#{foo} #{i}"
  end
  puts foo
end

a_method

