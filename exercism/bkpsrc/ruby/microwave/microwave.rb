Microwave = Class.new do
  def initialize(seconds)
    @seconds = seconds
  end

  ##
  # Convert an input in seconds to a timer display hh:mm.
  #
  # @return [String]
  #
  def timer
    tmp_hours, tmp_seconds = @seconds.divmod(100)
    overflow_hours, remaining_seconds = tmp_seconds.divmod(60)

    '%02d:%02d' % [tmp_hours + overflow_hours, remaining_seconds]
  end
end

=begin
The first solution used modular division for the seconds, but the idea can be extended so it also satisfy cases where the input is something like 272.

`272.divmod(10)` results in `2, 72`.

So we have 2 hours and 72 seconds.
We can then get those 72 seconds and do modular division in base 60.
`72.divmod(60)` results in `1, 12`, which means 72 seconds is actually 1 hour and 12 seconds.

We then simply add the 2 hours from the first modular division by 100 to the 1 hour we got from the modular division by 60.
It in the end, it gives us 3 hours and 12 seconds.

The same approach works with other inputs like 7, 93, 272, or 1001.
Let's see some IRB session examples.

.example with input 7
[source,irb]
----
>> tmp_hours, tmp_seconds = 7.divmod(100)
=> [0, 7]

#                                           7
>> overflow_hours, remaining_seconds = tmp_seconds.divmod(60)
=> [0, 7]

#                      0            0                 7
>> '%02d:%02d' % [tmp_hours + overflow_hours, remaining_seconds]
=> "00:07"
----

Both `tmp_hours` and `overflow_hours` are 0, and 0 + 0 is still 0.
The remaining seconds is still 7, and the resulting string is "00:07", which is 100% the expected result!

.example with input 93
[source,irb]
----
>> tmp_hours, tmp_seconds = 93.divmod(100)
=> [0, 93]

#                                           93
>> overflow_hours, remaining_seconds = tmp_seconds.divmod(60)
=> [1, 33]

#                    0      +       1       ,        33
>> '%02d:%02d' % [tmp_hours + overflow_hours, remaining_seconds]
=> "01:33"
----

.example with input 272
[source,irb]
----
>> tmp_hours, tmp_seconds = 272.divmod(100)
=> [2, 72]

#                                           72
>> overflow_hours, remaining_seconds = tmp_seconds.divmod(60)
=> [1, 12]

#                     2     +        1      ,       12
>> '%02d:%02d' % [tmp_hours + overflow_hours, remaining_seconds]
=> "03:12"
----

.example with input 1001
[source,irb]
----
>> tmp_hours, tmp_seconds = 1001.divmod(100)
=> [10, 1]

#                                            1
>> overflow_hours, remaining_seconds = tmp_seconds.divmod(60)
=> [0, 1]

#                     10    +         0     ,        1
>> '%02d:%02d' % [tmp_hours + overflow_hours, remaining_seconds]
=> "10:01"
----
=end
