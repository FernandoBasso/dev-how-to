# Ruby Questions

- [&& vs and](#vs-and)
- [falsy values](#-falsy-values)
- [hash keys to array sorted by length](#hash-keys-to-array-sorted-by-length)


## && vs and

```ruby
x = true and false # <1>
y = true && false  # <2>
```

1. `and` has lower precedence than `=`, therefore, `x = true and false` is the same as `(x = true) and false`, which means `true` is assigned to `x`.

2. `&&` has higher precedence than `=`, therefore, the value `false` (the result of `true && false`) is assigned to `y`.


## falsy values

Which expression will produce 'yes'‽

```ruby
true    ? 'yes' : 'nope' # 1
false   ? 'yes' : 'nope' # 2
nil     ? 'yes' : 'nope' # 3
1       ? 'yes' : 'nope' # 4
0       ? 'yes' : 'nope' # 5
'false' ? 'yes' : 'nope' # 6
''      ? 'yes' : 'nope' # 7
[]      ? 'yes' : 'nope' # 8
```

Only expressions 2 adn 3 will produce 'yes'. In Ruby, only `false` and `nil` are /falsy/ values. Everything else, including `[]`, `{}`, 0 and `''` are truthy.


## hash keys to array sorted by length

Create an array with a hash's keys sorted by the length of the keys, as strings.

https://stackoverflow.com/a/33326503/2855955

Input hash:

```ruby
hash = {
  skill: 'The Force',
  foo: 'bar',
  jedi: 'Yoda',
  1 => 'one',
}
```

Sort by length, ascending.

```ruby
p hash.keys.map(&:to_s).sort { |a, b| a.length <=> b.length }

p hash.keys.map(&:to_s).sort_by { |e| e.length }
```

All of the above produce this result:
```
["1", "foo", "jedi", "skill"]
```


```ruby
p hash.keys.map(&:to_s).sort_by { |e| e.length * -1 }

p hash.keys.map(&:to_s).sort_by { |e| -e.length }

p hash.keys.map(&:to_s).sort { |a, b| b.length <=> a.length }
```

All of the above produce the following result:
```
["skill", "jedi", "foo", "1"]
```
