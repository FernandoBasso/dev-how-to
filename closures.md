# Closures, Ruby, JavaScript

- [intro](#intro)
- [reference or value](#reference-or-value)

## Intro

What is a __closure__‽

A closure is way that a function or piece of runnable code remembers the values that were in scope when it was created.

https://en.wikipedia.org/wiki/Closure_(computer_programming)


## reference or value

```js
var callFn = fn => fn();
var name = 'Yoda';
var logName = () => console.log(name);
name = 'Luke';
logName(name);
// → Yoda
```

Note it prints 'Yoda', the value of name at the time `logName` was defined. The scope remembers the value.

```rb
def call_fn(fn)
  fn.call
end

name = 'Yoda'
log_name = -> do
  puts name
end

name = 'Luke'

call_fn(log_name)
# → Luke
```

Prints 'Luke', the value of the name at the time `log_name` was invoked. The scope remembers the reference. If the value at that memory location changes, it is reflected when the methods is invoked.

