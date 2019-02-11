# Closures, Ruby, JavaScript

- [intro](#intro)
- [reference or value](#reference-or-value)
- [reference or value explanation 2](#reference-or-value-explanation-2)

## Intro

What is a _closure_‽

A closure a scope that is created when a function or piece of runnable code remembers the values that were in scope when it was created.

The above definition applies to JavaScript. For Ruby, it is similar, but the difference is that it remembers the references (not the values themselves) that were in scope when the lambda or proc was created.

https://en.wikipedia.org/wiki/Closure_(computer_programming)


## reference or value

```js
var callFn = fn => fn();
var name = 'Yoda';
var logName = () => console.log(name);
name = 'Luke';
callFn(logName);
// → Yoda
```

Note it prints 'Yoda', the value of name at the time `logName` was defined. The scope remembers the value itself.

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


## reference or value explanation 2

An different approach to explaining closures.

```js
// Define a function that takes another function as argument
// and calls that function.
var callFn = fn => fn();

// Defines a variable and give it a value.
var name = 'Yoda';

// Defines a function that logs `name`, a variable in the function's outer scope.
var logName = () => console.log(name);

// Assign a new value to `name` _before_ invoking `callFn(logName)`.
name = 'Luke';

// Finally invoke `callFn(logName)
logName(name);
// → Yoda
```
