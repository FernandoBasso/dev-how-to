# Methods

- [private methods are not really private](#private-methods-are-not-really-private)

## private methods are not really private

We can't do something like `obj.my_private_method(arg)`. Private methods can't be invoked from an explicit receiver. They can only be called with the default receiver `self`. And yet, we can't use `self` explicitly. It has to be implicit.

```rb
class Enemy
  def threat=(threat)
    @threat = threat
  end

  # <1>
  def threat(yell: false)
    # <2>
    return yell_threat if yell
    @threat
  end

  private

    def yell_threat
      @threat.upcase
    end
end
```

<1>: We designed `threat` in a way that if it is passed `yell: true` (using keyword arguments), then it will invoke the private method `yell_threat`. Note the `return` keyword there. If `yell` is `true`, then we return the result of invoking `yell_threat` and the next line is not executed. If `yell` is `false` (the default value), then the condition is false, `yell_threat` is not run and the method simply returns the `@threat` instance variable.

<2>: `yell_threat` is a private method. Inside `threat`, we can invoke any instance's private method. Private methods do not allow a _receiver_. They can only be invoked from the implicit `self` object. If we write `self.yell_threat`, we get an exception. Note that we simply said `yell_threat`, because then the default receiver `self` is used and the private method runs without problems.


OK. We can invoke `threat` with the receiver because `threat` is public.

```rb
enemy1 = Enemy.new
enemy1.threat = 'I shall destroy you!'
p enemy1.threat
# → "I shall destroy you!"
```

And we can instruct `threat` to invoke `yell_threat`.

```rb
p enemy1.threat(yell: true)
# → "I SHALL DESTROY YOU!"
```

However, we can indeed invoke private methods using some meta-programming-related “tricks”.

```rb
p enemy1.send(:yell_threat)
p enemy1.method(:yell_threat).call
p enemy1.method(:yell_threat).()
p enemy1.method(:yell_threat).[]
p enemy1.method(:yell_threat).===
p enemy1.instance_eval { yell_threat }
# All of the above work.
```

To make sure you don't invoke a private method by accident, it is possible to use `public_send` instead `send` and its ilk.

```rb
p enemy1.public_send(:yell_threat)
# Produces a NoMethodError exception.
```

Q: In the code below, does <q1> print 9 or 0? Explain.

## ivar vs civar

```rb
class Foo
  @count = 0; # <e1>

  def initialize(num)
    @count  = num # <e2>
  end

  # <e3>
  def count
    @count
  end
end

foo = Foo.new(9);
p foo.count # <q1>
# → 9, not 0.
```

ANSWER:

It prints `0`.

A `@var` syntax used inside an instance method (`count` in this case) will _always_ refer to to an _instance variable_.

<e1> declares and initializes _civar_. It is an instance variable of the `Foo` object. (TODO: remember that classes are objects, instances of the Class class.)

Then, `initialize` creates the `@count` _ivar_. Now we have two variables with the same name. One is an _civar_, the other is an _ivar_.

In <e3>, we define a (reader) instance method `count` that simply returns `@count`. When that method is invoked, ruby has to decide whether to return the _ivar_ or the _civar_. Inside instance methods, a `@some_var` syntax will _always_ refer to an _ivar_.
