# Methods

- [private methods are not really private](#private-methods-are-not-really-private)

## private methods are not really private

```rb
class Enemy
  def name=(name)
    p 'name='
    p self
    @name = name
  end

  def yell_name
    p "yell_name: #{self}"
    yell
  end

  private

    def yell
      p "yell: #{self}"
      @name.upcase
    end
end

enemy1 = Enemy.new
enemy1.name = 'Vader'
p enemy1.yell_name

# But this is possible.
p enemy1.send(:yell)
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
