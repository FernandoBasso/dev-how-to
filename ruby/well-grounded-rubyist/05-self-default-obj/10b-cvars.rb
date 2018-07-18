class Foo
  @@z = 'a cvar'
  @z = 'a civar'

  # A class method (which is a method on the object Foo in this
  # example). It can access both cvars and civars.
  def self.print_vars
    p @@z # works from class method.
    p @z # works from class method.
  end

  # An instance method. It can access cvars and ivars, but not civars. Note
  # that inside this instance method @z is not the same as the previous @z
  # defined at the toplevel of the class.
  def print_vars
    p @@z # works from instance method.
    p @z #
  end
end

Foo.print_vars
Foo.new.print_vars

#
# cvars are visible from:
#   - the toplevel of the class
#   - inside class methods
#   - inside instance methods
#
