class Title
  def initialize(text)
    @text = text
  end

  def to_s
    @text
  end

  def +@
    @text.to_s
  end
end

title = 'May the force be with you!'
p (+title)


#
# Without parenthesis around ~+title~, we get a warning about ambiguity.
#
