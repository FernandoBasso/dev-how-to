# Private, protected, public methods

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
