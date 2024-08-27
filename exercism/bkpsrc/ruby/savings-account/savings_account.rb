module SavingsAccount
  class << self
    def interest_rate(balance)
      case balance
      when (0...1000)
        0.5
      when (1000...5000)
        1.621
      when (5000..)
        2.475
      else
        3.213
      end
    end

    def annual_balance_update(balance)
      interest = balance * (interest_rate(balance) / 100)
      balance + interest
    end

    def years_before_desired_balance(current_balance, desired_balance)
      years = 0

      while current_balance < desired_balance
        current_balance = annual_balance_update(current_balance)
        years += 1
      end

      ##
      # `years += 1` above does not return the new value so the entire
      # `while` returns `nil`, therefore, we need to return `years`
      # after the loop explicitly.
      #
      years
    end
  end
end
