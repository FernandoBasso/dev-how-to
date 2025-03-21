module Blackjack
  CARDS = %w[
    two
    three
    four
    five
    six
    seven
    eight
    nine
    ten
  ].map.with_index { |e, i| [e, i + 2] }.to_h.merge({
    'jack' => 10,
    'king' => 10,
    'queen' => 10,
    'ace' => 11,
  })

  def self.parse_card(card)
    CARDS[card] || 0
  end

  def self.card_range(card1, card2)
    case parse_card(card1) + parse_card(card2)
    when (4..11) then 'low'
    when (12..16) then 'mid'
    when (17..20) then 'high'
    when 21 then 'blackjack'
    else 'Sum is below < 4 or > 21'
    end
  end

  def self.first_turn(card1, card2, dealer_card)
    return 'P' if [card1, card2].all? { |c| c == "ace" }

    range = card_range(card1, card2)

    return 'S' if range == 'high'
    return 'H' if range == 'low'

    if range == 'blackjack'
      return ['ace'].member?(dealer_card) ? 'S' : 'W'
    end

    if range == 'mid'
      CARDS[dealer_card] < 7 ? 'S' : 'H'
    end
  end
end
