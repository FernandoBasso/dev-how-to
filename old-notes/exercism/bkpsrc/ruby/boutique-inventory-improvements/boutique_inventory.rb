require 'ostruct'
class BoutiqueInventory
  attr_reader :items

  def initialize(items)
    @items = items.collect { |item| OpenStruct.new(item) }
  end

  def item_names
    items.collect(&:name).sort
  end

  def total_stock
    items.map(&:quantity_by_size).flat_map(&:values).sum
  end
end
