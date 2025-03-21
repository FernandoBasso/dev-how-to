class LocomotiveEngineer
  def self.generate_list_of_wagons(*vagon_ids)
    vagon_ids
  end

  def self.fix_list_of_wagons(each_wagons_id, missing_wagons)
    first, second, *rest_of_each_wagons_id = each_wagons_id
    locomotive_id, *rest_after_locomotive_id = rest_of_each_wagons_id
    [locomotive_id, *missing_wagons, *rest_after_locomotive_id, first, second]
  end

  def self.add_missing_stops
    raise 'Please implement the LocomotiveEngineer.add_missing_stops method'
  end

  def self.extend_route_information(route, more_route_information)
    raise 'Please implement the LocomotiveEngineer.extend_route_information method'
  end
end
