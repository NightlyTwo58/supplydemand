open Supplydemand

(* dune exec -- supplydemand_app 1.0 -4.0 2000.0 1.0 6.0 -1200.0 *)
(* dune exec -- supplydemand_app *)

let () =
  print_endline "Enter supply line coefficient (a) for ax = by + c.";
  let supply_a = float_of_string (read_line ()) in
  print_endline "Enter supply line coefficient (b) for ax = by + c.";
  let supply_b = float_of_string (read_line ()) in
  print_endline "Enter supply line coefficient (c) for ax = by + c.";
  let supply_c = float_of_string (read_line ()) in
  print_endline "Enter demand line coefficient (d) for dx = ey + f.";
  let demand_a = float_of_string (read_line ()) in
  print_endline "Enter demand line coefficient (e) for dx = ey + f.";
  let demand_b = float_of_string (read_line ()) in
  print_endline "Enter demand line coefficient (f) for dx = ey + f.";
  let demand_c = float_of_string (read_line ()) in

  let supply =
    SupplyDemandGraph.to_y_defined
      { SupplyDemandGraph.a = supply_a; b = supply_b; c = supply_c }
  in
  let demand =
    SupplyDemandGraph.to_y_defined
      { SupplyDemandGraph.a = demand_a; b = demand_b; c = demand_c }
  in
  let graph = { SupplyDemandGraph.supply; demand } in

  let eq_x, eq_y = SupplyDemandGraph.equilibrium graph in
  let slope_supply, intercept_supply = (supply.a, supply.c) in
  let slope_demand, intercept_demand = (demand.a, demand.c) in
  Printf.printf "Supply equation in y-form: y = %.2fx + %.2f\n" slope_supply
    intercept_supply;
  Printf.printf "Demand equation in y-form: y = %.2fx + %.2f\n" slope_demand
    intercept_demand;
  let consumer_surplus = SupplyDemandGraph.consumer_surplus graph in
  let producer_surplus = SupplyDemandGraph.producer_surplus graph in
  Printf.printf "Equilibrium point: (%.2f, %.2f)\n" eq_x eq_y;
  Printf.printf "Consumer surplus: %.2f\n" consumer_surplus;
  Printf.printf "Producer surplus: %.2f\n" producer_surplus;

  print_endline "Enter mode: floor or ceiling";
  let restriction_mode = read_line () in
  print_endline "Enter restriction price (g):";
  let g = float_of_string (read_line ()) in
  if restriction_mode = "floor" then
    let floor = SupplyDemandGraph.price_floor graph g in
    print_endline
      ("The applied CS is " ^ string_of_float floor.cs ^ ", \n  the PS is "
     ^ string_of_float floor.ps ^ ", the surplus is "
      ^ string_of_float floor.shortage
      ^ ", the dwl is " ^ string_of_float floor.dwl)
  else
    let ceiling = SupplyDemandGraph.price_ceiling graph g in
    print_endline
      ("The applied CS is " ^ string_of_float ceiling.cs ^ ", \n  the PS is "
     ^ string_of_float ceiling.ps ^ ", the shortage is "
      ^ string_of_float ceiling.shortage
      ^ ", the dwl is "
      ^ string_of_float ceiling.dwl)
