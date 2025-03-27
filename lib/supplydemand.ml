module SupplyDemandGraph = struct
  type line = {
    a : float;
    b : float;
    c : float;
  }
  (**[line] represents a linear line in the format of ax = by + c with three
     variables. Slope is equivalent to [a]/[b] with a coefficient of -[c]/[b]*)

  type graph = {
    supply : line;
    demand : line;
  }
  (**[graph] represents a simple supply-demand graph containing two [line]
     objects*)

  type changes = {
    cs : float;
    ps : float;
    shortage : float;
    dwl : float;
  }
  (**[changes] bundles analytics requested from the program into one data type.
  *)

  (* Function to convert an x-defined equation (ax = by + c) to y-defined (y =
     mx + b) *)
  let to_y_defined line =
    if line.b = 0.0 then
      failwith "Vertical line, cannot be expressed in y = mx + b form"
    else
      let m = line.a /. line.b in
      let b = line.c /. line.b *. -1.0 in
      { a = m; b = 1.0; c = b }

  (* Function to find the equilibrium point where supply meets demand *)
  let equilibrium graph =
    let supply, demand = (graph.supply, graph.demand) in
    let det = (supply.a *. demand.b) -. (demand.a *. supply.b) in
    if det = 0.0 then failwith "No unique equilibrium (parallel lines)"
    else
      let x = ((demand.c *. supply.b) -. (supply.c *. demand.b)) /. det in
      let y = ((supply.a *. demand.c) -. (demand.a *. supply.c)) /. det in
      (x, y)
  (* Returns equilibrium point (x, y) *)

  (* Function to compute consumer surplus *)
  let consumer_surplus graph =
    let equilibrium_quantity, equilibrium_price = equilibrium graph in
    let b = graph.demand.c in
    0.5 *. (b -. equilibrium_price) *. equilibrium_quantity

  (* Function to compute producer surplus *)
  let producer_surplus graph =
    let equilibrium_quantity, equilibrium_price = equilibrium graph in
    let b = graph.supply.c in
    0.5 *. (equilibrium_price -. b) *. equilibrium_quantity

  (**[price_ceiling] calculates the price ceiling of a graph *)
  let price_ceiling graph ceiling =
    let eq_qty, eq_price = equilibrium graph in
    if ceiling >= eq_price then
      {
        cs = consumer_surplus graph;
        ps = producer_surplus graph;
        shortage = 0.0;
        dwl = 0.0;
      }
    else
      let upper_point_graph =
        {
          supply = { a = 0.; b = 9999.0; c = -1. *. ceiling };
          demand = graph.demand;
        }
      in
      let upper_point, new_quantity = equilibrium upper_point_graph in
      let lower_point_graph =
        { supply = graph.supply; demand = { a = 1.0; b = 0.0; c = ceiling } }
      in
      let lower_point, _ = equilibrium lower_point_graph in
      let right_point, _ =
        equilibrium
          { supply = { a = 1.0; b = 0.0; c = ceiling }; demand = graph.demand }
      in
      let max_price_yint = graph.demand.c in
      let min_price_yint = graph.supply.c in

      let new_cs =
        (0.5 *. (max_price_yint -. upper_point) *. new_quantity)
        +. ((upper_point -. lower_point) *. new_quantity)
      in
      let new_ps = 0.5 *. (ceiling -. min_price_yint) *. new_quantity in
      let dwl =
        0.5 *. (eq_qty -. new_quantity) *. (upper_point -. lower_point)
      in

      { cs = new_cs; ps = new_ps; shortage = right_point -. lower_point; dwl }

  (**[price_floor] calculates the price ceiling of a graph *)
  let price_floor graph floor =
    let eq_qty, eq_price = equilibrium graph in
    if floor <= eq_price then
      {
        cs = consumer_surplus graph;
        ps = producer_surplus graph;
        shortage = 0.0;
        dwl = 0.0;
      }
    else
      let new_graph =
        { supply = { a = 1.0; b = 0.0; c = floor }; demand = graph.demand }
      in
      let new_qty, _ = equilibrium new_graph in
      let max_price = graph.demand.c in
      let min_price = graph.supply.c in

      let new_cs = 0.5 *. (max_price -. floor) *. new_qty in

      let ps_rectangle = (floor -. min_price) *. new_qty in
      let ps_lost_triangle =
        0.5 *. (eq_qty -. new_qty) *. (floor -. eq_price)
      in
      let new_ps = ps_rectangle +. ps_lost_triangle in

      let dwl = 0.5 *. (eq_qty -. new_qty) *. (floor -. eq_price) in

      { cs = new_cs; ps = new_ps; shortage = eq_qty -. new_qty; dwl }
end
