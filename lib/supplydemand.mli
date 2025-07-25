module SupplyDemandGraph : sig
  (* type point = { x : float; y : float; } *)

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

  val to_y_defined : line -> line
  val equilibrium : graph -> float * float
  val consumer_surplus : graph -> float
  val producer_surplus : graph -> float

  val price_ceiling : graph -> float -> changes
  (**[price_ceiling]first float represents new CS, second float represents new
     PS, third represents DWL*)

  val price_floor : graph -> float -> changes
  (**[x_on_a_line] given a x-coordinate, returns the point that has this x-coordinate on the graph*)
  val x_on_a_line : float -> line -> float * float
end
