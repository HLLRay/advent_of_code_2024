open Core

let ( +: ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let ( ~: ) (x, y) = (-x, -y)
let ( -: ) v1 v2 = v1 +: ~:v2
let ( @: ) alpha (x, y) = (alpha * x, alpha * y)
let ( /: ) alpha (x, y) = (x / alpha, y / alpha)
let ( <:> ) (x1, y1) (x2, y2) = (x1 * y1) + (x2 * y2)

let ( *: ) (a11, a12, a21, a22) (x, y) =
  ((a11 * x) + (a12 * y), (a21 * x) + (a22 * y))

let v_in_grid (grid : 'a Array.t Array.t) ((i, j) : int * int) : bool =
  let max_i = Array.length grid in
  let max_j = if max_i > 0 then Array.length grid.(0) else 0 in
  0 <= i && i < max_i && 0 <= j && j < max_j

let orth_neighbours (i, j) : (int * int) list =
  [ (i - 1, j); (i, j + 1); (i + 1, j); (i, j - 1) ]

let orth_neighbours_in_grid (grid : 'a Array.t Array.t) (pos : int * int) :
    (int * int) list =
  List.filter ~f:(v_in_grid grid) (orth_neighbours pos)

let singular (a, b, c, d) = (a * d) - (b * c) = 0
let colinear (x1, y1) (x2, y2) = singular (x1, x2, y1, y2)

module IntPair = struct
  module T = struct
    type t = int * int

    let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
  include Tuple.Hashable_t (Int) (Int)
end
