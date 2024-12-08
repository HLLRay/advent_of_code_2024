let ( +: ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let ( ~: ) (x, y) = (-x, -y)
let ( -: ) v1 v2 = v1 +: ~:v2
let ( @: ) alpha (x, y) = (alpha * x, alpha * y)
let ( /: ) alpha (x, y) = (x / alpha, y / alpha)
let ( <:> ) (x1, y1) (x2, y2) = (x1 * y1) + (x2 * y2)

let v_in_grid grid (i, j) =
  let max_i = Array.length grid in
  let max_j = if max_i > 0 then Array.length grid.(0) else 0 in
  0 <= i && i < max_i && 0 <= j && j < max_j
