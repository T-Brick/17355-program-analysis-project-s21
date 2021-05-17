let id = fun x -> x
let f = fun y -> if (100 + (id y) > 100) then 1 else 0
let a = f 1
let b = f 0
let g = fun y -> if a = 1 then not(true) else not(false)
let t = id true
let h = let y = 100 in
  let x = 49 in
  y + a + x + b
let f' = fun x -> let y = 100 in x + y
