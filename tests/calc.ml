let s = fun x -> fun y -> fun z -> x z (y z)
let k = fun x -> fun y -> x
let i = fun x -> x
let i' = i i
let k' = k k i
let k'' = s k s k
