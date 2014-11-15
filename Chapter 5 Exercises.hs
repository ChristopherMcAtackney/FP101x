let sumOfSquares = sum[x^2 | x <- [1..100]]
let replicate x x' = [x' | _ <- [1..x]]
let pyths n = [(x,y,z) | x <- [1..n],y <- [1..n],z <- [1..n], (x^2 + y^2) = z^2]

let scalarProduct xs xs' = sum[a * b | (a,b) <- zip xs xs']