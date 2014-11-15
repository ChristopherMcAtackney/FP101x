--riffle xs ys = concat [[x,y]| x<-xs, y<-ys]
riffle xs ys = concat [[x,y]| (x,y) <- xs `zip` ys]
--riffle xs ys = [x,y | (x,y) <- xs `zip` ys]
--riffle xs ys = [x:[y] | x<-xs, y<-ys]