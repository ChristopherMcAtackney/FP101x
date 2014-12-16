import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
		 deriving Show
		 
		 
natToInteger :: Nat -> Integer
natToInteger = \n -> genericLength [c | c <- show n, c == 'S']

add :: Nat -> Nat -> Nat

add Zero n = n
add (Succ m) n = Succ (add n m)

mult :: Nat -> Nat -> Nat

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

data Tree = Leaf Integer
          | Node Tree Tree
		  
leaves (Leaf _) = True
leaves (Node l r) = leaves l + leaves r