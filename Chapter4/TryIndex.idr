import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         Just idx => Just $ index idx xs

vecTake : (n : Nat) -> (vec : Vect (n + m) a) -> Vect n a
vecTake Z vec = []
vecTake (S k) (x :: xs) = x :: vecTake k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just idx => Just $ index idx xs + index idx ys
