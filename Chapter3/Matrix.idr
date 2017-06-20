import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties


transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                            zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

multMatrixHelper : Num t => (xs : Vect n (Vect m t))
                         -> (ysTrans : Vect p (Vect m t))
                         -> Vect n (Vect p t)
multMatrixHelper [] ysTrans = []
multMatrixHelper (x :: xs) ysTrans = let topRow = multAll x ysTrans in
                                        topRow :: multMatrixHelper xs ysTrans
    where
        dotProd : Num t => Vect n t -> Vect n t -> t
        dotProd xs ys = sum (zipWith (*) xs ys)

        multAll : Num t => Vect n t -> Vect m (Vect n t) -> Vect m t
        multAll xs yss = map (dotProd xs) yss

multMatrix : Num t =>
             Vect n (Vect m t) -> Vect m (Vect p t) -> Vect n (Vect p t)
multMatrix xs ys = let ysTrans = transposeMat ys in
                multMatrixHelper xs ysTrans
