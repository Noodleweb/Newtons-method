{-# LANGUAGE FlexibleContexts, FlexibleInstances,TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts#-}

import Prelude hiding ((+), (-), (*), (/), negate, recip, (^),pi , sin, cos, exp, fromInteger, fromRational)
import DSLsofMath.L.DSLsofMath.Algebra
import DSLsofMath.L.DSLsofMath.FunExp
import DSLsofMath.L.DSLsofMath.Algebra (AddGroup, MulGroup)

type Tri a = (a, a, a)
type TriFun a = Tri (a -> a) -- = (a -> a, a ->  a, a -> a)
type FunTri a = a -> Tri a -- = a -> (a, a, a)

type R = Double

instance (Additive a ) => Additive (Tri a) where
    (+) = addTri
    zero = zeroTri

instance (Additive a, Multiplicative a) => Multiplicative (Tri a) where
    (*) = mulTri; one = oneTri

instance (AddGroup a) => AddGroup (Tri a) where
    negate = negateTri

instance (AddGroup a, MulGroup a) => MulGroup (Tri a) where
    recip = recipTri

addTri :: Additive a => Tri a -> Tri a -> Tri a
addTri (x,x',x'') (y,y',y'') = (x + y,x' + y',x'' + y'')

zeroTri :: Additive a => Tri a
zeroTri = (zero,zero,zero)

oneTri :: (Additive a, Multiplicative a) => Tri a
oneTri = (one,zero,zero)

mulTri :: (Multiplicative a, Additive a) => Tri a -> Tri a -> Tri a
mulTri (x,x',x'') (y,y',y'') = (x*y,x*y' + x'*y, x*y'' + x'*y' + x'*y' + x''*y)

negateTri :: AddGroup a  => Tri a -> Tri a
negateTri (x,x',x'') = (neg x,neg x',neg x'')

recipTri :: (AddGroup a, MulGroup a) => Tri a -> Tri a
recipTri (x,x',x'') = (y, y', y'')
  where
        y = recip x
        y' = negate (y*y) * x'
        y'' = (x'*x*x - x''*x*x)*y*y*y*y

--(addTri, zeroTri, mulTri, oneTri, negateTri, recipTri) = undefined

instance Transcendental a => Transcendental (Tri a) where
    pi = piTri
    sin = sinTri
    cos = cosTri
    exp = expTri

piTri :: Transcendental a => Tri a
piTri = (pi, zero, zero)

sinTri :: Transcendental a => Tri a -> Tri a
sinTri (x,x',x'') = (sin x, cos x * x', negate (sin x) * x' + cos x * x'')

cosTri ::Transcendental a => Tri a -> Tri a
cosTri (x, x', x'') = (cos x, negate (sin x) * x', negate (sin x) * x'' + negate (cos x) * x')

expTri ::Transcendental a => Tri a -> Tri a
expTri (x, x', x'') = (exp x, exp x * x', exp x * x' + exp x * x'')

--test1 :: (Transcendental a, Additive a) => Tri a -> Tri a -> Bool
--test1 t1 t2 = undefined
-- a -> (a, a, a) /= (a -> a , a -> a , a -> a )
--evalDD :: Transcendental a => FunExp -> FunTri a
--evalDD expression = \x -> (eval x, eval (derive x), eval (derive (derive x)))
--  where 


step1 :: Transcendental a => FunExp -> TriFun a
step1 f =  (eval f , eval (derive f),  eval (derive (derive f)))

step2 :: Transcendental a => TriFun a -> FunTri a
step2 (a,b,c) x = (a x, b x, c x)

evalDD :: Transcendental a => FunExp -> FunTri a
evalDD = step2 . step1

-- Group homorphism 
-- (evalDD, (*), ('*'))
-- evalDD (Mul f g) = mulTri (evalDD f) (evalDD g)



        -- (a,a,a) -> (a,a,a)
newton :: (Tri R -> Tri R) -> R -> R -> R
newton f e x    | abs fx < e = x
                | fx0 /= 0     = newton f e next
                | otherwise    = newton f e (x + e)
    where 
        (fx, fx', fx'') = f (x, x, x)
        fx0 = fx' -- should be f' x (derivative of f at x) 
        next = x - fx / fx'

test0 x = x*x-- one (double) zero, in zero
test1 x = x*x - one -- two zeros, in +-one
test2 x = sin x -- many, many zeros (in n  p for all n ::Z)
--test3 n x y = y^n - constTri x -- test3 n x, has zero in "nth roots of x"

--map (newton test1 0.001) [-2.0, −1.5..2.0]

fst' :: (a,a,a) -> a
fst' (a,_,_) = a

snd' :: (a,a,a) -> a
snd' (_,a,_) = a

trd' :: (a,a,a) -> a
trd' (_,_,a) = a

power :: Int -> Int -> Int
power n k | k < 0 = error "power: negative argument"
            | k == 0 = 1
            | otherwise = n * power n (k-1)