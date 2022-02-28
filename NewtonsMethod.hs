{-# LANGUAGE FlexibleContexts, FlexibleInstances,TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts#-}

import Prelude hiding ((+), (-), (*), (/), negate, recip, (^),pi , sin, cos, exp, fromInteger, fromRational)
import DSLsofMath.L.DSLsofMath.Algebra
import DSLsofMath.L.DSLsofMath.FunExp
import DSLsofMath.L.DSLsofMath.Algebra (AddGroup, MulGroup)

-- Daniel Nikolaev, Björn Solér, Axel Siwmark – Grupp 3

data Result a = Maxima R | Minima R | Dunno R
    deriving(Show)

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
        y'' = ((x+x)*x'*x' - x''*x*x)*y*y*y*y


instance Transcendental a => Transcendental (Tri a) where
    pi = piTri
    sin = sinTri
    cos = cosTri
    exp = expTri

piTri :: Transcendental a => Tri a
piTri = (pi, zero, zero)

sinTri :: Transcendental a => Tri a -> Tri a
sinTri (x,x',x'') = (sin x, cos x * x', negate (sin x) * x'* x' + cos x * x'')

cosTri ::Transcendental a => Tri a -> Tri a
cosTri (x, x', x'') = (cos x, negate (sin x) * x', negate (sin x) * x'' + negate (cos x) * x'* x')

expTri ::Transcendental a => Tri a -> Tri a
expTri (x, x', x'') = (exp x, exp x * x', exp x * x'*x' + exp x * x'')

pythagorean :: Transcendental a => Tri a -> Tri a
pythagorean = sin*sin+cos*cos

evalDD :: Transcendental a => FunExp -> FunTri a
evalDD f x =  (eval f x, eval (derive f) x, eval(derive (derive f)) x)

newton :: (Tri R -> Tri R) -> R -> R -> R
newton f e x    | abs fx < e = x                      -- Returns an x that is very close to a zero point for the function
                | fx' /= 0     = newton f e next      --else if
                | otherwise    = newton f e (x + e)   --else
    where
        fx   = fst' (f (var x))
        fx'  = snd'(f (var x)) -- should be f' x (derivative of f at x) 
        next = x - fx / fx'

test0 x = x*x-- one (double) zero, in zero
test1 x = x*x - one -- two zeros, in +-one
test2 x = sin x -- many, many zeros (in n * pi for all n ::Z)
--test3 n x y = y^n - constTri x -- test3 n x, has zero in "nth roots of x"

fst' :: Tri a -> a
fst' (a,_,_) = a

snd' :: Tri a  -> a
snd' (_,a,_) = a

trd' :: Tri a  -> a
trd' (_,_,a) = a

var :: (Additive a, Multiplicative a) => a -> Tri a
var x = (x, one, zero)

derTri :: (Tri a-> Tri a) -> (Tri a->Tri a)
derTri f = \x -> (snd'(f x), trd'(f x), fst'(f x)) 

optim :: (Tri R -> Tri R) -> R -> R -> Result R
optim f e x | f''x < 0   = Maxima (fst' (f (var xValue)))
            | f''x > 0   = Minima (fst' (f (var xValue)))
            | otherwise  = Dunno (fst' (f (var xValue)))
    where
        f' = derTri f
        xValue = newton f' e x
        f''x = trd' (f (var xValue))
