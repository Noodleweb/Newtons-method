{-# LANGUAGE FlexibleContexts, FlexibleInstances,TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances#-}

import Prelude hiding ((+), (-), (*), (/), negate, recip, (^),pi , sin, cos, exp, fromInteger, fromRational)
import DSLsofMath.L.DSLsofMath.Algebra
import DSLsofMath.L.DSLsofMath.FunExp 

type Tri a = (a, a, a)
type TriFun a = Tri (a -> a) -- = (a -> a, a ->  a, a -> a)
type FunTri a = a -> Tri a -- = a -> (a, a, a)

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
addTri (a1,a2,a3) (a1',a2',a3') = (a1+a1',a2+a2',a3+a3') 

zeroTri :: Additive a => Tri a
zeroTri = (zero,zero,zero)

oneTri :: (Additive a, Multiplicative a) => Tri a
oneTri = (one,one,one)

mulTri :: Additive a => Tri a -> Tri a -> Tri a
mulTri = undefined

negateTri :: Additive a => Tri a -> Tri a 
negateTri = undefined

recipTri :: Additive a => Tri a -> Tri a
recipTri = undefined

--(addTri, zeroTri, mulTri, oneTri, negateTri, recipTri) = undefined

instance Transcendental a => Transcendental (Tri a) where
    pi = piTri
    sin = sinTri
    cos = cosTri
    exp = expTri

piTri :: Transcendental a => Tri a
piTri = undefined 

sinTri :: Transcendental a => Tri a -> Tri a
sinTri = undefined 

cosTri ::Transcendental a => Tri a -> Tri a
cosTri = undefined 

expTri ::Transcendental a => Tri a -> Tri a
expTri = undefined 
