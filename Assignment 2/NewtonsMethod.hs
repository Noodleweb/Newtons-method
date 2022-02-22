{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module A2_Skeleton where
import Prelude hiding ((+), (-), (*), (/), negate, recip, (^),pi , sin, cos, exp, fromInteger, fromRational)
import DSLsofMath.Algebra
import DSLsofMath.FunExp
type Tri a = (a, a, a)
type TriFun a = Tri (a ! a) -- = (a ! a, a ! a, a ! a)
type FunTri a = a ! Tri a -- = a ! (a, a, a)

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
zeroTri = (0,0,0)

oneTri :: Additive a => Tri a
oneTri = (1,0,0)

mulTri :: Additive a => Tri a -> Tri a -> Tri a
mulTri = undefined

negateTri :: Additive a => Tri a -> Tri a -> Tri a
negateTri = undefined

recipTri :: Additive a => Tri a -> Tri a
recipTri = undefined



(addTri, zeroTri, mulTri, oneTri, negateTri, recipTri) = undefined



instance Transcendental a => Transcendental (Tri a) where
    pi = piTr
    sin = sinTri
    cos = cosTri
    exp = expTr

(piTri, sinTri, cosTri, expTri) = undefined