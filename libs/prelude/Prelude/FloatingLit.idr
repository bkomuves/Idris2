
-- floating point literals

module Prelude.FloatingLit

import Builtin
import Prelude.Basics
-- import Prelude.Num
-- import Prelude.EqOrd

%default total

||| Convert a floating point literal to Double.
public export
mantissaExpoToDouble : (Integer,Int) -> Double
mantissaExpoToDouble (mantissa,expo) = prim__cast_StringDouble
    (prim__cast_IntegerString mantissa ++ "e" ++ prim__cast_IntString expo) 
  where
    infixr 7 ++
    (++) : String -> String -> String
    x ++ y = prim__strAppend x y

{- an alternative implementation:

-- 10^n
integerExp10 : Int -> Integer
integerExp10 n = worker n where

  worker : Int -> Integer
  worker n = assert_total $ if n <= 0 
    then 1
    else  let m  = div n 2 
              a  = worker m
              aa = a * a 
              m2 = m + m 
          in  case m2 < n of 
                 True  => 10*aa 
                 False => aa

-- 10^n
doubleExp10 : Int -> Double
doubleExp10 expo = prim__cast_IntegerDouble (integerExp10 expo)

||| Convert a floating point literal to Double.
export
mantissaExpoToDouble : (Integer,Int) -> Double
mantissaExpoToDouble (mantissa,expo) = case compare expo 0 of
  EQ => prim__cast_IntegerDouble mantissa
  GT => prim__mul_Double (prim__cast_IntegerDouble mantissa) (doubleExp10 expo)
  LT => assert_total (
          prim__div_Double (prim__cast_IntegerDouble mantissa) (doubleExp10 (negate expo)) )

-}

%floatingLit fromMantissaExpo

||| Interface for types that can be constructed from double literals.
public export
interface FromFloating ty where
  constructor MkFromFloating
  ||| Conversion from mantissa and exponent
  total
  fromMantissaExpo : (Integer,Int) -> ty

%allow_overloads fromMantissaExpo

%inline
public export
FromFloating Double where
  fromMantissaExpo = mantissaExpoToDouble

%defaulthint
%inline
public export
defaultFloating : FromFloating Double
defaultFloating = %search

{-

||| Floating point literals, represented as an Integer mantissa and an Int exponent.
public export
record FloatingLit where
  constructor MkFloatingLit
  mantissa : Integer
  exponent : Int

||| Convert a floating point literal to Double.
export
floatingLitToDouble : FloatingLit -> Double
floatingLitToDouble (MkFloatingLit mantissa expo) = mantissaExpoToDouble (mantissa,expo)

||| Conversion from FloatingLit
total
export
fromFloatingLit : FromFloatingLit ty => FloatingLit -> ty
fromFloatingLit (MkFloatingLit m e) = fromMantissaExpo (m,e)

-}
