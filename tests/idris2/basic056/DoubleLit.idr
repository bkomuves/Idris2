import Data.So

record Newtype where
  constructor
  MkNewtype
  wrapped : Double

FromFloating Newtype where
  fromMantissaExpo me = MkNewtype $ fromMantissaExpo me

Show Newtype where
  showPrec p (MkNewtype v) = showCon p "MkNewtype" $ showArg v



record InUnit where
  constructor MkInUnit
  value      : Double
  0 inBounds : So (0 <= value && value <= 1)

Show InUnit where
  showPrec p (MkInUnit v _) = showCon p "MkInUnit" $ showArg v ++ " _"

mbInUnit : Double -> Maybe InUnit
mbInUnit v = case choose (0 <= v && v <= 1) of
  Left so => Just (MkInUnit v so)
  Right _ => Nothing

namespace InUnit
{-
  public export
  fromDouble : (v : Double) 
            -> {auto 0 prf : So (0 <= v && v <= 1)}
            -> InUnit
  fromDouble {prf} v  = MkInUnit v prf
-}

  public export
  fromMantissaExpo : (Integer,Int) -> Maybe InUnit
  fromMantissaExpo me = mbInUnit (mantissaExpoToDouble me)

record MyFloat where
  constructor MkMyFloat
  mantissa : Integer
  exponent : Int

Show MyFloat where show (MkMyFloat m e) = show (m,e)

implementation FromFloating MyFloat where
  fromMantissaExpo (m,e) = MkMyFloat m e


main : IO ()
main = do printLn $ the (Maybe InUnit)  0.25
          printLn $ the Newtype 123.456
          printLn $ the MyFloat 1234.567e-2
