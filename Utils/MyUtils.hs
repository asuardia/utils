
module Utils.MyUtils   
    ( 
     Result_ (..),
     split, flip123_312, eqtol, fst3, snd3, trd3, fst4, snd4, trd4, fth4,
     concatResult, sign, pass2EOM
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------   
    
import Control.Monad(liftM,ap)
import Control.Applicative
import Data.Time.Calendar 
import Data.Time.Calendar.MonthDay (monthLength)
 
--------------------------------------------------------------------------    
------------------------------ Result_ -----------------------------------
--------------------------------------------------------------------------    

-- | A type for parser results
data Result_ a = Ok_ a | Error_ String
  deriving (Eq,Show)

-- | Map Results to Eithers
resultToEither :: Result_ a -> Either String a
resultToEither (Ok_ a)    = Right a
resultToEither (Error_ s) = Left  s

instance Functor Result_ where fmap = liftM

instance Applicative Result_ where
  (<*>) = ap
  pure  = return

instance Alternative Result_ where
  Ok_ a <|> _    = Ok_ a
  _    <|> Ok_ b = Ok_ b
  err  <|> _    = err
  empty         = Error_ "empty"

instance Monad Result_ where
  return x      = Ok_ x
  fail x        = Error_ x
  Ok_ a >>= f    = f a
  Error_ x >>= _ = Error_ x 
    
concatResult :: [Result_ a] -> Result_ [a]
concatResult ras = foldl extract (Ok_ []) ras
    where extract ra rb = do
              a <- ra
              b <- rb
              return (b:a)
    
--------------------------------------------------------------------------    
----------------------- Util Functions -----------------------------------
--------------------------------------------------------------------------    
split :: String -> Char  -> [String]
split    []        delim =  [""]
split    (c:cs)    delim
   | c == delim          = "" : rest
   | otherwise           = (c : head rest) : tail rest
   where
         rest = split cs delim    
--------------------------------------------------------------------------
flip123_312 :: (a -> b -> c -> d) -> c -> a -> b -> d 
flip123_312    f                     z    x    y  = f x y z 
--------------------------------------------------------------------------
eqtol tol a b = tol > abs (a-b)
--------------------------------------------------------------------------
fst3 :: (a, b, c) -> a
fst3    (a, b, c) =  a
--------------------------------------------------------------------------
snd3 :: (a, b, c) -> b
snd3    (a, b, c) =  b
--------------------------------------------------------------------------
trd3 :: (a, b, c) -> c
trd3    (a, b, c) =  c

--------------------------------------------------------------------------
fst4 :: (a, b, c, d) -> a
fst4    (a, b, c, d) =  a
--------------------------------------------------------------------------
snd4 :: (a, b, c, d) -> b
snd4    (a, b, c, d) =  b
--------------------------------------------------------------------------
trd4 :: (a, b, c, d) -> c
trd4    (a, b, c, d) =  c
--------------------------------------------------------------------------
fth4 :: (a, b, c, d) -> d
fth4    (a, b, c, d) =  d
--------------------------------------------------------------------------
sign :: (RealFrac a, Integral b) => a -> b
sign n = round $ n / (abs n)
--------------------------------------------------------------------------
pass2EOM :: Day -> Day
pass2EOM dt = fromGregorian y m eom
    where (y,m,d) = toGregorian dt
          isLeap  = isLeapYear y
          eom     = monthLength isLeap m
