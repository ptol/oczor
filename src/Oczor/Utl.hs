{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Oczor.Utl (module Oczor.Utl, module Control.Arrow, module Control.Lens,  Fix(..), cata, para, (&)) where
import ClassyPrelude as C
import Control.Arrow
import Data.Function ((&))
import Text.Show.Pretty as PP
import Data.Functor.Foldable
import Data.Char as Char
import Control.Monad.Reader
import qualified Data.Foldable as F
import Control.Lens hiding (para, (&))

cataM
  :: (Monad f, Traversable (Base a), Recursive a) =>
     (Base a b -> f b) -> a -> f b
cataM f = (>>= f) . cata (traverse (>>= f))

eqLength x y = olength x == olength y

groupMapBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupMapBy f = map (f . headEx &&& id)
                   . groupBy ((==) `on` f)
                   . sortBy (compare `on` f)

concatNub x y = x ++ y & ordNub

toTitleCase :: String -> String
toTitleCase [] = []
toTitleCase (x : xs) = Char.toUpper x : xs

notNull x = not $ onull x

trace2 x y = C.trace x y
trace2 x y = y

ffold x = F.fold x

trac y x = trace2 ("\n\n" ++ y ++ ":\n " ++ show x ++ "\n") x
tracep y x = trace2 ("\n\n" ++ y ++ ":\n " ++ pshow x ++ "\n") x

traceArgs list = trace2 ("\n\n" ++ (unlines (zip [0..] list &map (\(i,x) -> ((if i > 0 then "  " ++ show i ++ ": " else "") ++ x))))) False
traceResult list = trace2 ("\n\n" ++ (unlines (zip [0..] list &map (\(i,x) -> ((if i > 0 then "  " ++ show i ++ ": " else "") ++ x))))) True
joinLines :: [String] -> String
joinLines = intercalate "\n"

pshow :: Show a => a -> String
pshow = PP.ppShow

unionMaps list = unionsWith (\_ y -> y) list

unsafeUnconsLast l = (unsafeInit l, unsafeLast l)

lookupEx key map = map & lookup key & headEx

t31 (x, _, _) = x
t32 (_, x, _) = x
t33 (_, _, x) = x

localPut x = local (const x)
