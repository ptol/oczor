{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Oczor.Utl (module Oczor.Utl, module Control.Arrow, module Control.Lens,  Fix(..), refix, cata, embed, project, para, (&)) where
import ClassyPrelude as C
import Control.Arrow
import Data.Function ((&), on)
import qualified Data.Map as M
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

eqLength = (==) `on` olength

groupMapBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupMapBy f = M.toList . M.fromListWith (\b [a]-> a : b) . map (f &&& (:[]))

concatNub x y = x ++ y & ordNub

toTitleCase :: String -> String
toTitleCase [] = []
toTitleCase (x : xs) = Char.toUpper x : xs

notNull = not . onull

-- trace2 x y = C.trace x y
trace2 x y = y

ffold = F.fold

trac y x = trace2 ("\n\n" ++ y ++ ":\n " ++ show x ++ "\n") x
tracep y x = trace2 ("\n\n" ++ y ++ ":\n " ++ pshow x ++ "\n") x

traceArgs list = trace2 ("\n\n" ++ unlines (zip [0..] list &map (\(i,x) -> ((if i > 0 then "  " ++ show i ++ ": " else "") ++ x)))) False
traceResult list = trace2 ("\n\n" ++ unlines (zip [0..] list &map (\(i,x) -> ((if i > 0 then "  " ++ show i ++ ": " else "") ++ x)))) True
joinLines :: [String] -> String
joinLines = intercalate "\n"

pshow :: Show a => a -> String
pshow = PP.ppShow

unionMaps = unions . reverse

unsafeUnconsLast l = (unsafeInit l, unsafeLast l)

lookupEx key = headEx . lookup key

t31 (x, _, _) = x
t32 (_, x, _) = x
t33 (_, _, x) = x

localPut x = local (const x)
