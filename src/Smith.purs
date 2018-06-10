module Smith (smith, parseAndProcess) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, pureST)
import Data.Array.ST (STArray, freeze, peekSTArray, pokeSTArray, thaw)
import Data.List (List(..), filter, foldl, (..))
import Prelude (class Eq, class EuclideanRing, class Functor, class Ring, class Semiring, class Show, Unit, append, bind, degree, discard, div, map, min, mod, negate, one, otherwise, pure, show, unit, zero, ($), (&&), (*), (+), (-), (/=), (<), (<$>), (<*>), (<<<), (<>), (==), (>), (>=))
import Data.Array as A
import Data.Foldable (sequence_,   indexl, class Foldable, and)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Utils (words, lines)
import Data.Traversable (traverse)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Data.Int (fromString)


type Matrix a = {nrows :: Int, ncols :: Int, entries :: Array a}


matrixToTexInner :: forall a. Show a => Matrix a -> String
matrixToTexInner {nrows: n, ncols:m, entries:arr}
  = (foldl f {count: 0, str: ""} arr).str
    where f {count:i, str} x | i==(m-1)    = {count: 0, str: str <> (show x) <> "\\\\"}
                             |otherwise    = {count:i+1, str: str <> (show x) <> "&"}


matrixToTex :: Matrix Int -> String
matrixToTex m = "\\left(\\begin{matrix}" <> (matrixToTexInner m) <> "\\end{matrix}\\right)"

smithToTex :: Matrix Int -> String
smithToTex m = let s = smith m in
                foldl (<>) ""
                ["$$", matrixToTex m, "=", matrixToTex s.b2b, matrixToTex s.a2b2, matrixToTex s.aa2, "$$",
                 "$${",matrixToTex s.b2b, "}^{-1}=", matrixToTex s.bb2, "$$",
                 "$${",matrixToTex s.aa2, "}^{-1}=", matrixToTex s.a2a, "$$"]




parseAndProcess :: String -> String
parseAndProcess str = case parseMatrix str of
                       Nothing -> "No valid matrix!"
                       Just m -> smithToTex m



allEqual :: forall a f . Eq a => Functor f => Foldable f => f a -> Boolean
allEqual xs = case indexl 0 xs of
                Nothing -> false
                Just x -> and (map (x == _) xs)


parseMatrix :: String -> Maybe (Matrix Int)
parseMatrix str = do arr2 <- (traverse (traverse fromString)) (words <$> lines str)
                     row <- indexl 0 arr2
                     if allEqual (A.length <$> arr2)
                       then Just {nrows: A.length arr2, ncols: A.length row, entries: A.concat arr2}
                       else Nothing










array :: forall a .  Int -> (Int -> a) -> Array a
array n f = A.fromFoldable $ map f (rangeAsc 0 (n-1))


matrix :: forall a . Int -> Int -> (Int -> Int -> a) -> Matrix a
matrix n m f = {ncols : m, 
                nrows : n, 
                entries : array (n*m) (\k -> f (k`div`m) (k`mod` m))}



multiply :: forall a . Semiring a => Matrix a -> Matrix a -> Maybe (Matrix a)
multiply m1 m2 | m1.ncols /= m2.nrows     = Nothing
               | otherwise                = unsafePartial $ Just $ matrix m1.nrows m2.ncols
                                             (\i j -> foldl (\n k -> n + unsafeEntry i k m1 * unsafeEntry k j m2) zero (rangeAsc 0 (m1.ncols-1)))

unsafeMultiply :: forall a . Semiring a => Partial => Matrix a -> Matrix a -> Matrix a
unsafeMultiply m1 m2 = fromJust $ multiply m1 m2



entry :: forall a . Int -> Int -> Matrix a -> Maybe a
entry i j m = m.entries `A.index` (i*m.ncols + j)

unsafeEntry :: forall a . Partial => Int -> Int -> Matrix a -> a
unsafeEntry i j m = fromJust $ entry i j m

at :: forall a . Matrix a -> {row :: Int, col :: Int} -> Maybe a
at m ix = entry ix.row ix.col m


infixl 8 at as !!! 


identity :: forall a . Semiring a => Int -> Matrix a 
identity n = matrix n n (\i j -> if i==j then one else zero)




type STMatrix h a = {nrows :: Int, ncols :: Int, entries :: STArray h a}

thawMatrix :: forall a h r . Matrix a -> Eff (st :: ST h | r) (STMatrix h a)
thawMatrix m = do arr <- thaw m.entries
                  pure {nrows : m.nrows, ncols : m.ncols, entries : arr}


freezeMatrix :: forall a h r . STMatrix h a -> Eff (st :: ST h | r) (Matrix a)
freezeMatrix m = do arr <- freeze m.entries
                    pure {nrows : m.nrows, ncols : m.ncols, entries : arr}

peekMatrix :: forall a h r . Int -> Int -> STMatrix h a -> Eff (st :: ST h | r) (Maybe a)
peekMatrix i j m = peekSTArray m.entries (j + i*m.ncols)

unsafePeekMatrix :: forall a h r . Partial => Int -> Int -> STMatrix h a -> Eff (st :: ST h | r) a
unsafePeekMatrix i j m = do x <- peekMatrix i j m
                            case x of
                              Just val -> pure val
                              Nothing  -> crashWith (foldl append "" ["Index: (", show i, ",", show j, "), dimensions: ", show m.nrows, ", ", show m.ncols, ")."])

pokeMatrix :: forall a h r . Int -> Int -> a -> STMatrix h a -> Eff (st :: ST h | r) Unit
pokeMatrix i j x m = do _ <- pokeSTArray m.entries (j + i*m.ncols) x
                        pure unit


identityST :: forall a h r . Semiring a => Int -> Eff (st :: ST h | r) (STMatrix h a)
identityST = thawMatrix <<< identity




initSmith :: forall a h r . Semiring a => Matrix a -> Eff (st :: ST h | r) (SmithST h a)
initSmith m = do idaa2 <- identityST m.ncols
                 ida2a <- identityST m.ncols
                 idbb2 <- identityST m.nrows
                 idb2b <- identityST m.nrows
                 mata2b2 <- thawMatrix m
                 pure {b2b : idb2b, bb2 : idbb2, a2a : ida2a, aa2 : idaa2, a2b2 : mata2b2}




addRowST :: forall a h r . Semiring a => Int -> Int -> a -> STMatrix h a -> Eff (st :: ST h | r) Unit
addRowST i0 i1 x m = do _<- traverse (addRowSTJ m.entries) (rangeAsc 0 (m.ncols-1))  --todo: This is wrong for dimension 0!!
                        pure unit
                where addRowSTJ arr j = do a <- peekSTArray arr (j + i0*m.ncols)
                                           b <- peekSTArray arr (j + i1*m.ncols)
                                           sequence_ ((\xi0j xi1j -> pokeSTArray arr (j + i1*m.ncols) (xi1j + xi0j * x)) <$> a <*> b)



addColST :: forall a h r . Semiring a => Int -> Int -> a -> STMatrix h a -> Eff (st :: ST h | r) Unit
addColST j0 j1 x m = do _ <- traverse (addColSTI m.entries) (0..(m.nrows-1))
                        pure unit
                where addColSTI arr i = do a <- peekSTArray arr (j0 + i*m.ncols)
                                           b <- peekSTArray arr (j1 + i*m.ncols)
                                           sequence_ $ ((\xij0 xij1 -> pokeSTArray arr (j1 + i*m.ncols) (xij1 + xij0 * x)) <$> a <*> b)








type SmithST h a = {aa2 :: STMatrix h a , a2a :: STMatrix h a, bb2 :: STMatrix h a, b2b :: STMatrix h a, a2b2 :: STMatrix h a}
                            
type Smith a = {aa2 :: Matrix a , a2a :: Matrix a, bb2 :: Matrix a, b2b :: Matrix a, a2b2 :: Matrix a}





freezeSmith :: forall a h r . SmithST h a -> Eff (st :: ST h | r) (Smith a)
freezeSmith s = do aa2 <- freezeMatrix s.aa2   --TODO: use unsafeFreeze (we are not exporting the SmithST anywhere)
                   bb2 <- freezeMatrix s.bb2
                   a2a <- freezeMatrix s.a2a
                   b2b <- freezeMatrix s.b2b
                   a2b2 <- freezeMatrix s.a2b2
                   pure {aa2 : aa2, bb2 : bb2, a2a : a2a, b2b : b2b, a2b2 : a2b2}

addRowSmith :: forall a h r . Ring a => Show a=>  Int -> Int -> a -> SmithST h a -> Eff (st :: ST h | r) Unit
addRowSmith i0 i1 x s = do --_ <- traceAnyM (foldl append "" ["addRowSmith ", show i0, " ", show i1, " ", show x])
                           addRowST i0 i1 x s.a2b2
                           addRowST i0 i1 x s.bb2
                           addColST i1 i0 (-x) s.b2b

addColSmith :: forall a h r . Ring a => Show a => Int -> Int -> a -> SmithST h a -> Eff (st :: ST h | r) Unit
addColSmith j0 j1 x s = do --_ <- traceAnyM (foldl append "" ["addColSmith ", show j0, " ", show j1, " ", show x])
                           addColST j0 j1 x s.a2b2
                           addColST j0 j1 x s.a2a
                           addRowST j1 j0 (-x) s.aa2

swapRowSmith :: forall a h r . Ring a => Show a => Int -> Int -> SmithST h a -> Eff (st :: ST h | r) Unit
swapRowSmith i0 i1 s | i0 /= i1
                     = do addRowSmith i0 i1 one s
                          addRowSmith i1 i0 (-one) s
                          addRowSmith i0 i1 one s
                     | otherwise
                     = pure unit

swapColSmith :: forall a h r . Ring a => Show a => Int -> Int -> SmithST h a -> Eff (st :: ST h | r) Unit
swapColSmith j0 j1 s | j0 /= j1
                     = do addColSmith j0 j1 one s
                          addColSmith j1 j0 (-one) s
                          addColSmith j0 j1 one s
                     | otherwise
                     = pure unit


rangeAsc :: Int -> Int -> List Int
rangeAsc n m | n>m = Nil
             |otherwise = Cons n (rangeAsc (n+1) m)

rangeWithout :: Int -> Int -> Int -> List Int
rangeWithout l u n = filter (_ /= n) (rangeAsc l u)


reduceRow :: forall a h r . Partial => EuclideanRing a => Show a => Int -> Int -> SmithST h a -> Eff (st :: ST h | r) Unit
reduceRow i j s = do x <- unsafePeekMatrix i j s.a2b2
                     _ <-traverse (\j1 -> do x1 <- unsafePeekMatrix i j1 s.a2b2
                                             addColSmith j j1 (-(x1 `div` x)) s) (rangeWithout 0 (s.a2b2.ncols-1) j)
                     pure unit

reduceCol :: forall a h r . Partial => EuclideanRing a => Show a => Int -> Int -> SmithST h a -> Eff (st :: ST h | r) Unit
reduceCol i j s = do x <- unsafePeekMatrix i j s.a2b2
                     _<-traverse (\i1 -> do x1 <- unsafePeekMatrix i1 j s.a2b2
                                            addRowSmith i i1 (-(x1 `div` x)) s) (rangeWithout 0 (s.a2b2.nrows-1) i)
                     pure unit










smithST :: forall a h r . Partial => EuclideanRing a => Eq a => Show a =>  SmithST h a -> Int -> Int -> Int -> Eff (st :: ST h | r) (Smith a)
smithST s n i j | n >= min s.a2b2.nrows s.a2b2.ncols = freezeSmith s
                | otherwise
                = do reduceRow i j s
                     reduceCol i j s
                     rowEntries <- traverse (\i1 -> do x <- unsafePeekMatrix i1 j s.a2b2
                                                       pure {val: x, i: i1, j: j}) (rangeWithout 0 (s.a2b2.nrows-1) i)
                     colEntries <- traverse (\j1 -> do x <- unsafePeekMatrix i j1 s.a2b2
                                                       pure {val: x, i: i, j: j1}) (rangeWithout 0 (s.a2b2.ncols-1) j)
                     case foldl updateMin Nothing (rowEntries `append` colEntries) of
                           Nothing             -> do swapColSmith j n s
                                                     swapRowSmith i n s
                                                     smithST s (n+1) (n+1) (n+1)
                           Just {i: i', j: j'} ->  do smithST s n i' j'
        where
          updateMin Nothing y@{val: m} 
                              | m==zero   = Nothing
                              | otherwise = Just y
          updateMin x@(Just {val: m}) y@{val: m'} | m' /= zero && degree m'<degree m = Just y
                                                  | otherwise = x




smith :: forall a . EuclideanRing a => Eq a => Show a=>  Matrix a -> Smith a
smith mat = unsafePartial $ pureST (do s <- initSmith mat
                                       smithST s 0 0 0)

