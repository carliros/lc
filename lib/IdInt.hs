module IdInt(IdInt(..), firstBoundId, toIdInt) where

import           Control.Monad.State
import qualified Data.Map            as M
import           Lambda

newtype IdInt = IdInt Int
    deriving (Eq, Ord)

firstBoundId :: IdInt
firstBoundId = IdInt 0

instance Enum IdInt where
    toEnum i = IdInt i
    fromEnum (IdInt i) = i

instance Show IdInt where
   show (IdInt i) = if i < 0 then "f" ++ show (-i) else "x" ++ show i

toIdInt :: (Ord v) => LC v -> LC IdInt
toIdInt e = evalState (conv e) (0, fvmap)
  where fvmap = foldr (\ (v, i) m -> M.insert v (IdInt (-i)) m) M.empty
                      (zip (freeVars e) [1..])

type M v a = State (Int, M.Map v IdInt) a

-- | Retrieve a values from the state, or insert if there is no value
convVar :: (Ord v) => v -> M v IdInt
convVar v = do
   (i, m) <- get
   case M.lookup v m of
       Nothing -> do
           let ii = IdInt i
           put (i+1, M.insert v ii m)
           return ii
       Just ii -> return ii

-- | Convert lambda calculus to state lambda calculus
conv :: (Ord v) => LC v -> M v (LC IdInt)
conv (Var v)   = liftM Var (convVar v)
conv (Lam v e) = liftM2 Lam (convVar v) (conv e)
conv (App f a) = liftM2 App (conv f) (conv a)
