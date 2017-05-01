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

-- | Builds a Monad State for LC IdInt where all free vars of LC are inserted into a Map with an [1..] value
toIdInt :: (Ord v) => LC v -> LC IdInt
toIdInt e = evalState (convertToStateLC e) initialState
  where initialState = (0, freeVarsMap)
        freeVarsMap = foldr func M.empty zipList
        zipList = zip (freeVars e) [1..]
        func (v, i) m = M.insert v (IdInt (-i)) m

type M v a = State (Int, M.Map v IdInt) a

-- | Convert lambda calculus to state lambda calculus
convertToStateLC :: (Ord v) => LC v -> M v (LC IdInt)
convertToStateLC (Var v)   = liftM Var (convertVar v)
convertToStateLC (Lam v e) = liftM2 Lam (convertVar v) (convertToStateLC e)
convertToStateLC (App f a) = liftM2 App (convertToStateLC f) (convertToStateLC a)

-- | Retrieve a values from the state, or insert if there is no value
convertVar :: (Ord v) => v -> M v IdInt
convertVar v = do
   (i, m) <- get
   case M.lookup v m of
       Nothing -> do let ii = IdInt i
                     put (i+1, M.insert v ii m)
                     return ii
       Just ii -> return ii