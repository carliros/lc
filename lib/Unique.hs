module Unique(normalForm) where

import           Control.Monad.State
import qualified Data.Map            as M
import           IdInt
import           Lambda

normalForm :: LC IdInt -> LC IdInt
normalForm e = evalState (normalForm' e') i
  where (e', (i, _)) = runState (unique e) (firstBoundId, M.empty)

type N a = State IdInt a

normalForm' :: LC IdInt -> N (LC IdInt)
normalForm' e@(Var _) = return e
normalForm' (Lam x e) = liftM (Lam x) (normalForm' e)
normalForm' (App f a) = do f' <- weekHeadNormalForm f
                           case f' of
                              Lam x b -> substitute x a b >>= normalForm'
                              _       -> liftM2 App (normalForm' f') (normalForm' a)

weekHeadNormalForm :: LC IdInt -> N (LC IdInt)
weekHeadNormalForm e@(Var _) = return e
weekHeadNormalForm e@(Lam _ _) = return e
weekHeadNormalForm (App f a) = do f' <- weekHeadNormalForm f
                                  case f' of
                                      Lam x b -> substitute x a b >>= weekHeadNormalForm
                                      _       -> return $ App f' a

substitute :: IdInt -> LC IdInt -> LC IdInt -> N (LC IdInt)
substitute x s b = sub b
  where sub e@(Var v)
            | v == x    = clone M.empty s
            | otherwise = return e
        sub (Lam v e) = liftM (Lam v) (sub e)
        sub (App f a) = liftM2 App (sub f) (sub a)

        clone m e@(Var v) = return $ maybe e Var (M.lookup v m)
        clone m (Lam v e) = do v' <- newVar
                               liftM (Lam v') (clone (M.insert v v' m) e)
        clone m (App f a) = liftM2 App (clone m f) (clone m a)

newVar :: N IdInt
newVar = do i <- get
            put (succ i)
            return i

type U a = State (IdInt, M.Map IdInt IdInt) a

unique :: LC IdInt -> U (LC IdInt)
unique (Var v)   = liftM Var (getVar v)
unique (Lam v e) = liftM2 Lam (addVar v) (unique e)
unique (App f a) = liftM2 App (unique f) (unique a)

addVar :: IdInt -> U IdInt
addVar v = do (i, m) <- get
              put (succ i, M.insert v i m)
              return i

getVar :: IdInt -> U IdInt
getVar v = do (_, m) <- get
              return $ maybe v id (M.lookup v m)
