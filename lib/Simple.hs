module Simple(normalForm) where

import           Data.List (union, (\\))
import           IdInt
import           Lambda

-- | Computes normal form
normalForm :: LC IdInt -> LC IdInt
normalForm e@(Var _) = e
normalForm (Lam x e) = Lam x (normalForm e)
normalForm (App f a) =
    case weekHeadNormalForm f of
        Lam x b -> normalForm (substitute x a b)
        f'      -> App (normalForm f') (normalForm a)

-- | Computes week head normal form only for Application
weekHeadNormalForm :: LC IdInt -> LC IdInt
weekHeadNormalForm e@(Var _) = e
weekHeadNormalForm e@(Lam _ _) = e
weekHeadNormalForm (App f a) =
    case weekHeadNormalForm f of
        Lam x b -> weekHeadNormalForm (substitute x a b)
        f'      -> App f' a

substitute :: IdInt -> LC IdInt -> LC IdInt -> LC IdInt
substitute x s b = sub b
 where sub e@(Var v)
            | v == x = s
            | otherwise = e
       sub e@(Lam v e')
            | v == x = e
            | v `elem` freeVariables = Lam v' (sub e'')
            | otherwise = Lam v (sub e')
                where v'  = newId variables
                      e'' = substitute v (Var v') e'
       sub (App f a) = App (sub f) (sub a)
       freeVariables = freeVars s
       variables = freeVariables `union` allVars b

newId :: [IdInt] -> IdInt
newId vs = head ([firstBoundId .. ] \\ vs)
