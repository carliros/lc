module Lambda(LC(..), freeVars, allVars, Id(..)) where

import           Data.Char                    (isAlphaNum)
import           Data.List
import           Text.ParserCombinators.ReadP
import           Text.PrettyPrint.HughesPJ    (Doc, parens, renderStyle, style,
                                               text, (<+>), (<>))

-- | Main data type for lambda calculus
data LC v
  = Var v               -- ^ a variable
  | Lam v (LC v)        -- ^ a lambda function
  | App (LC v) (LC v)   -- ^ an application
   deriving (Eq)


-- | Retrieves free variables of the Lambda expression
freeVars :: (Eq v) => LC v -> [v]
freeVars (Var v)   = [v]
freeVars (Lam v e) = freeVars e \\ [v]
freeVars (App f a) = freeVars f `union` freeVars a


-- | Retrieves all variables of the lambda expression
allVars :: (Eq v) => LC v -> [v]
allVars (Var v)   = [v]
allVars (Lam _ e) = allVars e
allVars (App f a) = allVars f `union` allVars a


instance (Read v) => Read (LC v) where
    readsPrec _ = readP_to_S pLC

pLC :: (Read v) => ReadP (LC v)
pLC = pLCLam +++ pLCApp +++ pLCLet

pLCVar :: (Read v) => ReadP (LC v)
pLCVar
  = do v <- pVar
       return $ Var v

pLCLam :: (Read v) => ReadP (LC v)
pLCLam
  = do schar '\\'
       v <- pVar
       schar '.'
       e <- pLC
       return $ Lam v e

pLCApp :: (Read v) => ReadP (LC v)
pLCApp
  = do es <- many1 pLCAtom
       return $ foldl1 App es

pLCAtom :: (Read v) => ReadP (LC v)
pLCAtom = pLCVar +++ (do schar '('; e <- pLC; schar ')'; return e)

pLCLet :: (Read v) => ReadP (LC v)
pLCLet = do
    let lcLet (x,e) b = App (Lam x b) e
        pDef = do
          v <- pVar
          schar '='
          e <- pLC
          return (v, e)
    sstring "let"
    bs <- sepBy pDef (schar ';')
    sstring "in"
    e <- pLC
    return $ foldr lcLet e bs

schar :: Char -> ReadP Char
schar c = skipSpaces >> char c

sstring :: String -> ReadP String
sstring c = skipSpaces >> string c

pVar :: (Read v) => ReadP v
pVar = skipSpaces >> readS_to_P (readsPrec 9)


instance (Show v) => Show (LC v) where
    show = renderStyle style . ppLC 0

ppLC :: (Show v) => Int -> LC v -> Doc
ppLC _ (Var v)   = text $ show v
ppLC p (Lam v e) = pparens (p>0) $ text ("\\" ++ show v ++ ".") <> ppLC 0 e
ppLC p (App f a) = pparens (p>1) $ ppLC 1 f <+> ppLC 2 a

pparens :: Bool -> Doc -> Doc
pparens True d  = parens d
pparens False d = d


newtype Id = Id String
    deriving (Eq, Ord)

instance Show Id where
    show (Id i) = i

instance Read Id where
    readsPrec _ s =
        case span isAlphaNum s of
          ("", _) -> []
          (i, s') -> [(Id i, s')]
