module Spirit.Types (TypeSignature(..) 
                    , HaskellExpr(..) 
                    , Name 
                    , Assumption 
                    , Goal 
                    , Pattern) where

-- ADT for type signatures
data TypeSignature = TVar Int
                   | TypeSignature :-> TypeSignature
                   | TypeSignature :.: TypeSignature
                     deriving (Eq, Show)

infixl 9 :.:
infixr 5 :->

-- ADT for generated expressions
data HaskellExpr = Literal String
                 | Lambda Pattern HaskellExpr
                 | App HaskellExpr HaskellExpr
                   deriving (Eq)

-- Type of names
type Name = String

-- Type of assumptions
type Assumption = (HaskellExpr, TypeSignature)

-- Type of goals
type Goal = TypeSignature

-- Type of patterns
type Pattern = String

-- Printing of expressions
instance Show HaskellExpr where
    show (Literal s) = s

    show (Lambda p e) = "\\" ++ p ++ " -> " ++ show e

    show (App (Literal f) (Literal e)) = f ++ " " ++ e

    show (App f (Literal e)) | simple f  = show f ++ " " ++ e
                             | otherwise =  "(" ++ show f ++ ") " ++ e

    show (App f expr) | simple f = show f ++ " (" ++ show expr ++ ")"
                      | otherwise = "(" ++ show f ++ ") (" ++ show expr ++ ")"

-- Check if a function application is "simple" (not needing parens)
simple :: HaskellExpr -> Bool
simple (App f _) = True
simple (Literal _) = True
simple _ = False
