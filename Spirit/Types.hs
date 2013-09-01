module Spirit.Types where

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
                   deriving (Eq, Show)

-- Type of names
type Name = String

-- Type of assumptions
type Assumption = (HaskellExpr, TypeSignature)

-- Type of goals
type Goal = TypeSignature

-- Type of patterns
type Pattern = String
