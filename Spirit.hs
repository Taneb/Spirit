module Spirit (TypeSignature(..), spirit, spirit') where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (find)

import Spirit.Types

-- Try to reify a type with no builtins
spirit :: TypeSignature -> Maybe HaskellExpr
spirit = spirit' []

-- Try to reify a type using a given collection of builtins
spirit' :: [Assumption] -> TypeSignature -> Maybe HaskellExpr
spirit' assumptions = reify (nameList $ map (show . fst) assumptions) assumptions

-- Try to reify a type
reify :: [Name] -> [Assumption] -> Goal -> Maybe HaskellExpr
reify names assumptions goal@(t :-> u) = match assumptions goal names <|> reify'

    -- If an outright match didn't work, try pattern matching and reifying the new goal.
    where reify' = let (as', ns', pattern) = expand t names
                   in reify ns' (as' ++ assumptions) u >>=
                      \f -> Just $ Lambda pattern f

reify names assumptions goal = match assumptions goal names

-- Fully expand a type signature for pattern matching.
expand :: TypeSignature -> [Name] -> ([Assumption], [Name], Pattern)
expand t@(TVar _) (n:ns) = ([(Literal n, t)], ns, n)
expand t@(_ :-> _) (n:ns) = ([(Literal n, t)], ns, n)
expand t@(a :.: b) (n:m:o:ns) = ([(Literal n, t), (Literal m, a), (Literal o, b)], ns, n ++ "@(" ++ m ++ "," ++ o ++ ")")

-- Check if the goal can be fulfilled by some combination of the assumptions
match :: [Assumption] -> Goal -> [Name] -> Maybe HaskellExpr
match assumptions goal names = match1 assumptions goal <|>
                               match2 names assumptions assumptions goal

-- Try to find an exact match
match1 :: [Assumption] -> Goal -> Maybe HaskellExpr
match1 as g = fmap fst $ find ((g==) . snd) as

-- Try to find a function application match
match2 :: [Name] -> [Assumption] -> [Assumption] -> Goal -> Maybe HaskellExpr
match2 names assumptions ((n, t :-> u):as) goal | relevant goal (t :-> u) =
    case reify names assumptions t of
      Nothing -> match2 names assumptions as goal
      Just r -> reify names ((App n r, u) : assumptions) goal

match2 names assumptions (_:as) goal = match2 names assumptions as goal
match2 _ _ [] _ = Nothing

-- Check if a function type is relevant to a goal
relevant :: Goal -> TypeSignature -> Bool
relevant g f@(_ :-> u) = f == g || relevant g u
relevant g t = t == g

-- Produce an infinite list of unique names, which don't overlap with the assumptions
nameList :: [Name] -> [Name]
nameList as = [replicate k ['a' .. 'z'] | k <- [1..]] >>=
              sequence >>=
              \n -> guard (n `notElem` as) >>
              return n
