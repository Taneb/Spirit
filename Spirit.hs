module Spirit (TypeSignature(..), spirit, spirit') where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (fromMaybe)

import Spirit.Names
import Spirit.Types

-- Try to reify a type with no builtins
spirit :: TypeSignature -> Maybe HaskellExpr
spirit = spirit' []

-- Try to reify a type using a given collection of builtins
spirit' :: [Assumption] -> TypeSignature -> Maybe HaskellExpr
spirit' assumptions = reify (filteredNameList $ map (show . fst) assumptions) assumptions

-- Try to reify a type
reify :: [Name] -> [Assumption] -> Goal -> Maybe HaskellExpr
reify names assumptions goal@(t :-> u) = match assumptions goal names <|> reify'

    -- If an outright match didn't work, try pattern matching and reifying the new goal.
    where reify' = let (n, expansions) = expand t names
                   in if length expansions == 1
                      then let (p, f) = makeCase $ head expansions
                           in fmap (Lambda (if p == "" then n else n ++ "@" ++ p)) f
                      else Just . Lambda n . Case (Literal n) $ map (fmap (fromMaybe (Literal "undefined")) . makeCase) expansions

          makeCase (as', ns', pattern) = (pattern, reify ns' (as' ++ assumptions) u)

reify names assumptions goal = match assumptions goal names

-- Fully expand a type signature for pattern matching.
expand :: TypeSignature -> [Name] -> (Name, [([Assumption], [Name], Pattern)])
expand t@(TVar _) (n:ns) = (n, [([(Literal n, t)], ns, "")])
expand t@(_ :-> _) (n:ns) = (n, [([(Literal n, t)], ns, "")])
expand t@(a :.: b) (m:n:o:ns) = (m, [([(Literal n, a), (Literal o, b), (Literal m, t)], ns, "(" ++ n ++ "," ++ o ++ ")")])
expand t@(ListT a) (m:n:o:ns) = (m, [([(Literal n, a), (Literal o, ListT a), (Literal m, t)], ns, "(" ++ n ++ ":" ++ o ++ ")")
                                    , ([(Literal m, t)], n:o:ns, "[]")])

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
