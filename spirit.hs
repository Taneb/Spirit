module Spirit (TypeSignature(..), spirit, spirit') where

import Control.Monad (guard)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

-- ADT for type signatures
data TypeSignature = TVar Int
                   | TypeSignature :-> TypeSignature
                   | TypeSignature :.: TypeSignature
                     deriving (Eq, Show)

infixl 9 :.:
infixr 5 :->

-- Type of names
type Name = String

-- Type of assumptions
type Assumption = (Name, TypeSignature)

-- Try to reify a type with no builtins
spirit :: TypeSignature -> Maybe String
spirit = spirit' []

-- Try to reify a type using a given collection of builtins
spirit' :: [Assumption] -> TypeSignature -> Maybe String
spirit' assumptions = reify (nameList $ map fst assumptions) assumptions

-- Try to reify a type
reify :: [Name] -> [Assumption] -> TypeSignature -> Maybe String
-- reify names assumptions goal | trace ("reify " ++ show assumptions ++ " | " ++ show goal) False = undefined
reify names@(n:ns) assumptions goal = case match assumptions goal names of
                                        Nothing -> case goal of
                                                    t :-> u -> let (as', ns', pattern) = expand t names
                                                              in reify ns' (as' ++ assumptions) u >>=
                                                                 \f -> Just $ "\\" ++ pattern ++ " -> " ++ f
                                                    _ -> Nothing
                                        m -> m

-- Fully expand a type signature for pattern matching.
expand :: TypeSignature -> [Name] -> ([Assumption], [Name], String)
expand t@(TVar _) (n:ns) = ([(n, t)], ns, n)
expand t@(_ :-> _) (n:ns) = ([(n, t)], ns, n)
expand t@(a :.: b) (n:m:o:ns) = ([(n, t), (m, a), (o, b)], ns, n ++ "@(" ++ m ++ "," ++ o ++ ")")

-- Check if the goal can be fulfilled by some combination of the assumptions
match :: [Assumption] -> TypeSignature -> [Name] -> Maybe String
match assumptions goal names = case match1 assumptions goal of
                                 Nothing -> match2 names assumptions assumptions goal
                                 m -> m

-- Try to find an exact match
match1 :: [Assumption] -> TypeSignature -> Maybe String
-- match1 ((n, t):as) g | trace ("match1 " ++ n ++ " " ++ show t ++ " | " ++ show g) False = undefined
match1 ((n, t):as) g | t == g = Just n
                     | otherwise = match1 as g
match1 [] _ = Nothing

-- Try to find a function application match
match2 :: [Name] -> [Assumption] -> [Assumption] -> TypeSignature -> Maybe String
-- match2 names assumptions ((n, t :-> u):as) goal | trace ("match2 " ++ show assumptions ++ " | " ++ show goal ++ " | " ++ show (t :-> u) ++ " | " ++ show (relevant goal (t :-> u))) False = undefined
match2 names assumptions ((n, t :-> u):as) goal | relevant goal (t :-> u) =
                                                    case reify names assumptions t of
                                                      Nothing -> match2 names assumptions as goal
                                                      Just r -> let (as', ns', pattern) = expand t names
                                                               in let r' = if all isAlphaNum r then r else "(" ++ r ++ ")"
                                                                  in reify ns' ((n ++ " " ++ r', u) : as' ++ assumptions) goal
match2 names assumptions (_:as) goal = match2 names assumptions as goal
match2 _ _ [] _ = Nothing

-- Check if a function type is relevant to a goal
relevant :: TypeSignature -> TypeSignature -> Bool
relevant g f@(t :-> u) = f == g || relevant g u
relevant g t = t == g

-- Produce an infinite list of unique names, which don't overlap with the assumptions
nameList :: [Name] -> [Name]
nameList as = [replicate k ['a' .. 'z'] | k <- [1..]] >>=
              sequence >>=
              \n -> guard (n `notElem` as) >>
              return n


main = do demonstrate "id"    idType
          demonstrate "const" constType
          demonstrate "fst"   fstType
          demonstrate "snd"   sndType
          demonstrate "uncurry" uncurryType
          demonstrate "flip"    flipType

    where demonstrate name typ = do putStrLn $ name ++ " :: " ++ show typ
                                    putStrLn . fromMaybe "undefined" . spirit $ typ
                                    putStrLn ""

          idType    = TVar 1 :-> TVar 1
          constType = TVar 1 :-> TVar 2 :-> TVar 1
          fstType   = TVar 1 :.: TVar 2 :-> TVar 1
          sndType   = TVar 1 :.: TVar 2 :-> TVar 2

          uncurryType = (TVar 1 :-> TVar 2 :-> TVar 3) :-> TVar 1 :.: TVar 2 :-> TVar 3
          flipType    = (TVar 1 :-> TVar 2 :-> TVar 3) :-> TVar 2 :-> TVar 1 :-> TVar 3
