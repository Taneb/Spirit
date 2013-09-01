module Main where

import Data.Maybe (fromMaybe)

import Spirit
import Spirit.Types (HaskellExpr(Literal))

main :: IO ()
main = do demonstrate "id"    idType
          demonstrate "const" constType
          demonstrate "fst"   fstType
          demonstrate "snd"   sndType

          demonstrate "uncurry" uncurryType
          demonstrate "flip"    flipType

    where demonstrate name typ = do putStrLn $ name ++ " :: " ++ show typ
                                    print . fromMaybe (Literal "undefined") . spirit $ typ
                                    putStrLn ""

          idType    = TVar 1 :-> TVar 1
          constType = TVar 1 :-> TVar 2 :-> TVar 1
          fstType   = TVar 1 :.: TVar 2 :-> TVar 1
          sndType   = TVar 1 :.: TVar 2 :-> TVar 2

          uncurryType = (TVar 1 :-> TVar 2 :-> TVar 3) :-> TVar 1 :.: TVar 2 :-> TVar 3
          flipType    = (TVar 1 :-> TVar 2 :-> TVar 3) :-> TVar 2 :-> TVar 1 :-> TVar 3
