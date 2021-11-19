{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- I really wish we didn't need AllowAmbiguousTypes, FlexibleContexts, and FlexibleContexts, but it appears we do...

module Lib
    ( someFunc
    ) where
import Data.List (intercalate)

data PlainCtx = PlainCtx

data Json where
    JsonNull :: Json
    JsonStr :: String -> Json
    JsonNum :: (Show a, Num a) => a -> Json
    JsonBool :: Bool -> Json
    JsonArray :: [Json] -> Json
    -- ignore objects/maps for now...

instance Show Json where
    show JsonNull = "null"
    show (JsonStr s) = '"' : s ++ "\""
    show (JsonNum a) = show a
    show (JsonBool b) = show b
    show (JsonArray jss) = '[' : (intercalate "," (map show jss) ++ "]")

data JsonConfig = JsonConfig -- in other words, an empty configuration

-- An important typeclass
class CanGen i o cfg ctx where
    gen :: i -> cfg -> ctx -> o

data Expr = AddE Expr Expr | SubE Expr Expr | IntE Int

data Sentence o cfg ctx where
    S :: String -> Sentence o cfg ctx
    SGen :: CanGen i o cfg ctx => i -> Sentence o cfg ctx
    SConcat :: Sentence o cfg ctx -> Sentence o cfg ctx -> Sentence o cfg ctx

instance CanGen Expr Json JsonConfig PlainCtx where
    gen (AddE l r) cfg ctx = JsonArray [JsonStr "+"]
    -- gen (SubE l r) cfg= JsonArray [JsonStr "-"]
    gen (SubE l r) cfg ctx = JsonArray [gen l cfg ctx, JsonStr "-", gen r cfg ctx]
    gen (IntE i)   cfg ctx = JsonNum i

instance CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig PlainCtx where
    gen (S s) cfg ctx = JsonStr s
    gen (SGen i) cfg ctx = gen i cfg ctx
    gen (SConcat l r) cfg ctx = JsonArray [gen l cfg ctx, gen r cfg ctx]

data OtherCtx = OtherCtx
instance CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig OtherCtx where -- OtherCtx is a variant that discards strings (as Null, but renders everything else with a PlainCtx)
    gen (S s)         cfg ctx = JsonNull
    gen (SGen c)      cfg ctx = gen c cfg PlainCtx
    gen (SConcat l r) cfg ctx = JsonArray [gen l cfg ctx, gen r cfg ctx]


ex2 :: Sentence Json JsonConfig PlainCtx
ex2 = SConcat (S "Hello, World!") (SGen $ AddE (IntE 1) (SubE (IntE 2) (IntE 3)))

ex2' :: Sentence Json JsonConfig PlainCtx
ex2' = SConcat (S "Hello, World!") (SGen $ AddE (IntE 1) (SubE (IntE 2) (IntE 3)))


conv_ex2 :: CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig PlainCtx => Json
conv_ex2 = gen ex2 JsonConfig PlainCtx

conv_ex2' :: CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig PlainCtx => Json
conv_ex2' = gen ex2' JsonConfig OtherCtx

s_ex2 :: String
s_ex2 = show conv_ex2

s_ex2' :: String
s_ex2' = show conv_ex2'

someFunc :: IO ()
someFunc = do
    putStrLn s_ex2
    putStrLn s_ex2'

{-

Important notes: 
    - we can have "higher level" gens that use "higher level configs" to use already written "gen"s that use "lower level configs"!!!!
        - This is a huge 'plus' for re-usability!
    - 

-}
