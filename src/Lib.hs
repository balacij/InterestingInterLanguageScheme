{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- I really wish we didn't need AllowAmbiguousTypes and FlexibleInstances, but it appears we do...

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

data JsonConfig = JsonConfig -- in other words, an empty configuration, we can trivially add options to JsonConfig, but they are not important to this prototype (I think!)

-- An important typeclass
class CanGen i o cfg ctx where
    gen :: i -> cfg -> ctx -> o

data Expr = AddE Expr Expr | SubE Expr Expr | IntE Int

data Sentence o cfg ctx where
    S       :: String -> Sentence o cfg ctx
    SGen    :: CanGen i o cfg ctx => i -> Sentence o cfg ctx
    SConcat :: Sentence o cfg ctx -> Sentence o cfg ctx -> Sentence o cfg ctx

instance CanGen Expr Json JsonConfig PlainCtx where
    gen (AddE l r) cfg ctx = JsonArray [gen l cfg ctx, JsonStr "+", gen r cfg ctx]
    gen (SubE l r) cfg ctx = JsonArray [gen l cfg ctx, JsonStr "-", gen r cfg ctx]
    gen (IntE i)   cfg ctx = JsonNum i

instance CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig PlainCtx where
    gen (S s) cfg ctx = JsonStr s
    gen (SGen i) cfg ctx = gen i cfg ctx
    gen (SConcat l r) cfg ctx = JsonArray [gen l cfg ctx, gen r cfg ctx]

data OtherCtx = OtherCtx

instance CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig OtherCtx where -- OtherCtx is a variant that discards S constructors (replace with Null, but renders everything else with a PlainCtx)
    gen (S s)         cfg ctx = JsonNull
    gen (SGen c)      cfg ctx = gen c cfg PlainCtx
    gen (SConcat l r) cfg ctx = JsonArray [gen l cfg ctx, gen r cfg ctx]


ex2 :: Sentence Json JsonConfig PlainCtx -- Here, we are saying that the 'SGen' terms of the language should also be renderable to a PlainCtx version of Json, PlainCtx is a 'name' of sorts for this 'gen' instance.
ex2 = SConcat (S "Hello, World!") (SGen $ AddE (IntE 1) (SubE (IntE 2) (IntE 3)))
-- Unfortunately, with ex2, we have to write the type signature manually, HLS cannot resolve it on it's own


-- ex2' :: Sentence Json JsonConfig OtherCtx -- Here, we are saying that the terms of the language should also be renderable via 'OtherCtx', but the RHS expression is not!, thus, this code needs to be commented out, else an error!
-- ex2' = SConcat (S "Hello, World!") (SGen $ AddE (IntE 1) (SubE (IntE 2) (IntE 3)))


-- conv_ex2 :: CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig PlainCtx => Json
conv_ex2 :: Json
conv_ex2 = gen ex2 JsonConfig PlainCtx

-- conv_ex2' :: CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig OtherCtx => Json
-- conv_ex2' :: CanGen (Sentence Json JsonConfig PlainCtx) Json JsonConfig PlainCtx => Json -- TODO: Figure out why this type signature work too.
conv_ex2_othCtx :: Json
conv_ex2_othCtx = gen ex2 JsonConfig OtherCtx

s_ex2 :: String
s_ex2 = show conv_ex2

s_ex2_othCtx :: String
s_ex2_othCtx = show conv_ex2_othCtx

{-
    NOTE: We can trim down allowed languages, but I intentionally complicated it above.
    More practically, we might have a target language specifically that they should all
    be convertible into.
-}


data Sentence' where
    S'       :: String -> Sentence'
    SGen'    :: (CanGen i Json JsonConfig PlainCtx) => i -> Sentence' -- SGen' now only allows terms that are convertible to Json, with a JsonConfig and in the PlainCtx
    SConcat' :: Sentence' -> Sentence' -> Sentence'

instance CanGen Sentence' Json JsonConfig PlainCtx where
    gen (S' s)         cfg ctx = JsonStr s
    gen (SGen' i)      cfg ctx = gen i cfg ctx
    gen (SConcat' l r) cfg ctx = JsonArray [gen l cfg ctx, gen r cfg ctx]

ex3 :: Sentence' -- HLS can resolve this type signature!
ex3 = SConcat' (SGen' $ AddE (IntE 10) (IntE 30)) (SConcat' (S' "Hello, world!") (SGen' $ SubE (IntE 20) (IntE 30)))

conv_ex3 :: Json
conv_ex3 = gen ex3 JsonConfig PlainCtx

s_ex3 :: String
s_ex3 = show conv_ex3

someFunc :: IO ()
someFunc = do
    putStrLn s_ex2
    putStrLn s_ex2_othCtx
    putStrLn s_ex3 -- This one might not actually be so bad! This might actually work!

{-

Important notes: 
    - we can have "higher level" gens that use "higher level configs" to use already written "gen"s that use "lower level configs"!!!!
        - This is a huge 'plus' for re-usability!
    - We are defining terms accepted into a language as the ways in which it can be "printed"/"gen"ed while carried by the carrying language.
    - A printer here is:
        - an input
        - an output
        - a configuration
        - a context of the printing output (e.g., through this, we can have multiple instances for a particular Input/Output, but
            retain knowledge of the differentiation via the 'context'!)
    - The above definition might also be my definition for #2896 - "What is printing?"

-}
