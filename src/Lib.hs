{- Required Extensions -}
{-# LANGUAGE GADTs #-} -- ExistentialQuantification, at least
{-# LANGUAGE MultiParamTypeClasses #-} -- Required for the classes
{-# LANGUAGE FlexibleInstances #-} -- Required to be able to instantiate typeclasses for specific subsets
{-# LANGUAGE FlexibleContexts #-} -- Required to constrain to specific subsets of constraints

{- Optional Extensions -}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib (
    someFunc, someFunc2
) where

import Data.List (intercalate)
import qualified Data.Functor as Drasil

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

{-
    Alternatively, a much more configurable, less constricted variant  (which I think I prefer)...
-}

type Sentence'' = Sentence Json JsonConfig PlainCtx

-- Unfortunately, HLS won't be able to resolve this type signature automatically
-- because Sentence'' is just a type synonym.
ex4 :: Sentence''
ex4 = SConcat (S "Example 4!") (SGen $ AddE (IntE 1) (SubE (IntE 2) (IntE 3)))

conv_ex4 :: Json
conv_ex4 = gen ex4 JsonConfig PlainCtx

someFunc :: IO ()
someFunc = do
    print conv_ex2
    print conv_ex2_othCtx
    print conv_ex3 -- This one might not actually be so bad! This might actually work!
    print conv_ex4 -- probably the best one because it leaves it general, but it depends on usecase. With this, we can force that components can target compiling to nearly any language!

{-

Notes: 
    - we can have "higher level" gens that use "higher level configs" to use already written "gen"s that use "lower level configs"!!!!
        - This is a huge 'plus' for re-usability!
    - We are defining terms accepted into a language as the ways in which it can be "printed"/"gen"ed and handled by another (arguably, "super") language.
    - A printer here is:
        - an input
        - an output
        - a configuration
        - a context of the printing output (e.g., through this, we can have multiple instances for a particular Input/Output, but
            retain knowledge of the differentiation via the 'context'!)
    - The above definition might also be my definition for #2896 - "What is printing?"
    - This appears to be fit reasonably well in Drasil...
    - By having target encodings we 'compile' to, we can compose languages together via typeclasses by saying that certain components of the language *just* need to be compilable to that target language!

-}


someFunc2 :: IO ()
someFunc2 = do
    print "Temp."

