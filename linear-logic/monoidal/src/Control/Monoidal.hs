{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, GADTs, RankNTypes #-}

module Control.Monoidal where

import Control.Category
import Prelude hiding (curry, uncurry, sum)
import qualified Prelude as P
import qualified Control.Category as C

{- 

class Category cat where
  -- | the identity morphism
  id :: cat a a

  -- | morphism composition
  (.) :: cat b c -> cat a b -> cat a c

-}

{-
Pair / Tuple / Product

Sum / coproduct / either

Choice / "external choice"

par "a par b" == "(A -> Empty, B -> Empty) -> Empty"

-}

-- class Category cat => ProductCategory cat pair | cat -> pair where
--   uncurry :: (pair a b `cat` c) `cat` (a `cat` (b `cat` c))
--   curry   :: (a `cat` (b `cat` c)) `cat` (pair a b `cat` c)

class Category cat => ProductCategory cat pair | cat -> pair where
  curry   :: (pair a b `cat` c) -> (a `cat` (b `cat` c))
  uncurry :: (a `cat` (b `cat` c)) -> (pair a b `cat` c)


class Category cat => SumCategory cat sum | cat -> sum where
  -- sum2choice :: ChoiceCategory cat choice => (sum a b) `cat` (((a `cat` c) `choice` (b `cat` c)) `cat` c)
  sum2choice :: ChoiceCategory cat choice => (sum a b `cat` c) -> ((choice (a `cat` c) (b `cat` c)) `cat` c)
  caseSum :: ChoiceCategory cat choice => sum a b `cat` (choice (a `cat` c) (b `cat` c) `cat` c)
  inLeft :: a `cat` sum a b
  inRight :: b `cat` sum a b

{- 
not (Sum a b)  == choice (not a) (not b)
not (Pair a b) == par (not a) (not b)

not (choice a b) == sum (not a)
-}

type Two = Bool

class Category cat => ChoiceCategory cat choice | cat -> choice where
  choiceIntro :: SumCategory cat sum => 
    ((sum a b) `cat` c) -> (choice (a `cat` c) (b `cat` c))
  chooseLeft :: SumCategory cat sum =>
    choice a b `cat` a
  chooseRight :: SumCategory cat sum =>
    choice a b `cat` b
  -- choiceElim :: SumCategory cat sum =>
    -- (choice (a `cat` b) (b `cat` c)) `cat` ((sum a b) `cat` c)
    -- (choice (a `cat` b) (b `cat` c)) -> ((sum a b) `cat` c)



instance ProductCategory (->) (,) where
  curry   = P.curry
  uncurry = P.uncurry

data ChooseH a b c where
  CLeft  :: ChooseH a b a
  CRight :: ChooseH a b b

newtype Choice a b = MkC (forall res . ChooseH a b res -> res)

instance ChoiceCategory (->) Choice where
  chooseLeft  (MkC f) = f CLeft
  chooseRight (MkC f) = f CRight
  choiceIntro f = MkC $ \ x ->
          case x of
            CRight -> f C.. inRight
            CLeft ->  f C.. inLeft


instance SumCategory (->) Either where
  inRight = Right
  inLeft  = Left
  caseSum elr c = case elr of
    Left a  -> chooseLeft c $ a
    Right a -> chooseRight c $ a


data LittleLinear a where
  LInRight :: LittleLinear (Choice a b -> b)
  LInLeft :: LittleLinear (Choice a b -> a)
  Apply  :: (LittleLinear (a -> b)) -> (LittleLinear a) -> LittleLinear b

{-

You need this for Choice and Sum
reassociate :: Pair a (Pair b c) -> Pair (Pair a b) c
swap :: Pair a b -> Pair b a

choose[a->c, b->c] -> 

not (sum a b) = choose (not a) (not b)

sequent logic:
comma on the left of |- == "AND"
comma on the right of |- == "OR"

PAIR Tuple
Gamma,a,b  |-  c
------------------ curry/uncurry
Gamma,(a, b)  |-  c


Gamma,a |- c
------------------- Intro left / left elim
Gamma,(Sum a b) |-  c

Gamma,b |- c
--------------------InRight / right elim
Gamma,(Sum a b) |- c



-}
