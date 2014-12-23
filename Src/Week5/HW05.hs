{-
   Njagi Mwaniki
   I am renaming modules so that they work with the cabal project it is under.
   Sources consulted:
     Learn you a haskell http://www.learnyouahaskell.com/making-our-own-types-and-typeclasses
-}

module HW05 where


import Src.Week5.Ring
import Src.Week5.Parser
import Test.HUnit
import Data.Maybe

instance Parsable Bool where
--   parse :: String -> Maybe (Bool, String)
  parse = listToMaybe . reads

instance Ring Bool where
  addId  = False
  addInv = not
  mulId  = True

  add = (\fao seco -> if (mod ((bool2Int fao) + (bool2Int seco)) 2)  == 1 then True else False)
  mul = (\fao seco -> if (mod ((bool2Int fao) * (bool2Int seco)) 2)  == 1 then True else False)

bool2Int :: Bool -> Int
bool2Int bool = case bool of
  True -> 1
  False -> 0

-- Definitions for Boolean work.
testBool :: Test
testBool = TestList [
  "test boolean True" ~: (Just (True, "") :: Maybe (Bool, String))  ~=? (parse "True"),
  "test boolean True" ~: (Just (False, " we are") :: Maybe (Bool, String))  ~=? (parse "False we are"),
  "addId"  ~: False ~=? addId,
  "negate" ~: False ~=? addInv True,
  "mulId"  ~: True ~=?  mulId,
  "add"    ~: True ~=? add True False,
  "mul"    ~: False ~=? mul True False
  ]

-- Definitions for Integer work.
testInteger :: Test
testInteger = TestCase $ assertEqual "test addition id is 0" (0 :: Integer) addId 

testIntegerII :: Test
testIntegerII = TestList [
  "test parseRing" ~: (Just 12 :: Maybe Integer)                 ~=? (parseRing "3 + 3* 3"),
  "test parse"     ~: (Just (3, "") :: Maybe (Integer, String))  ~=? (parse "3"),
  "test addId"     ~: (0 :: Integer)                             ~=? (addId),
  "test mulId"     ~: (1 :: Integer)                             ~=? (mulId),
  "test add"       ~: (4 :: Integer)                             ~=? (add 3 1),
  "test mul"       ~: (35 :: Integer)                            ~=? (mul 5 7)
  ]


-- Mod 5 ring.
data Mod5 = MkMod Integer deriving (Show, Eq, Read)

instance Ring Mod5 where
  addId  = MkMod 0
  addInv = MkMod . negate .  (\(MkMod int)  -> int)
  mulId  = MkMod 1

  add  (MkMod fir) (MkMod sec)  =  MkMod $ mod (fir + sec) 5
  mul  (MkMod fir) (MkMod sec)  =  MkMod $ mod (fir * sec) 5

instance Parsable Mod5 where
  parse = listToMaybe . reads

-- Tests for Mod5 as instances of Parsable and Ring

-- As an instance of Parsable
testMod5 :: Test
testMod5 = TestList [
  "test with succeeding chars" ~: (Just (MkMod 61, " kids alone") :: Maybe (Mod5, String))  ~=? (parse "MkMod 61 kids alone"),
  "test with preceeding chars" ~: (Nothing :: Maybe (Mod5, String))  ~=? (parse " kids were MkMod 100"),
  "test with empty string"     ~: (Nothing :: Maybe (Mod5, String))  ~=? (parse ""),
  "test it as a stringxs alone"  ~: (Just (MkMod 5, "") :: Maybe (Mod5, String))  ~=? (parse "MkMod 5"),
  "test addId" ~: (MkMod 0 :: Mod5) ~=? (addId),
  "test mulId" ~: (MkMod 1 :: Mod5) ~=? (mulId),
  "test add"   ~: (MkMod 2 :: Mod5) ~=? (add (MkMod 3) (MkMod 4)),
  "test mul"   ~: (MkMod 0 :: Mod5) ~=? (mul (MkMod 5) (MkMod 7))
  ]

{-
data Mat2x2 = Mat2x2 {a, b, c, d :: Integer}
            deriving (Show, Read, Eq)


instance Ring Mat2x2 where
  addId = Mat2x2 1 0 0 1
-}
