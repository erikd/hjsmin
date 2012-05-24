{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Control.Monad
import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar5

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Tests"
            [
            ]

data Record     = InputRecord Name Number
                | OutputRecord Name Number OutputType deriving Show
data Number     = Number String                       deriving Show
data Name       = Name String                         deriving Show
data OutputType = OutputType String                   deriving Show

instance Arbitrary Number where
  arbitrary   = liftM Number $ resize 4 $ neStringOf "123456789" "0123456789"
instance Arbitrary Name where
  arbitrary   = liftM Name $ elements [ "foo", "bar", "baz" ]
instance Arbitrary OutputType where
  arbitrary   = liftM OutputType $ elements [ "Binary", "Ascii" ]


{-
instance Arbitrary Record where
  arbitrary = do
                name <- arbitrary
                num <- arbitrary
                InputRecord name num
-}


-- ---------------------------------------------------------------------
-- TODO: This needs to take cognisance of the full unicode identifier range, in time
identifierGen :: Gen [Char]
identifierGen = neStringOf identifierStart identifierPart
  where
    identifierStart = ['a'..'z']++['A'..'Z'] ++ ['_']
    identifierPart = identifierStart ++ ['0'..'9']

-- ---------------------------------------------------------------------
{-
DecimalLiteral ::
  DecimalIntegerLiteral . DecimalDigitsopt ExponentPartopt
  . DecimalDigits ExponentPartopt
  DecimalIntegerLiteral ExponentPartopt
-}
-- TODO: extend to full range
decimalGen :: Gen [Char]
decimalGen = neStringOf ['0'..'9'] ['0'..'9']

-- ---------------------------------------------------------------------

stringLiteralGen :: Gen [Char]
stringLiteralGen = arbitrary

-- ---------------------------------------------------------------------

instance Arbitrary JSNode where
  arbitrary = oneof
              [
                liftM (JSIdentifier JSNoAnnot) identifierGen
              , liftM (JSDecimal JSNoAnnot) decimalGen
              , liftM (JSLiteral JSNoAnnot) stringLiteralGen

              ]

{-
instance Arbitrary JSBlock where
  arbitrary = JSBlock JSAnnot [JSStatement] JSAnnot

instance Arbitrary JSBlock where
  arbitrary = undefined

instance Arbitrary JSStatement where
  arbitrary = undefined
-}
--instance Arbitrary JSAST where
--  arbitrary   = JSSourceElementsTop (elements [[],])


-- ---------------------------------------------------------------------
-- These routines are taken from
-- http://www.haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator

neStringOf :: [a] -> [a] -> Gen [a]
neStringOf chars_start chars_rest =
  do s <- elements chars_start
     r <- listOf' $ elements chars_rest
     return (s:r)

listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf' k gen

vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]

-- ---------------------------------------------------------------------
