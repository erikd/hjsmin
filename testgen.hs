{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Control.Monad
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar5
import Language.JavaScript.Parser.Parser
import Text.Jasmine
import Text.Jasmine.Transform
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as S8

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Tests"
            [
              testProperty "foo" prop_parse
            , testProperty "parse" prop_parseJsOk
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

jsIdentifierGen :: Gen JSNode
jsIdentifierGen = liftM  (JSIdentifier JSNoAnnot) identifierGen

-- ---------------------------------------------------------------------

nullLiteralGen :: Gen [Char]
nullLiteralGen = elements ["null"]

-- ---------------------------------------------------------------------

booleanLiteralGen :: Gen [Char]
booleanLiteralGen = elements ["true","false"]

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
{-
HexIntegerLiteral ::
  0x HexDigit
  0X HexDigit
  HexIntegerLiteral HexDigit
-}
hexIntegerGen :: Gen [Char]
hexIntegerGen = do
  s <- elements ["0x","0X"]
  let hexDigits = ['0'..'9'] ++ ['a'..'f']++['A'..'F']
  r <- neStringOf hexDigits hexDigits
  return (s++r)

-- ---------------------------------------------------------------------

-- TODO: include escape chars etc.
stringLiteralGen :: Gen [Char]
stringLiteralGen = arbitrary

-- ---------------------------------------------------------------------

-- TODO: extend / fix
regexLiteralGen :: Gen [Char]
regexLiteralGen = do
  body <- stringLiteralGen
  flags <- listOf' $ elements (['a'..'z']++['A'..'Z'])
  return (['/']++body++['/']++flags)

-- ---------------------------------------------------------------------

instance Arbitrary JSNode where
  arbitrary = oneof
              [
                -- Literals
                liftM  (JSLiteral       JSNoAnnot) nullLiteralGen
              , liftM  (JSLiteral       JSNoAnnot) booleanLiteralGen
              , liftM  (JSDecimal       JSNoAnnot) decimalGen
              , liftM  (JSHexInteger    JSNoAnnot) hexIntegerGen
              , liftM2 (JSStringLiteral JSNoAnnot) (elements ['\'','"']) stringLiteralGen
              , liftM  (JSRegEx         JSNoAnnot) regexLiteralGen


              --, liftM  (JSIdentifier JSNoAnnot) identifierGen
              , jsIdentifierGen
              , liftM  (JSLiteral JSNoAnnot) stringLiteralGen

              ]


-- TODO: 4 is an arbitrary number
instance Arbitrary JSBlock where
  arbitrary = liftM mkBlock (resize 3 $ arbitrary :: Gen [JSStatement])
    where
      mkBlock xs = JSBlock JSNoAnnot xs JSNoAnnot

  shrink (JSBlock JSNoAnnot xs JSNoAnnot) = (go 1) ++ (go 2)
    where
      go n = map (\xs' -> (JSBlock JSNoAnnot xs' JSNoAnnot)) $ splitInto n xs

emptyJSNode :: Gen [JSNode]
emptyJSNode = elements [[]]

optionalIdentifierGen :: Gen [JSNode]
optionalIdentifierGen = do
  oneof [(vectorOf 0 jsIdentifierGen),(vectorOf 1 jsIdentifierGen)]

breakStatementGen :: Gen JSStatement
breakStatementGen = liftM mkBreak optionalIdentifierGen
  where
    -- TODO: AST should not require this space to be explicitly added
    mkBreak [] = JSBreak JSNoAnnot [] (JSSemi JSNoAnnot)
    mkBreak xs = JSBreak JSNoAnnot ((literal " "):xs) (JSSemi JSNoAnnot)

blockStatementGen :: Gen JSStatement
blockStatementGen = liftM JSStatementBlock arbitrary

instance Arbitrary JSStatement where
  arbitrary = oneof
              [
                breakStatementGen
              , blockStatementGen
              ]

instance Arbitrary JSAST where
  arbitrary = liftM JSSourceElementsTop arbitrary


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

data JavaScriptSource = JavaScriptSource String JSAST deriving Show

instance Arbitrary JavaScriptSource where
  arbitrary = liftM mkSrc (arbitrary :: Gen JSAST)
    where
      --mkSrc :: JSAST
      mkSrc s = JavaScriptSource (lbToStr $ BB.toLazyByteString $ renderJS s) s

  shrink (JavaScriptSource _ (JSSourceElementsTop xs)) = (go 1) ++ (go 2) ++ (go 3) ++ (go 4)
    where
      go n = map (\xs' -> JavaScriptSource (toJsStr (JSSourceElementsTop xs')) (JSSourceElementsTop xs')) $ splitInto n xs

splitInto :: Int -> [a] -> [[a]]
splitInto n [] = []
splitInto n xs = (take n xs):splitInto n (drop n xs)


genJs :: IO JavaScriptSource
genJs = do
  s <- liftM head $ liftM (drop 8) $ sample' (arbitrary :: Gen JSAST)
  return $ JavaScriptSource (toJsStr s) s
  -- return $ BB.toLazyByteString $ renderJS $ transformJS s
  -- return $ s

toJsStr :: JSAST -> [Char]
toJsStr s = lbToStr $ BB.toLazyByteString $ renderJS s

lbToStr :: S8.ByteString -> [Char]
lbToStr = unpack . decodeUtf8With lenientDecode

-- ---------------------------------------------------------------------

prop_parse :: [Int] -> Bool
prop_parse xs = xs == xs


prop_parseJsOk :: JavaScriptSource -> Bool
prop_parseJsOk (JavaScriptSource input _s) = do
  case (parse input "src") of
    Left msg -> False
    Right p -> True
