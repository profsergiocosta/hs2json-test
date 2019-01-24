
import Test.QuickCheck
import Data.List
import Prettify

import Control.Monad

{--
instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1,6) :: Gen Int
        case n of
             1 -> return Empty

             2 -> do x <- arbitrary
                     return (Char x)

             3 -> do x <- arbitrary
                     return (Text x)

             4 -> return Line

             5 -> do x <- arbitrary
                     y <- arbitrary
                     return (Concat x y)

             6 -> do x <- arbitrary
                     y <- arbitrary
                     return (Union x y)
--}

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

main :: IO ()
main = do
    docs <- generate  arbitrary :: IO [Doc]
    print docs
    docs <- generate  arbitrary :: IO [Doc]
    print docs
    docs <- generate  arbitrary :: IO [Doc]
    print docs
