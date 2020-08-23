module Sandbox.BoxSpec (spec) where

import Test.Hspec
import Control.Exception
import Control.Monad.Reader


data MyContext = MyContext
                 { foo :: String
                 , bar :: Int
                 } deriving (Show)

spec :: Spec
spec = do
  describe "monads" $ do

    it "maybe" $ do
      let
        add2 :: Integer -> Maybe Integer
        add2 a = Just (a + 2)
      let
        mul3 :: Integer -> Maybe Integer
        mul3 a = Just (a * 3)
      (Just 4 >>= add2 >>= mul3 >>= add2) `shouldBe` Just 20

      let
        e1 :: Maybe Int
        e1 = do
          a <- Just 3
          b <- Just 4
          return $ a + b

      e1 `shouldBe` Just 7

    it "list" $ do
      let
        f :: Integer -> [Integer]
        f x = [ x + 2 ]
      ([1, 2, 3] >>= f) `shouldBe` [3, 4, 5]


    it "IO Monad" $ do
       let
         f :: String -> IO ()
         f x = putStrLn x
       let
         stringInIO :: String -> IO String
         stringInIO x = evaluate x
       stringInIO "I was created by stringInIo" >>= f


    it "reader monad" $ do
      let
        computation :: Reader MyContext (Maybe String)
        computation = do
          n <- asks bar
          x <- asks foo
          if n > 0
            then return (Just x)
            else return Nothing

      runReader computation (MyContext "hello" 1) `shouldBe` Just "hello"
      runReader computation (MyContext "hello" 0) `shouldBe` Nothing



