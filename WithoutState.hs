{-# LANGUAGE LambdaCase #-}
module WithoutState where

import System.Random
import Control.Monad.State
import Data.Time.Clock
import Data.Maybe

pureGen :: IO StdGen
pureGen =  mkStdGen
        .  floor
        .  toRational
        .  utctDayTime
       <$> getCurrentTime

roll :: IO Int
roll = getStdRandom $ randomR (0, 100)

data Guess = Start
           | Guessed Int

guess :: Int -> Guess -> IO ()
guess r g = do
    case g of
        Start -> do
            mnum <- fmap fst . listToMaybe . reads <$> getLine
            case mnum of
                Nothing -> do
                    putStrLn "Bad input! Enter a number 0-100"
                    guess r Start
                Just num -> guess r (Guessed num)
        Guessed n -> do
            res <- guessIt n r
            if res
                then return ()
                else guess r Start

    where
        guessIt n rng
            | n == rng = do
                putStrLn "You guessed right!"
                return True
            | n > rng  = do
                putStrLn "You're over!"
                return False
            | n < rng  = do
                putStrLn "You're under!"
                return False

game :: IO ()
game = do
    putStrLn "Enter a valid number"
    r <- roll
    guess r Start

main :: IO ()
main = undefined
