module WithState where

import System.Random ( randomRIO )
import Control.Monad.State.Lazy
import Data.Maybe

data GameRecord =
     GameRecord { rng            :: Int
                , guesses        :: Int
                , correctGuesses :: Int }

type GameState = StateT GameRecord IO

roll :: IO Int
roll = randomRIO (0, 100)

input :: GameState Int
input = do
    liftIO $ putStrLn "Enter a number from 0 - 100"
    num <- liftIO (fmap fst . listToMaybe . reads <$> getLine)
    maybe input return num

guess :: Bool -> GameState ()
guess True  = return ()
guess False = do
    n <- input
    s <- get
    compareNums (rng s) n s
    s' <- get
    if correctGuesses s' == 1
        then guess True
        else guess False

    where
        compareNums r n s
            | n == r = do
                liftIO $ putStrLn "Correct!"
                liftIO $ putStrLn $ "That took you this many tries: " ++ show (guesses s)
                r' <- liftIO roll
                put GameRecord { rng            = r'
                               , correctGuesses = correctGuesses s + 1
                               , guesses        = 0 }
            | n > r  = do
                liftIO $ putStrLn "You're over!"
                incGuess
            | n < r  = do
                liftIO $ putStrLn "You're under!"
                incGuess

        incGuess =
            modify (\s -> GameRecord { rng            = rng s
                                     , correctGuesses = correctGuesses s
                                     , guesses        = guesses s + 1 })

    -- if n ==
    --
emptyGame :: Int -> GameRecord
emptyGame r = GameRecord { rng            = r
                         , correctGuesses = 0
                         , guesses        = 0 }

game :: IO GameRecord
game = do
    r <- roll
    snd <$> runStateT (guess False) (emptyGame r)
