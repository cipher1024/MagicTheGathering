
module Main where

import           Pipes
import qualified Pipes.Prelude as P
import Control.Monad

data Action = DrawCard | DiscardRandomCard | Gain1Life
  deriving (Show,Eq)
data Card = Draw2InsteadOf1 | Discard1InsteadOfDrawing1 | WhenDiscardingGain1Life
  deriving (Show,Eq)
data PlayerAction = Player1 Action | Player2 Action
  deriving (Show,Eq)

applyCard :: Monad m => Card -> Pipe Action Action m ()
applyCard Draw2InsteadOf1 = do
  c <- await
  if c == DrawCard
    then yield DrawCard >> yield DrawCard
    else yield c
applyCard Discard1InsteadOfDrawing1 = do
  c <- await
  if c == DrawCard
    then yield DiscardRandomCard
    else yield c
applyCard WhenDiscardingGain1Life = do
  c <- await
  if c == DiscardRandomCard
    then yield DiscardRandomCard >> yield Gain1Life
    else yield c

oneTurn :: Monad m => [Card] -> Pipe Action Action m ()
oneTurn [] = cat
oneTurn (c:cs) = forever (applyCard c) >-> oneTurn cs

-- player1Cards :: [Card]
-- player1Cards = [Draw2InsteadOf1,Discard1InsteadOfDrawing1]

-- player2Cards :: [Card]
-- player2Cards = [Discard1InsteadOfDrawing1,WhenDiscardingGain1Life]
-- -- Player1 DiscardRandomCard
-- -- Player1 DiscardRandomCard

-- -- Player2 DiscardRandomCard
-- -- Player2 Gain1Life

-- -- Player1 DiscardRandomCard
-- -- Player1 DiscardRandomCard

-- -- Player2 DiscardRandomCard
-- -- Player2 Gain1Life

player1Cards :: [Card]
player1Cards = [Draw2InsteadOf1,Discard1InsteadOfDrawing1,WhenDiscardingGain1Life]

player2Cards :: [Card]
player2Cards = [Discard1InsteadOfDrawing1]
-- Player1 DiscardRandomCard
-- Player1 Gain1Life
-- Player1 DiscardRandomCard
-- Player1 Gain1Life

-- Player2 DiscardRandomCard

-- Player1 DiscardRandomCard
-- Player1 Gain1Life
-- Player1 DiscardRandomCard
-- Player1 Gain1Life

-- Player2 DiscardRandomCard

game :: Monad m => Int -> Producer PlayerAction m ()
game n =
  replicateM_ n ( (yield DrawCard >-> oneTurn player1Cards >-> P.map Player1) >>
                  (yield DrawCard >-> oneTurn player2Cards >-> P.map Player2) )

main :: IO ()
main = runEffect $ game 2 >-> P.print

-- Player1 DiscardRandomCard
-- Player1 DiscardRandomCard

-- Player2 DiscardRandomCard
-- Player2 Gain1Life

-- Player1 DiscardRandomCard
-- Player1 DiscardRandomCard

-- Player2 DiscardRandomCard
-- Player2 Gain1Life
