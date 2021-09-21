module Main where

import           CAS
import           Game
import           Plot
import           System.Random (getStdGen)
import           Control.Monad.Trans.State (execStateT)

main :: IO ()
main = do
    let expr = Sum (Prod (Leaf Var) (Leaf Var)) (Leaf (Const (-1) )) -- x^2 - 1
    stdSeed <- getStdGen
    let initGameState = GameState { points = 0,
                                    chooseExprs = [expr, expr, expr, expr],
                                    plotIndex = 0,
                                    plotsLeft = 3,
                                    forceExit = False,
                                    plotOptions = stdPlotOptions,
                                    exprsUpTo = 2,
                                    allowedTerms = [Var, Const 1, Const (-1)],
                                    seed = stdSeed }
    finalGameState <- execStateT gameLoop initGameState
    putStrLn $ "You got " ++ show (points finalGameState) ++ "/" ++ show (1 + plotsLeft initGameState) ++ " right!"
    return ()
