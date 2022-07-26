module Game where

import           CAS
import           Control.Monad.Extra       (whileM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, get, gets, modify)
import           Data.List                 (elemIndex, nubBy, sortOn)
import           Plot
import           System.Random             (StdGen, uniformR)
import           System.Random.Shuffle     (shuffle')

-- NEEDS A SMART CONSTRUCTOR
data GameState = GameState { points       :: Int,
                             chooseExprs  :: [SymExpr],
                             plotIndex    :: Int,
                             plotsLeft    :: Int,
                             forceExit    :: Bool,
                             plotOptions  :: PlotOptions,
                             exprsUpTo    :: Int,
                             allowedTerms :: [Term Float],
                             seed         :: StdGen }
                             deriving (Eq, Show)

-- | Determines if the game has *not* ended
isOn :: GameState -> Bool
isOn game = not (forceExit game) && plotsLeft game >= 0

nextQuestion :: GameState -> GameState
nextQuestion game = game{ plotIndex = newPlotIndex,
                          chooseExprs = newChooseExprs,
                          seed = newSeed
                        } where
    -- What follows here is a disgusting removal of equivalent expressions
    -- by calculating the L^2 distance between them
    allExprs = genUpTo (allowedTerms game) (exprsUpTo game)
    nubbedExprs = nubBy (\s1 s2 -> (l2dist (-1,1) 0.1 s1 s2) <= 1e-6) allExprs
    (plotExprInd, seed1) = uniformR (0, length nubbedExprs - 1) (seed game)
    plotExpr = nubbedExprs !! plotExprInd -- Chooses the expression to be plotted
    nExprs = length (chooseExprs game) - 1
    newOtherExprs = take nExprs $ tail $ sortOn distToPlotExpr nubbedExprs
        where distToPlotExpr = l2dist (-1,1) 0.1 plotExpr
    newShuffledOtherExprs = shuffle' newOtherExprs nExprs seed1 -- Shuffles the other options
    (newPlotIndex, newSeed) = uniformR (0, nExprs) seed1
    newChooseExprs = take newPlotIndex newShuffledOtherExprs
                  ++ [plotExpr]
                  ++ drop newPlotIndex newShuffledOtherExprs -- Arranges options

getChoice :: GameState -> IO Int
getChoice game = do
    choiceRaw <- getLine
    let choiceMaybe = elemIndex choiceRaw $ map show [0 .. length (chooseExprs game) - 1]
    case choiceMaybe of
        Just choice -> return choice
        Nothing -> do
            putStrLn $ choiceRaw ++ " is not a valid answer"
            getChoice game


getsM :: Monad m => (s -> m a) -> StateT s m a
getsM f = do
    s <- get
    lift (f s)

chooseExpr :: Int -> GameState -> GameState
chooseExpr p game =
    if plotIndex game == p
        then game {points = points game + 1, plotsLeft = plotsLeft game - 1}
        else game {plotsLeft = plotsLeft game - 1}

printExprs :: [SymExpr] -> IO ()
printExprs exprs = mapM_ putStrLn $ zipWith assembleQ [0..length exprs - 1] exprs where
    assembleQ n expr = show n ++ ") " ++ show expr

render :: GameState -> IO ()
render game = do
    plot (plotOptions game) (chooseExprs game !! plotIndex game)
    printExprs (chooseExprs game)

gameLoop :: StateT GameState IO ()
gameLoop = whileM $ do
    modify nextQuestion -- Generate plot and expressions
    getsM render -- Show plots and expressions to player
    choice <- getsM getChoice -- Player chooses the expression
    modify $ chooseExpr choice -- Checks if the choice is correct
    gets isOn -- Do we continue to the next round?
