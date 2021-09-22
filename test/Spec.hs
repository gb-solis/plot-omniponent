import           CAS
import           Game
import           Plot

import           Data.List     (groupBy, nubBy, sortOn)
import           System.Random (mkStdGen)
import           Util          (compose2, (∘∘))

main :: IO ()
main = do
    let expr = Sum (Prod (Leaf Var) (Leaf Var)) (Leaf (Const (-1) )) -- x^2 - 1
    let expr2 = Sum (Exp (Leaf Var)) (Leaf (Const (-1))) -- exp x - 1
    let initGameState = GameState { points = 0,
                                    chooseExprs = [expr, expr2],
                                    plotIndex = 0,
                                    plotsLeft = 3,
                                    forceExit = False,
                                    plotOptions = stdPlotOptions,
                                    exprsUpTo = 2,
                                    allowedTerms = [Var, Const 1, Const (-1)],
                                    seed = mkStdGen 1337 }
    let upToExprs = genUpTo [Var] 2 :: [SymExpr]
    -- let dists = map (l2dist (-1,1) 0.01 expr) upToExprs
    -- print $ filter ((<= 1e-10) . abs . snd) $ zip upToExprs dists
    let grouped = groupBy ((<= 1e-6) ∘∘ l2dist (-1,1) 0.1) upToExprs
    let nubbed = nubBy ((<= 1e-6) ∘∘ l2dist (-1,1) 0.1) upToExprs
    let dists = map (l2dist (-1,1) 0.01 (nubbed !! 4) ) nubbed
    -- Let us test if the string and expression tree representations are
    -- (naturally) equivalent (categories)
    let prop_equivalence expr = either (const False) (equivalentQ expr) (parseSymExpr $ show expr)
        equivalentQ e1 e2 = l2dist (-1,1) 0.1 e1 e2 <= 1e-10
    mapM_ print $ zip upToExprs $ map prop_equivalence upToExprs
    return ()
