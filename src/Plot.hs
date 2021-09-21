{-# LANGUAGE RecordWildCards #-}
module Plot where

import           CAS

-- | Specifies the Continuity of the line in the plot
data LineContinuity = Discrete | PartialCont | TotalCont deriving (Eq, Show)

data PlotOptions = PlotOptions { deltaX         :: Float,
                                 deltaY         :: Float,
                                 minSlotsX      :: Int,
                                 maxSlotsX      :: Int,
                                 minSlotsY      :: Int,
                                 maxSlotsY      :: Int,
                                 lineContinuity :: LineContinuity,
                                 pointChar      :: Char }
                                 deriving (Eq, Show)

stdPlotOptions :: PlotOptions
stdPlotOptions = PlotOptions { deltaX = 0.1,
                               deltaY = 0.1,
                               minSlotsX = -50,
                               maxSlotsX = 50,
                               minSlotsY = -20,
                               maxSlotsY = 20,
                               lineContinuity = PartialCont,
                               pointChar = '█' }


plotASCII :: SymExpr -> PlotOptions -> String
plotASCII se PlotOptions {..} = concatMap (printLine (se $|)) coordsList where
    coordsList = [[(slotX * deltaX, slotY * deltaY) | slotX <- map fromIntegral [minSlotsX..maxSlotsX]] | slotY <- reverse $ map fromIntegral [minSlotsY..maxSlotsY]]
    printLine f c = map (printCoord f) c ++ ['\n']
    isHit f (x, y) = case lineContinuity of
        Discrete -> abs (midPoint - y) <= deltaY/2
        PartialCont -> signum(maxY - y - deltaY/2) * signum(y - deltaY/2 - minY) >= 0
                    || (maxY >= y + deltaY/2 && abs(minY - y) <= deltaY/2)
        TotalCont -> minY <= y + deltaY/2 && y - deltaY/2 <= maxY
        where
            leftY = f (x - deltaX/2)
            rightY = f (x + deltaX/2)
            midPoint = (leftY + rightY) / 2
            minY = min leftY rightY
            maxY = max leftY rightY
    printCoord f (x, y)
        | isHit f (x, y) = pointChar
        | y == 0 && x == 0 = '+'
        | y == 0 && abs(x - fromInteger (round x)) < deltaX/2 = last $ show (round x :: Int)
        | y == 0 = '-'
        | x == 0 && abs(y - fromInteger (round y)) < deltaY/2 = last $ show (round y :: Int)
        | x == 0 = '|'
        | otherwise = ' '

plot :: PlotOptions -> SymExpr -> IO ()
plot po se = putStr $ plotASCII se po
