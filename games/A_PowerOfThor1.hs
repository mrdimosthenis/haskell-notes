module A_PowerOfThor1 where

  import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    -- ---
    -- Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.

    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position

    let solution = directions (lightx, lighty) (initialtx, initialty)

    loop solution

loop :: [Direction] -> IO ()
loop solution@(dir:restDirs) = do
    input_line <- getLine
    let remainingturns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.

    hPutStrLn stderr $ show remainingturns

    -- A single line providing the move to be made: N NE E SE S SW W or NW

    putStrLn $ show dir

    loop restDirs
loop _ = putStrLn "NO"

type Point = (Int, Int)

data Direction = N | NE | E | SE | S | SW | W | NW | NO deriving (Show, Eq)

move :: Direction -> Point -> Point
move N  (x, y) = (x    , y - 1)
move NE (x, y) = (x + 1, y - 1)
move E  (x, y) = (x + 1, y    )
move SE (x, y) = (x + 1, y + 1)
move S  (x, y) = (x    , y + 1)
move SW (x, y) = (x - 1, y + 1)
move W  (x, y) = (x - 1, y    )
move NW (x, y) = (x - 1, y - 1)
move NO (x, y) = (x    , y    )

isLightAtNorth :: Point -> Point -> Bool
isLightAtNorth light@(_ , y1) pos@(_ , y2) = y1 < y2

isLightAtEast  :: Point -> Point -> Bool
isLightAtEast  light@(x1, _ ) pos@(x2, _ ) = x1 > x2

isLightAtSouth :: Point -> Point -> Bool
isLightAtSouth light@(_ , y1) pos@(_ , y2) = y1 > y2

isLightAtWest  :: Point -> Point -> Bool
isLightAtWest  light@(x1, _ ) pos@(x2, _ ) = x1 < x2

whereToMove :: Point -> Point -> Direction
whereToMove light pos
    | isLightAtNorth light pos && isLightAtEast light pos = NE
    | isLightAtSouth light pos && isLightAtEast light pos = SE
    | isLightAtSouth light pos && isLightAtWest light pos = SW
    | isLightAtNorth light pos && isLightAtWest light pos = NW
    | isLightAtNorth light pos                            = N
    | isLightAtEast  light pos                            = E
    | isLightAtSouth light pos                            = S
    | isLightAtWest  light pos                            = W
    | otherwise                                           = NO

directions :: Point -> Point -> [Direction]
directions light pos = takeWhile (/= NO)
                       . map fst
                       . iterate (\(direction, point) ->
                          let newPoint = move direction point
                          in (whereToMove light newPoint, newPoint))
                       $ (whereToMove light pos, pos)
