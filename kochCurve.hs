import SOE

type Segment = (Point,Point)
type Vector  = (Int,Int)

fromSegment :: Segment -> Vector
fromSegment ((x1,y1), (x2,y2)) = (x2-x1,y2-y1)

fromVector :: Point -> Vector -> Segment
fromVector (x,y) (vx,vy) = ((x,y),(vx+x,vy+y))

segmentPoint1 :: Segment -> Point
segmentPoint1 ((x,y),_) = (x,y)

segmentPoint2 :: Segment -> Point
segmentPoint2 (_,(x,y)) = (x,y)

drawSegments :: Window -> [Segment] -> IO()
drawSegments _ [] = return ()
drawSegments w (x:xs) = do
  processDrawing w x
  drawSegments   w xs
  where processDrawing w (pt1, pt2) = do
        drawInWindow w $ withColor Blue (line pt1  pt2)

drawHelp :: Window -> Bool -> IO ()
drawHelp w b
  | b         = drawInWindow w $ withColor White (text (20,50) "+ to add, - to delete, q to quit, h to toggle help")
  | otherwise = return ()


divideSegment :: Segment -> [Segment]
divideSegment s   = [s1, s2, s3, s4]
  where orig      = segmentPoint1 s
        v@(vx,vy) = fromSegment s
        s1        = fromVector orig (x1,y1)
        s2        = fromVector (segmentPoint2 s1) (x2,y2)
        s3        = fromVector (segmentPoint2 s2) (x3,y3)
        s4        = fromVector (segmentPoint2 s3) (x4,y4)
        x1        = round $ fvx * (1/3)
        y1        = round $ fvy * (1/3)
        x2        = round $ fvx * (1/6) - 0.289 * fvy + (1/3)
        y2        = round $ fvx * 0.289 + (1/6) * fvy
        x3        = round $ fvx * (1/6) + 0.289 * fvy + (1/2)
        y3        = round $ fvx * (-0.289) + (1/6) * fvy + 0.289
        x4        = round $ (1/3) * fvx + (2/3)
        y4        = round $ (1/3) * fvy
        fvx       = fromIntegral vx
        fvy       = fromIntegral vy

kochCurve :: Window -> Int -> [Segment] -> IO ()
kochCurve _ _ [] = return ()
kochCurve w max lst
  | length lst < max = kochCurve w max $ foldl (\acc x -> acc ++ (divideSegment x)) [] lst
  | otherwise        = drawSegments w lst

--
main' :: Window -> Int -> Bool -> IO ()
main' w size b
  | size < 1  = main' w 1 b
  | otherwise = do clearWindow w
                   drawHelp w b
                   kochCurve w size [fromVector (0,400) (1000,0)]
                   k <- getKey w
                   case k of
                     '+'       -> main' w (size * 4) b
                     '-'       -> main' w (size `div` 4) b
                     'q'       -> return ()
                     'h'       -> main' w size (not b)
                     otherwise -> main' w size b

main = runGraphics (
    do w <- openWindow "Koch's curve" (1000,1000)
       main' w 0 True
       closeWindow w
    )
