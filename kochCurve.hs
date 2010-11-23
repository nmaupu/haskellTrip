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

kochDrawLines :: Window -> [(Segment,Segment,Segment,Segment)] -> IO ()
kochDrawLines w [] = return ()
kochDrawLines w (x:xs) = do
  processDrawing w x
  kochDrawLines w xs
  where processDrawing w ((pt1a,pt1b), (pt2a,pt2b), (pt3a,pt3b), (pt4a,pt4b)) = do
        drawInWindow w (withColor Blue (line pt1a pt1b))
        drawInWindow w (withColor Blue (line pt2a pt2b))
        drawInWindow w (withColor Blue (line pt3a pt3b))
        drawInWindow w (withColor Blue (line pt4a pt4b))

divideSegment :: Segment -> (Segment, Segment, Segment, Segment)
divideSegment s   = (s1,s2,s3,s4)
  where orig      = segmentPoint1 s
        v@(vx,vy) = fromSegment s
        s1        = fromVector orig (x1,y1)
        s2        = fromVector (segmentPoint2 s1) (x2,y2)
        s3        = fromVector (segmentPoint2 s2) (x3,y3)
        s4        = fromVector (segmentPoint2 s3) (x4,y4)
        x1        = round $ fvx * 0.333
        y1        = round $ fvy * 0.333
        x2        = round $ fvx * 0.167 - 0.289 * fvy + 0.333
        y2        = round $ fvx * 0.289 + 0.167 * fvy
        x3        = round $ fvx * 0.167 + 0.289 * fvy + 0.500
        y3        = round $ fvx * (-0.289) + 0.167 * fvy + 0.289
        x4        = round $ 0.333 * fvx + 0.667
        y4        = round $ 0.333 * fvy
        fvx       = fromIntegral vx
        fvy       = fromIntegral vy

kochDivideSegment :: Point -> Vector -> [(Segment,Segment,Segment,Segment)] -> [(Segment,Segment,Segment,Segment)]
kochDivideSegment orig v@(vx,vy) [] = kochDivideSegment orig v [divideSegment (fromVector orig v)]
kochDivideSegment orig v@(vx,vy) lst@((s1,s2,s3,s4):xs)
  | (length lst) >= maxSize = lst
  | otherwise               = kochDivideSegment orig v $ (divideSegment s1) : (divideSegment s2) : (divideSegment s3) : (divideSegment s4) : []
  where maxSize             = 2

kochCurve :: Window -> Point -> Vector -> IO ()
kochCurve w p v = do
  segs <- return $ kochDivideSegment p v []
  kochDrawLines w segs

main0 = runGraphics (
    do w <- openWindow "Koch's curve" (1000,1000)
       kochCurve w (300,300) (256,0)
       k <- getKey w
       closeWindow w
  )

