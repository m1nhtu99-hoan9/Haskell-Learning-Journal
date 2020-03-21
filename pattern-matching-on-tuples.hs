module PatMatchTuples where 

  -- the broadest match
  swap :: (t1,t2) -> (t3,t4) -> ((t4, t2), (t3, t1))
  swap (a, b) (c, d) = ((d, b), (c, a))

  swapForThree :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
  swapForThree (v1, _, v3) (y1, _, y3) = ((v1, y1), (v3, y3))