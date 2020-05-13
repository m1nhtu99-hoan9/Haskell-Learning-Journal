import Data.Tuple

roll tuple1 tuple2 = ((snd tuple2, snd tuple1), (fst tuple2, fst tuple1))