module BMP where

import Data.Binary
import Data.Binary.Put
import Data.Bits
import Control.Monad

data RGB = RGB { r :: Word8, g :: Word8, b :: Word8 } deriving Show

data BMP = BMP {
    width  :: Word32,
    height :: Word32,
    pixels :: [RGB]
}

instance Binary BMP where
    put (BMP w h ps) =
        p16 0x4d42 >>
        p32 (54 + imgsz) >>
        p32 0 >> p32 54 >> p32 40 >>
        p32 w >> p32 h >>
        p16 1 >> p16 24 >> p32 0 >>
        p32 imgsz >>
        p32 0 >> p32 0 >> p32 0 >> p32 0 >>
        mapM_ put ps'
        where p16   = putWord16le
              p32   = putWord32le
              lnsz  = if (w * 3) .&. 0x0003 == 0 then w * 3
                         else ((w * 3) .|. 0x0003) + 1
              imgsz = lnsz * h
              ps'   = concat . reverse . pad . map concat . grp . tolists $ ps
                      where (nfit, npad) = quotRem (fromIntegral lnsz) 3
                            tolists      = map (\p -> [b p, g p, r p])
                            grp []       = []
                            grp xs       = take nfit xs : grp (drop nfit xs)
                            pad          = map (++ replicate npad (0::Word8))
