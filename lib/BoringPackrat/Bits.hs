module BoringPackrat.Bits where

import Data.Bits (xor, (.&.), shiftL, shift)
import Data.Int
import Data.Word
import Data.Char (ord)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 as BU
import Data.List

-------------------------------------------------------------------------------
-- BString
--
type BString = B.ByteString

bsIndex = B.index
bsFromString = BU.fromString
bsLength = B.length

-------------------------------------------------------------------------------
-- Bits Ops
--
mask6, mask5, mask4, mask3, mask2, mask1 :: Word8
mask6 = 63
mask5 = 31
mask4 = 15
mask3 = 7
mask2 = 3
mask1 = 1

bin 0 n acc = (Prelude.take n (repeat 0)) ++ acc
bin x n acc = bin (div x 2) (n-1) $ (mod x 2):acc

bs n = Prelude.concat $ fmap show $ bin n 8 []

ibs ls = ibs' ls 0
  where
    ibs' [] acc = acc
    ibs' (l:ls) acc = ibs' ls $ (shift acc 1) + ord l - ord '0'

b10 :: Word8 -> Bool
b10 n = (n `xor` 64) .&. 192 == 192 -- extract with mask6
b110 n = (n `xor` 32) .&. 224 == 224 -- ... mask5
b1110 n = (n `xor` 16) .&. 240 == 240 -- ... mask4
b11110 n = (n `xor` 8) .&. 248 == 248 -- ... mask3


-------------------------------------------------------------------------------
-- UTF8 stuff
--
type BytePack = [Word8]
type WBytePack = (BytePack, Word8)

-- (bytesConsumed, totalConsumed)
consumeBits :: BString -> Int -> WBytePack
consumeBits word index
    | w <= 127 = ([fromIntegral w], 1) -- tail, value, consumed
    | b110 w = consumeUnicode word i' ([mask5 .&. w],1)
    | b1110 w = consumeUnicode word i' ([mask4 .&. w],1)
    | b11110 w = consumeUnicode word i' ([mask3 .&. w],1)
    | otherwise = ([], 0)
    where w = bsIndex word index
          i' = index + 1
  
consumeUnicode :: BString -> Int -> WBytePack -> WBytePack
consumeUnicode word index (accW,accA)
  | bsLength word <= index = result
  | b10 w = consumeUnicode word i' ((w .&. mask6):accW, accA + 1)
  | otherwise = result 
  where w = bsIndex word index  
        i' = index + 1
        result = (reverse accW, accA)

bytePackToInt :: BytePack -> Int32
bytePackToInt = foldl' (\acc cur -> acc `shiftL` 6 + fromIntegral cur) 0 
wBytePackToInt = bytePackToInt . fst
