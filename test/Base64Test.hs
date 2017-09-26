{-# LANGUAGE OverloadedStrings #-}

module Base64Test where

import Data.Char (ord)
import Data.Monoid ((<>))
import Test.QuickCheck.Instances ()
import Test.Tasty ()
import Test.QuickCheck (quickCheck, collect, Property, classify, cover)

import qualified Data.ByteString as BL
import qualified Data.Set as S
import Codec.Binary.Base64 (encode)
import Text.Regex.Posix ((=~))

prop_sizeRatio :: BL.ByteString -> Bool
prop_sizeRatio b =
  BL.length (encode b) == 4 * ceiling ((fromIntegral (BL.length b) / 3) :: Double)

prop_endsWithPadding :: BL.ByteString -> Property
prop_endsWithPadding b =
  collect suffix $
  (encB =~ ("(^|[^=])" <> suffix <> "$"))              -- at end
    && not (encB =~ ("=[^=]" :: BL.ByteString))        -- only at end
  where
    encB :: BL.ByteString
    encB = encode b
    remainder :: Int
    remainder = fromIntegral $ BL.length b `rem` 3
    suffix :: BL.ByteString
    suffix = BL.replicate ((3 - remainder) `rem` 3) (fromIntegral $ ord '=')

prop_outputAlphabet :: BL.ByteString -> Property
prop_outputAlphabet b =
  cover (S.size used >= 63) 2 "cover full alphabet" -- fails if condition  (>= 63) is not met 2% times
  $ classify (S.size used >= 32) "half-alphabet"  -- accumulates the distribution
  $ classify (S.size used >= 63) "full-alphabet" -- accumulates the distribution
  $ used `S.isSubsetOf` allowed
  where
    used = S.fromList . BL.unpack $ encode b
    allowed = S.fromList . map (fromIntegral . ord) $
      ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['+', '/', '=']

main :: IO ()
main = do
  quickCheck prop_sizeRatio
  quickCheck prop_endsWithPadding
  quickCheck prop_outputAlphabet

main1 :: IO ()
main1 = main
