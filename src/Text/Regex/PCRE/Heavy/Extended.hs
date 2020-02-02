{-# LANGUAGE QuasiQuotes #-}

module Text.Regex.PCRE.Heavy.Extended
  ( module Text.Regex.PCRE.Heavy,
    myRe,
  )
where

import Language.Haskell.TH.Quote (QuasiQuoter)
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light (multiline, utf8)

myRe :: QuasiQuoter
myRe = mkRegexQQ [multiline, utf8]
