module QuoteStr (quoteStr) where

import           Data.List                 (stripPrefix)
import           Language.Haskell.TH       (Exp (LitE), ExpQ, Lit (StringL))
import           Language.Haskell.TH.Quote (QuasiQuoter (..))

quoteStr :: QuasiQuoter
quoteStr =
    QuasiQuoter
        { quoteDec = const $ fail "quoteStr doesn't support declarations"
        , quotePat = const $ fail "quoteStr doesn't support patterns"
        , quoteType = const $ fail "quoteStr doesn't support types"
        , quoteExp = processString
        }

processString :: String -> ExpQ
processString ('\n' : xs) =
    let ws = takeWhile (' ' ==) xs
        cleanup "" = pure ""
        cleanup x = case stripPrefix ws x of
            Nothing -> fail "bad prefix"
            Just x' -> pure x'
     in LitE . StringL . unlines <$> traverse cleanup (lines xs)
processString _ = fail "malformed string literal"
