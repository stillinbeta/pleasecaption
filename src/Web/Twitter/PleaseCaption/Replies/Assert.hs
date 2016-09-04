module Web.Twitter.PleaseCaption.Replies.Assert (assertAllLength) where

import Control.Monad (foldM)
import Language.Haskell.TH

assertAllLength :: Int -> [String] -> ExpQ
--assertAllLength = return . ListE . map (LitE . StringL)
assertAllLength size = foldM (consOrFail size) (ListE [])
consOrFail :: Int -> Exp -> String -> ExpQ
consOrFail size (ListE xs) x
  | length x > size = fail $ show x ++ " is too long!"
  | otherwise = return $ ListE ((LitE $ StringL x):xs)
consOrFail _ _ _ = fail "unexpected element"
