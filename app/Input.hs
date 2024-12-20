{-# LANGUAGE TemplateHaskell #-}

module Input (withInput) where

import Language.Haskell.TH
import Control.Monad.IO.Class

withInput :: Int -> Q Exp
withInput str = do
  -- TODO
  let path = "input/" <> show str
  content <- liftIO $ readFile path
  pure $ LitE (StringL content)
