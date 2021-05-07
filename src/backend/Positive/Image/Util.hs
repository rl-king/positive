{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Image.Util where

import Positive.Prelude
import qualified System.FilePath.Glob as Glob
import System.FilePath.Posix as Path

-- FS
ensureUniqueFilename :: MonadIO m => FilePath -> m FilePath
ensureUniqueFilename filepath = do
  current <- liftIO $ Glob.glob (dropExtension filepath <> "*")
  pure $
    if null current
      then filepath
      else
        let toNumbers = read @Int . reverse . takeWhile isDigit . reverse . dropExtension
            preExt = case sortOn Down $ toNumbers <$> filter (/= filepath) current of
              n : _ -> "-" <> show (n + 1)
              _ -> "-1"
         in mconcat [dropExtension filepath, preExt, takeExtension filepath]
