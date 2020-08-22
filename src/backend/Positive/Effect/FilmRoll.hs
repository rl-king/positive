{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Positive.Effect.FilmRoll where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Throw
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import Positive.Prelude
import Positive.Settings
import System.Directory
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path

data FilmRoll (m :: Type -> Type) k where
  Read :: FilmRoll m FilmRollSettings

read :: Has FilmRoll sig m => m FilmRollSettings
read =
  send Read

newtype FilmRollC m a = FilmRollC {unFilmRollC :: ReaderC Dir m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m, Has (Throw String) sig m) => Algebra (FilmRoll :+: sig) (FilmRollC m) where
  alg hdl sig ctx =
    case sig of
      R other ->
        FilmRollC (alg (unFilmRollC . hdl) (R other) ctx)
      L Read ->
        fmap (<$ ctx) . either throwError pure =<< sendIO . getSettingsFile =<< FilmRollC ask

runFilmRoll :: Dir -> FilmRollC m a -> m a
runFilmRoll dir =
  runReader dir . unFilmRollC

--

getSettingsFile :: Dir -> IO (Either String FilmRollSettings)
getSettingsFile (Dir dir) = do
  let path = Text.unpack dir </> "image-settings.json"
  exists <- doesPathExist path
  if exists
    then Aeson.eitherDecodeFileStrict path
    else do
      filenames <- getAllPngs $ Text.unpack dir
      let settings = fromFilenames filenames
      ByteString.writeFile path $ Aeson.encode settings
      pure (Right settings)

getAllPngs :: FilePath -> IO [Text]
getAllPngs dir =
  fmap Text.pack . filter ((".png" ==) . Path.takeExtension) <$> listDirectory dir
