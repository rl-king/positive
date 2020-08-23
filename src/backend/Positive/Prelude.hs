module Positive.Prelude
  ( module X,
    tshow,
  )
where

import Control.Applicative as X ((<|>))
import Control.Category as X ((>>>))
import Control.Concurrent as X (forkIO, threadDelay)
import Control.Exception.Safe as X (SomeException, catch, throwIO, try, tryAny)
import Control.Monad as X (forever, guard, join, unless, when)
import Control.Monad.IO.Class as X
import Data.Aeson as X (FromJSON, ToJSON)
import Data.Bifoldable as X (biconcat)
import Data.Bifunctor as X
import Data.ByteString as X (ByteString)
import Data.Coerce as X (Coercible, coerce)
import Data.Either as X (isLeft, isRight, lefts, rights)
import Data.Foldable as X (for_, sequenceA_, traverse_)
import Data.Function as X ((&), fix)
import Data.Functor as X (void)
import Data.Functor.Contravariant as X ((>$<))
import Data.Int as X (Int32)
import Data.Kind as X (Type)
import Data.List as X (sortBy)
import Data.List.NonEmpty as X (NonEmpty)
import Data.Maybe as X (catMaybes, fromMaybe)
import Data.Proxy as X (Proxy (..))
import Data.Text as X (Text, pack)
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
import Data.Traversable as X (for)
import Data.Vector as X (Vector)
import Data.Void as X (Void, absurd)
import Debug.Trace as X (traceShowM)
import GHC.Generics as X (Generic, Generic1)
import Numeric.Natural as X (Natural)
import Text.Read as X (readMaybe)
import Prelude as X hiding (error, log, undefined)

tshow :: Show a => a -> Text
tshow =
  pack . show
