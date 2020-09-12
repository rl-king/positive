module Positive.Prelude
  ( module X,
    rightToMaybe,
    tshow,
    unlessM,
    whenM,
  )
where

import Control.Applicative as X ((<|>))
import Control.Category as X ((>>>))
import Control.Concurrent as X (forkIO, threadDelay)
import Control.Exception.Safe as X (SomeException, catch, throwIO, try, tryAny)
import Control.Monad as X (foldM, forM, forever, guard, join, unless, when, (<=<))
import Control.Monad.IO.Class as X
import Data.Aeson as X (FromJSON, ToJSON)
import Data.Bifoldable as X (biconcat)
import Data.Bifunctor as X
import Data.ByteString as X (ByteString)
import Data.Coerce as X (Coercible, coerce)
import Data.Either as X (isLeft, isRight, lefts, rights)
import Data.Foldable as X (for_, sequenceA_, traverse_)
import Data.Function as X (fix, (&))
import Data.Functor as X (void)
import Data.Functor.Contravariant as X ((>$<))
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.Int as X (Int32)
import Data.Kind as X (Type)
import Data.List as X (sortBy, sortOn)
import Data.List.NonEmpty as X (NonEmpty)
import Data.Maybe as X (catMaybes, fromMaybe)
import Data.Ord as X (Down (..))
import Data.Proxy as X (Proxy (..))
import Data.Text as X (Text, pack)
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
import Data.Traversable as X (for)
import Data.Vector as X (Vector)
import Data.Void as X (Void, absurd)
import Data.Word as X (Word8)
import Debug.Trace as X (traceShowM)
import GHC.Generics as X (Generic, Generic1)
import Numeric.Natural as X (Natural)
import Text.Read as X (readMaybe)
import Prelude as X hiding (log, undefined)

tshow :: Show a => a -> Text
tshow =
  pack . show

-- FROM https://github.com/ndmitchell/extra

-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t =
  ifM b t (pure ())

-- | Like 'unless', but where the test can be monadic.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b =
  ifM b (pure ())

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do c <- b; if c then t else f

rightToMaybe :: Either a b -> Maybe b
rightToMaybe e =
  case e of
    Left _ -> Nothing
    Right r -> Just r
