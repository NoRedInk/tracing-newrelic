{-# LANGUAGE DeriveFunctor #-}
module FreeAfter (FreeAfter(..), freeAfter) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Prelude

-- | A type for performing operations and freeing memory that was used during
-- them once these operations are done.
newtype FreeAfter b a = FreeAfter { runFreeAfter :: (a -> IO b) -> IO b } deriving (Functor)

freeAfter :: FreeAfter a a -> IO a
freeAfter task = runFreeAfter task pure

instance Applicative (FreeAfter b) where
  pure x = FreeAfter $ \run -> run x
  FreeAfter f <*> FreeAfter x =
    FreeAfter $ \run -> f (\f' -> x (\x' -> run (f' x') ))

instance Monad (FreeAfter b) where
  FreeAfter x >>= f = FreeAfter $ \run ->
    x (\x' ->
        let
          (FreeAfter y) = f x'
        in y run
      )

instance MonadIO (FreeAfter b) where
  liftIO io = FreeAfter $ \f -> io >>= f
