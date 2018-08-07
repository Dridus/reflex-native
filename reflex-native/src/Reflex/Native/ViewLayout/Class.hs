{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Native.ViewLayout.Class
  ( ViewLayout(..)
  ) where

import Data.Default (Default)


class (Default (ContainerLayout layout t), Default (ContentLayout layout t)) => ViewLayout layout t where
  data ContainerLayout layout t :: *
  data ContentLayout layout t :: *
