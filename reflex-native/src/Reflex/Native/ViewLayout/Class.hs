{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Native.ViewLayout.Class
  ( ViewLayout(..)
  ) where

import Data.Default (Default)


class (Default (ContainerLayout t layout), Default (ContentLayout t layout)) => ViewLayout t layout where
  data ContainerLayout t layout :: *
  data ContentLayout t layout :: *
