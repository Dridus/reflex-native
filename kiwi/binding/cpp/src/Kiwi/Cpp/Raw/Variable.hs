{-# LANGUAGE RecordWildCards #-}
-- |Methods of the Kiwi @Variable@ class, which are variables optimized by the constraint system.
module Kiwi.Cpp.Raw.Variable
  (
  -- * Creating new 'Variable's
    new, adopt

  -- * Accessor functions
  , getName, getValue

  -- * Raw FFI bindings
  , kiwiVariable_new, kiwiVariable_free, kiwiVariable_getName, kiwiVariable_getValue
  ) where

import Control.Monad ((<=<), liftM)
import Data.ByteString.Char8 (packCString, useAsCString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Void (Void)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Kiwi.Cpp.Raw.Types (Variable(..), VariableType)


-- |Create a new Kiwi 'Variable' with the given name which can be empty. Other than being retrievable later via 'getName', the name is only used when dumping
-- the Kiwi solver state via 'Kiwi.Cpp.Raw.Solver.dump' or 'Show'ing.
new :: Text -> IO Variable
new nameT =
  useAsCString (encodeUtf8 nameT) $
    adopt <=< kiwiVariable_new

-- |Adopt a @'Ptr' 'VariableType'@ converting it into a 'Variable'. Variable is different than other bridged types because we collect and retain the name and
-- identity so we can do @Show@ and @Eq@ on variables, respectively.
adopt :: Ptr VariableType -> IO Variable
adopt varP = do
  _variable_ptr <- newForeignPtr kiwiVariable_free varP
  _variable_name <- getName' varP
  _variable_identity <- kiwiVariable_getIdentity varP
  pure $ Variable {..}

-- |Get the current name of a Kiwi 'Variable'.
getName :: Variable -> IO Text
getName variable =
  withForeignPtr (_variable_ptr variable) getName'

-- |Get the current name of a Kiwi @Variable@ when using its raw pointer.
getName' :: Ptr VariableType -> IO Text
getName' varP = liftM decodeUtf8 $ packCString =<< kiwiVariable_getName varP

-- |Get the current value of a Kiwi 'Variable'. Starts at 0.0 before a value is suggested or set by optimization.
getValue :: Variable -> IO Double
getValue variable =
  withForeignPtr (_variable_ptr variable) kiwiVariable_getValue

-- |Raw binding to @Variable::Variable(const char*)@.
foreign import ccall unsafe kiwiVariable_new :: CString -> IO (Ptr VariableType)
-- |Raw binding to @delete@ for @Variable@.
foreign import ccall unsafe "&kiwiVariable_free" kiwiVariable_free :: FunPtr (Ptr VariableType -> IO ())
-- |Raw binding to @const char* Variable::name() const@
foreign import ccall unsafe kiwiVariable_getName :: Ptr VariableType -> IO CString
-- |Raw binding to @double Variable::value() const@
foreign import ccall unsafe kiwiVariable_getValue :: Ptr VariableType -> IO Double
-- |Raw binding to @void* Variable::identity() const@
foreign import ccall unsafe kiwiVariable_getIdentity :: Ptr VariableType -> IO (Ptr Void)
