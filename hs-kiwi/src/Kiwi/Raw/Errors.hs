{-# LANGUAGE LambdaCase #-}
-- |Representation of errors signalled by Kiwi when making Solver calls.
module Kiwi.Raw.Errors
  (
  -- * ADT representation
    KiwiError(..)
  -- * Raw FFI representation
  , ErrorStructType, ErrorType(..), peekKiwiError, withErroringCall
  ) where

import Data.ByteString.Char8 (packCString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr)


data KiwiError
  = KiwiError_UnsatisfiableConstraint
  | KiwiError_UnknownConstraint
  | KiwiError_DuplicateConstraint
  | KiwiError_UnknownEditVariable
  | KiwiError_DuplicateEditVariable
  | KiwiError_BadRequiredStrength
  | KiwiError_InternalSolverError Text
  deriving (Eq, Show)

data ErrorStructType

data ErrorType
  -- WARNING: keep in sync with hs-kiwi.cpp!
  = ErrorType_UnsatisfiableConstraint
  | ErrorType_UnknownConstraint
  | ErrorType_DuplicateConstraint
  | ErrorType_UnknownEditVariable
  | ErrorType_DuplicateEditVariable
  | ErrorType_BadRequiredStrength
  | ErrorType_InternalSolverError
  deriving (Bounded, Enum, Eq, Ord, Show)

peekKiwiError :: Ptr ErrorStructType -> IO (Maybe KiwiError)
peekKiwiError p
  | p == nullPtr = pure Nothing
  | otherwise =
    toEnum  . (fromIntegral :: CInt -> Int) <$> kiwiError_getType p >>= \ case
      ErrorType_UnsatisfiableConstraint -> pure . Just $ KiwiError_UnsatisfiableConstraint
      ErrorType_UnknownConstraint       -> pure . Just $ KiwiError_UnknownConstraint
      ErrorType_DuplicateConstraint     -> pure . Just $ KiwiError_DuplicateConstraint
      ErrorType_UnknownEditVariable     -> pure . Just $ KiwiError_UnknownEditVariable
      ErrorType_DuplicateEditVariable   -> pure . Just $ KiwiError_DuplicateEditVariable
      ErrorType_BadRequiredStrength     -> pure . Just $ KiwiError_BadRequiredStrength
      ErrorType_InternalSolverError     -> Just . KiwiError_InternalSolverError . decodeUtf8 <$> (packCString =<< kiwiError_getMessage p)

withErroringCall :: IO (Ptr ErrorStructType) -> IO (Either KiwiError ())
withErroringCall action = do
  errorP <- action
  peekKiwiError errorP >>= \ case
    Nothing -> pure $ Right ()
    Just e -> do
      kiwiError_free errorP
      pure $ Left e

foreign import ccall unsafe kiwiError_getType :: Ptr ErrorStructType -> IO CInt
foreign import ccall unsafe kiwiError_getMessage :: Ptr ErrorStructType -> IO CString
foreign import ccall unsafe kiwiError_free :: Ptr ErrorStructType -> IO ()

