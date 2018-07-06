-- |Methods for Kiwi's 'Expression' class, which represents linear expressions of the form @c + x1•f1 + x2•f2 + …@ to be solved by the constraint system.
-- Each @xn•fn@ is represented by a 'Kiwi.Raw.Types.Term', which can be created and manipulated using the "Kiwi.Raw.Term" module.
module Kiwi.Raw.Expression
  (
  -- * Creating new 'Expression's
    new

  -- * Accessor functions
  , getTerms, getConstant, getValue

  -- * Raw FFI bindings
  , kiwiExpression_new, kiwiExpression_free, kiwiExpression_getTerms, kiwiExpression_getTermCount, kiwiExpression_getConstant, kiwiExpression_getValue

  -- * Raw FFI binding helpers
  , wrapTermFillFunction, unwrapTermAddFunction
  ) where

import Data.Foldable (traverse_)
import Data.Void (Void)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (FunPtr, Ptr)
import Kiwi.Raw.Term (kiwiTerm_free)
import Kiwi.Raw.Types (Expression, ExpressionType, Term, TermType)


-- |Create a new 'Expression' given the 'Term's to sum along with independent constant.
new :: [Term] -> Double -> IO Expression
new terms constant = do
  fillTermsWrapper <- wrapTermFillFunction $ \ vector appendFun -> do
    let append = unwrapTermAddFunction appendFun
    traverse_ (append vector . unsafeForeignPtrToPtr) terms -- safe on account of touching each foreign pointer after the new call completes, below
  expr <- newForeignPtr kiwiExpression_free =<< kiwiExpression_new fillTermsWrapper constant
  traverse_ touchForeignPtr terms
  pure expr

-- |Get the 'Term's of an 'Expression'.
getTerms :: Expression -> IO [Term]
getTerms exprFp =
  withForeignPtr exprFp $ \ exprP -> do
    count <- kiwiExpression_getTermCount exprP
    termsP <- kiwiExpression_getTerms exprP
    mapM (newForeignPtr kiwiTerm_free) =<< peekArray (fromIntegral count) termsP

-- |Get the independent constant of an 'Expression'.
getConstant :: Expression -> IO Double
getConstant exprFp = withForeignPtr exprFp kiwiExpression_getConstant

-- |Get the last solved value of an 'Expression'.
getValue :: Expression -> IO Double
getValue exprFp = withForeignPtr exprFp kiwiExpression_getValue

-- |Type of a function given by the C code to a 'TermFillFunction' to add a new 'Term' to a @std::vector<Term>@ when initializing an 'Expression'.
type TermAddFunction = Ptr Void -> Ptr TermType -> IO ()
-- |Type of a function given to the C code which repeatedly calls the 'TermAddFunction' to fill out the terms of a new 'Expression'.
type TermFillFunction = Ptr Void -> FunPtr TermAddFunction -> IO ()
-- |Raw binding to @Expression::Expression(std::vector<Term>&, double)@
foreign import ccall safe kiwiExpression_new :: FunPtr TermFillFunction -> Double -> IO (Ptr ExpressionType)
-- |Wrapper factory for giving @'FunPtr' 'TermFillFunction'@ instances to the C code when calling 'kiwiExpression_new'.
foreign import ccall "wrapper" wrapTermFillFunction :: TermFillFunction -> IO (FunPtr TermFillFunction)
-- |Dynamic 'FunPtr' unwrapper for 'TermAddFunction's to allow a 'TermFillFunction' to call the C code which adds a term to the vector.
foreign import ccall "dynamic" unwrapTermAddFunction :: FunPtr TermAddFunction -> TermAddFunction
-- |Raw binding to @delete@ for 'Expression'.
foreign import ccall unsafe "&kiwiExpression_free" kiwiExpression_free :: FunPtr (Ptr ExpressionType -> IO ())
-- |Raw binding to @expr->terms().data()@ giving the storage @Term*@ inside the @std::vector<'Term'>@ of an 'Expression'.
foreign import ccall unsafe kiwiExpression_getTerms :: Ptr ExpressionType -> IO (Ptr (Ptr TermType))
-- |Raw binding to @expr->terms().size()@ giving the size of a @std::vector<'Term'>@ of an 'Expression'.
foreign import ccall unsafe kiwiExpression_getTermCount :: Ptr ExpressionType -> IO CInt
-- |Raw binding to @double Expression::constant() const@
foreign import ccall unsafe kiwiExpression_getConstant :: Ptr ExpressionType -> IO Double
-- |Raw binding to @double Expression::value() const@
foreign import ccall unsafe kiwiExpression_getValue :: Ptr ExpressionType -> IO Double

