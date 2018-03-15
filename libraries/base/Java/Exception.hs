{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
             DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses,
             ScopedTypeVariables, FlexibleInstances, UndecidableInstances,
             BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Execption
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Execption utilities
--
-----------------------------------------------------------------------------

module Java.Exception where

import GHC.Base
import GHC.Int
import Java
import Java.Array
import GHC.Show
import GHC.Exception
import qualified System.IO.Error as SysIOErr
import Data.Typeable (Typeable, cast)
import Data.List (any, elem, isSubsequenceOf)
import Data.Either

data {-# CLASS "java.lang.StackTraceElement[]" #-} StackTraceElementArray = StackTraceElementArray (Object# StackTraceElementArray)
  deriving Class

instance JArray StackTraceElement StackTraceElementArray

-- Start java.lang.Throwable

data {-# CLASS "java.lang.Throwable" #-} Throwable = Throwable (Object# Throwable)
  deriving Class

foreign import java unsafe fillInStackTrace :: (a <: Throwable) => Java a a

foreign import java unsafe getCause :: (a <: Throwable, b <:Throwable) => Java a b

foreign import java unsafe getLocalizedMessage :: (a <: Throwable) => Java a String

foreign import java unsafe getMessage :: (a <: Throwable) => Java a String

foreign import java unsafe getStackTrace ::  (a <: Throwable) => Java a StackTraceElementArray

foreign import java unsafe initCause :: (a <: Throwable, b <:Throwable) => a -> Java b b

foreign import java unsafe printStackTrace ::  (a <: Throwable) => Java a ()

foreign import java unsafe setStackTrace ::  (a <: Throwable) => StackTraceElementArray -> Java a ()

-- End java.lang.Throwable

-- Start java.lang.StackTraceElement

data {-# CLASS "java.lang.StackTraceElement" #-} StackTraceElement = StackTraceElement (Object# StackTraceElement)
  deriving Class

foreign import java unsafe getClassName :: Java StackTraceElement String

foreign import java unsafe getFileName :: Java StackTraceElement String

foreign import java unsafe getLineNumber :: Java StackTraceElement Int

foreign import java unsafe getMethodName :: Java StackTraceElement String

foreign import java unsafe isNativeMethod :: Java StackTraceElement Bool

-- End java.lang.StackTraceElement

-- Start java.lang.Exception

data {-# CLASS "java.lang.Exception" #-} JException = JException (Object# JException)
  deriving (Class, Show, Typeable)

type instance Inherits JException = '[Throwable]

instance Exception JException

-- End java.lang.Exception

-- Start java.lang.Error

data {-# CLASS "java.lang.Error" #-} Error = Error (Object# Error)
  deriving Class

type instance Inherits Error = '[Throwable]

-- End java.lang.Error

-- Start java.lang.VirtualMachineError

data {-# CLASS "java.lang.VirtualMachineError" #-}
  VirtualMachineError = VirtualMachineError (Object# VirtualMachineError)
  deriving Class

type instance Inherits VirtualMachineError = '[Error]

-- End java.lang.VirtualMachineError

-- Start java.lang.InternalError

data {-# CLASS "java.lang.InternalError" #-}
  InternalError = InternalError (Object# InternalError)
  deriving Class

type instance Inherits InternalError = '[VirtualMachineError]

-- End java.lang.InternalError

-- Start java.io.IOException

data {-# CLASS "java.io.IOException" #-} IOException = IOException (Object# IOException)
  deriving (Class, Show, Typeable)

type instance Inherits IOException = '[JException]

data {-# CLASS "java.nio.file.FileSystemException" #-} FileSystemException = FileSystemException (Object# FileSystemException)
  deriving (Class, Show, Typeable)

type instance Inherits FileSystemException = '[IOException]

data {-# CLASS "java.io.FileNotFoundException" #-} FileNotFoundException =
  FileNotFoundException (Object# FileNotFoundException)
  deriving (Class, Show, Typeable)

type instance Inherits FileNotFoundException = '[IOException]

data {-# CLASS "java.nio.file.NoSuchFileException" #-} NoSuchFileException =
  NoSuchFileException (Object# NoSuchFileException)
  deriving (Class, Show, Typeable)

type instance Inherits NoSuchFileException = '[FileSystemException]

data {-# CLASS "java.io.EOFException" #-} EOFException =
  EOFException (Object# EOFException)
  deriving (Class, Show, Typeable)

type instance Inherits EOFException = '[IOException]

data {-# CLASS "java.nio.file.FileAlreadyExistsException" #-} FileAlreadyExistsException =
  FileAlreadyExistsException (Object# FileAlreadyExistsException)
  deriving (Class, Show, Typeable)

type instance Inherits FileAlreadyExistsException = '[FileSystemException]

data {-# CLASS "java.nio.channels.OverlappingFileLockException" #-} OverlappingFileLockException =
  OverlappingFileLockException (Object# OverlappingFileLockException)
  deriving (Class, Show, Typeable)

type instance Inherits OverlappingFileLockException = '[IllegalStateException]

class (e <: Throwable) => ToIOError e where
  toIOError :: e -> Maybe SysIOErr.IOError
  toIOError e = Nothing

instance (e <: Throwable) => ToIOError e

mkIOError :: (e <: Throwable) => SysIOErr.IOErrorType
          -> e -> SysIOErr.IOError
mkIOError t e = SysIOErr.ioeSetErrorString
                (SysIOErr.mkIOError t "" Nothing Nothing) msg
  where msg = unsafePerformJavaWith e getMessage

instance ToIOError IOException where
  toIOError e = Just $ mkIOError type' e
    where type' | isDoesNotExistError  = SysIOErr.doesNotExistErrorType
                | isAlreadyInUseError  = SysIOErr.alreadyInUseErrorType
                | isAlreadyExistsError = SysIOErr.alreadyExistsErrorType
                | isPermissionError    = SysIOErr.permissionErrorType
                | isFullError          = SysIOErr.fullErrorType
                | otherwise            = SysIOErr.userErrorType
          msg = unsafePerformJavaWith e getMessage

          isDoesNotExistError =
            msg == "The system cannot find the path specified"
          isAlreadyInUseError = isSubsequenceOf msg
            ("The process cannot access the file " ++
             "because another process has locked a portion of the file" )
          isAlreadyExistsError = msg == "File already exists"
          isPermissionError = msg == "Permission denied"  
          isFullError = msg `elem` ["There is not enough space on the disk", -- windows
                                    "Not enough space",                      -- nix
                                    "Not space left on device"]              -- GCJ

instance ToIOError OverlappingFileLockException where
  toIOError = Just . mkIOError SysIOErr.alreadyInUseErrorType

instance ToIOError FileNotFoundException where
  toIOError = Just . mkIOError SysIOErr.doesNotExistErrorType

instance ToIOError NoSuchFileException where
  toIOError = Just . mkIOError SysIOErr.doesNotExistErrorType

instance ToIOError FileAlreadyExistsException where
  toIOError = Just . mkIOError SysIOErr.alreadyExistsErrorType

instance ToIOError EOFException where
  toIOError = Just . mkIOError SysIOErr.eofErrorType
  
toSomeException :: (a <: Throwable) => a -> SomeException
toSomeException ex = SomeException (JException (unsafeCoerce# (unobj ex)))

instance {-# OVERLAPPABLE #-} (Show a, Typeable a, a <: Throwable)
  => Exception a where
  toException x = case toIOError x of
    Nothing  -> toSomeException x
    Just ioErr -> toException ioErr
  fromException e = do
    jexception :: JException <- fromException e
    safeDowncast jexception
  {-# INLINE fromException #-}

showException :: SomeException -> Object# JString
showException e = s#
  where !(JS# s#) = toJString (displayException e)

-- Start java.lang.ReflectiveOperationException

data ReflectiveOperationException = ReflectiveOperationException @java.lang.ReflectiveOperationException
  deriving (Class, Typeable)

type instance Inherits ReflectiveOperationException = '[JException]

-- End java.lang.ReflectiveOperationException

-- Start java.lang.RuntimeException

data RuntimeException = RuntimeException @java.lang.RuntimeException
  deriving (Class, Typeable)

type instance Inherits RuntimeException = '[JException]

-- End java.lang.RuntimeException

-- Start of java.lang.IllegalStateException

data {-# CLASS "java.lang.IllegalStateException" #-} IllegalStateException =
  IllegalStateException (Object# IllegalStateException)
  deriving (Class, Show, Typeable)

type instance Inherits IllegalStateException = '[RuntimeException]

-- End of java.lang.IllegalstateException
