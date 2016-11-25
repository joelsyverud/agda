{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}

module Agda.Compiler.Dump.Compiler where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.IO.Class

import Data.Function
import qualified Data.List as List

import Agda.Syntax.Common
import Agda.Syntax.Literal
import Agda.Syntax.Position
import Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Abstract.Pretty as A
import Agda.Syntax.Internal as I
import Agda.Syntax.Internal.Generic

import Agda.TypeChecking.Monad as I
import Agda.TypeChecking.Monad.Builtin
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Substitute
import Agda.Syntax.Translation.InternalToAbstract

import Agda.Compiler.Common

import Agda.Utils.HashMap (HashMap)
import qualified Agda.Utils.HashMap as HashMap
import Agda.Utils.Functor
import Agda.Utils.List
import Agda.Utils.Monad
import Agda.Utils.Pretty (render, prettyShow)
import Agda.Utils.Singleton

#include "undefined.h"
import Agda.Utils.Impossible

compilerMain :: Interface -> TCM ()
compilerMain i = withScope_ (iInsideScope i) $ withShowAllArguments $ do --  disableDisplayForms $ do
  let (Sig secs defs rews) = iSignature i
  forM_ (HashMap.toList defs) $ \ (q, def) -> do
    let t = defType def
    doc <- prettyTCM q <+> text ":" <+> prettyTCM t
    liftIO $ putStrLn $ render doc
  forM_ (HashMap.toList defs) $ \ (q, def) -> do
    let t = defType def
    liftIO $ putStrLn $ show q ++ " : " ++ show t
