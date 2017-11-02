{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Rm
  ( rm, rm', sshRm, sshRm', sshRmCmd
  , sshRmDirContents, sshRmDirContents', sshRmDirContentsCmd
  , rmTxt
  )
where

import Prelude ( )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( (>>=), return )
import Data.Foldable        ( Foldable, concat )
import Data.Function        ( ($) )
import Data.Functor         ( fmap )
import Data.List.NonEmpty   ( NonEmpty( (:|) ), (<|) )
import GHC.Exts             ( toList )

-- data-default ------------------------

import Data.Default  ( def )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )

-- path --------------------------------

import Path  ( Abs, Dir, Path )

-- proclib -----------------------------

import ProcLib.CommonOpt.Busybox  ( AsBusyboxOpt, HasROUseBusybox
                                  , boolBBOpt, ifUseBusybox, useAsBusyboxOpts
                                  )
import ProcLib.Process            ( CmdOut_, CmdSpec( CmdSpec )
                                  , ExitVal( ExitVal ), StdStream( NoStream )
                                  , errorIfNonZero', system_, throwNotOkay
                                  )
import ProcLib.ShellPath          ( ShellPathFragment( SPathF, WildStar )
                                  , ShellPathLike
                                  , shellQuote
                                  )
import ProcLib.Types              ( HostName )

-- proclib-ssh -------------------------

import ProcLib.SSH        ( sshCmd, sshNoCapture )
import ProcLib.SSH.Error  ( AsSSHError )
import ProcLib.SSH.Opt    ( AsSSHOpt, SSHOpt )

-- text --------------------------------

import Data.Text  ( Text )

-- textconv ----------------------------

import Data.Text.Conv  ( toText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Rm.Opt  as  Rm
import qualified  ProcLib.Rm.Req  as  Req

import ProcLib.Rm.Error  ( AsRmError, RmError, RmSSHError, mkRmError )
import ProcLib.Rm.Opt    ( AsRmOpt, RmOpt, RmSSHOpt, rmOpts, mkOptsRmSSH )
import ProcLib.Rm.Req    ( HasRmReq )

--------------------------------------------------------------------------------

rmTxt :: HasRmReq α => ShellPathLike ζ => α -> NonEmpty ζ -> CmdSpec
rmTxt req paths =
  let mebbe = boolBBOpt req Req.rmBusyBoxOpt
      recurseArg  =  mebbe  Req.rmRecurse  "--recursive"  "-r"
      quotedPaths =  toList $ fmap shellQuote paths
      rmArgs = concat ([recurseArg, [ "--" ], quotedPaths ] :: [[Text]])
   in CmdSpec (req ^. Req.rmPath) rmArgs

-------------------------------------------------------------------------------

rmCmd :: (AsRmOpt ρ, AsBusyboxOpt ρ, ShellPathLike ζ, Foldable φ)
      => NonEmpty ζ -> φ ρ -> CmdSpec
rmCmd pths os = rmTxt (rmOpts os) pths

-------------------------------------------------------------------------------

rm :: (ShellPathLike ζ, AsRmOpt ρ, AsBusyboxOpt ρ, Foldable φ, AsRmError ε) =>
       NonEmpty ζ -> φ ρ -> CmdOut_ ε μ ()
rm paths opts = do
  let cmdspec = rmCmd paths opts
  exit <- system_ (ExitVal 0) def NoStream cmdspec
  errorIfNonZero' mkRmError cmdspec exit

----------------------------------------

rm' :: (ShellPathLike ζ, Foldable φ) =>
        NonEmpty ζ -> φ RmOpt -> CmdOut_ RmError μ ()
rm' = rm

-------------------------------------------------------------------------------

-- | formatted text cmd for ssh rm
sshRmCmd :: (ShellPathLike ζ, AsRmOpt ρ, AsSSHOpt ρ, AsBusyboxOpt ρ,
             Foldable φ) =>
            HostName -> NonEmpty ζ -> φ ρ -> CmdSpec
sshRmCmd h pths os = sshCmd h (rmTxt (rmOpts os) pths) os

-- | remove a directory's contents, but not he directory itself, on android
--   with busybox
sshRmDirContentsCmd :: HasROUseBusybox p =>
                       HostName -> Path Abs Dir -> p -> CmdSpec
sshRmDirContentsCmd h path o =
  let opts :: [RmSSHOpt]
      opts = ifUseBusybox o $( mkOptsRmSSH [ Rm.recurse, Rm.busybox ] )
                            $( mkOptsRmSSH [ Rm.recurse ] )
   in sshRmCmd h (pure $ SPathF (toText path) <| WildStar :| []) opts


-------------------------------------------------------------------------------

-- | common heart of sshRm', sshRmDirContents'
sshRmDo :: (AsRmError ε, AsSSHError ε,
            Foldable φ, AsSSHOpt ρ, AsBusyboxOpt ρ) =>
           HostName -> CmdSpec -> φ ρ -> CmdOut_ ε μ ()
sshRmDo h cmd opts = do
  ev <- sshNoCapture h cmd opts
  ev >>= throwNotOkay (mkRmError cmd)
  return ()

sshRm :: (ShellPathLike ζ, AsRmError ε, AsSSHError ε,
           AsRmOpt ρ, AsSSHOpt ρ, AsBusyboxOpt ρ, Foldable φ) =>
          HostName -> NonEmpty ζ -> φ ρ -> CmdOut_ ε μ ()
sshRm h pths os = sshRmDo h (rmCmd pths os) os

----------------------------------------

sshRm' :: (ShellPathLike ζ, Foldable φ) =>
          HostName -> NonEmpty ζ -> φ RmSSHOpt -> CmdOut_ RmSSHError μ ()
sshRm' = sshRm

----------------------------------------

-- | remove a directory's contents, but not the directory itself, on android
--   with busybox
sshRmDirContents :: (AsRmError ε, AsSSHError ε,
                      Foldable φ, AsSSHOpt ρ, AsBusyboxOpt ρ) =>
                     HostName -> Path Abs Dir -> φ ρ -> CmdOut_ ε μ ()
sshRmDirContents h path opts =
  sshRmDo h (sshRmDirContentsCmd h path (opts ^. useAsBusyboxOpts)) opts

sshRmDirContents' :: Foldable φ =>
                    HostName -> Path Abs Dir -> φ SSHOpt
                 -> CmdOut_ RmSSHError μ ()
sshRmDirContents' = sshRmDirContents

-- that's all, folks! ---------------------------------------------------------


