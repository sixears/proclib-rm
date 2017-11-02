{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Rm.Opt
  ( AsRmOpt, RmOpt, RmSSHOpt
  , rmOpts, mkOptsRm, mkOptsRmSSH

  , busybox -- re-exported from ProcLib.Opt for convenience
  , path, pathAndroid
  , recurse
  )
where

import Prelude ( id )

-- base --------------------------------

import Data.Bool          ( Bool( True ) )
import Data.Eq            ( Eq )
import Data.Foldable      ( Foldable, foldr )
import Data.Function      ( (.) )
import Data.Maybe         ( Maybe( Just, Nothing ) )
import Text.Show          ( Show )

-- fluffy ------------------------------

import Fluffy.Lens  ( prismMatch )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Setter  ( (.~) )
import Control.Lens.TH      ( makeClassyPrisms, makePrisms )

-- path --------------------------------

import Path  ( Abs, File, Path, mkRelFile )

-- template-haskell --------------------

import Language.Haskell.TH         ( Exp
                                   , Q
                                   , appE, conE
                                   )
import Language.Haskell.TH.Syntax  ( Lift( lift ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.CommonOpt.Busybox  ( AsBusyboxOpt, BusyboxOpt( ANDROID_BUSYBOX )
                                 , _BusyboxOpt, androidBusyboxBin, busybox )
import ProcLib.Opt                ( Opt( cmpOpt, mkOpts )
                                 , (##), cmpOptDefaultLensed
                                 , cmpOptPrism, cmpOptPrisms, liftAbsFile
                                 )
import ProcLib.Rm.Req             ( RmReq

                                 , rmPath, rmRecurse, rmBusyBoxOpt

                                 , rmReqDefault
                                 )

import ProcLib.SSH.Opt            ( AsSSHOpt( _SSHOpt )
                                 , AsSSHPureOpt( _SSHPureOpt ), SSHOpt
                                 , _SSH_PURE_OPT
                                 )

-------------------------------------------------------------------------------

-- | options specific to rm (as opposed to common options)
data RmPureOpt = RM_RECURSE | RM_PATH (Path Abs File)
  deriving (Eq, Show)

$( makeClassyPrisms ''RmPureOpt )

instance Opt RmPureOpt where
  cmpOpt = cmpOptDefaultLensed "rm" [ (prismMatch _RM_PATH, "path") ] []

instance Lift RmPureOpt where
  lift RM_RECURSE  = conE 'RM_RECURSE
  lift (RM_PATH p) = liftAbsFile 'RM_PATH p

-------------------------------------------------------------------------------

androidPath :: Path Abs File
androidPath = androidBusyboxBin $( mkRelFile "rm" )

path :: AsRmPureOpt o => Path Abs File -> o
path p = _RM_PATH ## p

pathAndroid :: AsRmPureOpt o => o
pathAndroid = _RM_PATH ## androidPath

recurse :: AsRmPureOpt o => o
recurse = _RM_RECURSE ## ()

-------------------------------------------------------------------------------

data RmOpt = RM_PURE_OPT RmPureOpt | BUSYBOX_OPT BusyboxOpt
  deriving (Eq, Show)

$( makeClassyPrisms ''RmOpt )

instance Opt RmOpt where
  -- delegate opt comparisons to the constituents
  cmpOpt = cmpOptPrisms [ cmpOptPrism  _RM_PURE_OPT  RM_PURE_OPT
                        , cmpOptPrism  _BUSYBOX_OPT  BUSYBOX_OPT
                        ]

instance Lift RmOpt where
  lift (RM_PURE_OPT d) = appE (conE 'RM_PURE_OPT) (lift d)
  lift (BUSYBOX_OPT c) = appE (conE 'BUSYBOX_OPT)  (lift c)

instance AsRmPureOpt RmOpt where
  _RmPureOpt = _RM_PURE_OPT

instance AsBusyboxOpt RmOpt where
  _BusyboxOpt = _BUSYBOX_OPT

mkOptsRm :: [RmOpt] -> Q Exp
mkOptsRm = mkOpts

-------------------------------------------------------------------------------

-- | Apply a list of options to the default RmReq, to create a customized
--   rm request.  Note that earlier options take precedence over later options.
rmOpts :: (AsRmOpt o, AsBusyboxOpt o, Foldable t) => t o -> RmReq
rmOpts =
  foldr od rmReqDefault
  where optDecode :: RmPureOpt -> RmReq -> RmReq
        optDecode (RM_PATH p) = rmPath    .~ p
        optDecode RM_RECURSE  = rmRecurse .~ True

        busyboxOptDecode :: BusyboxOpt -> RmReq -> RmReq
        busyboxOptDecode ANDROID_BUSYBOX =
          (rmPath .~ androidPath) . (rmBusyBoxOpt .~ True)

        od :: (AsRmOpt o, AsBusyboxOpt o) => o -> RmReq -> RmReq
        od x = case x ^? _RmOpt of
                 Just x' -> case x' of
                              RM_PURE_OPT d -> optDecode d
                              BUSYBOX_OPT c -> busyboxOptDecode c
                 Nothing -> case x ^? _BusyboxOpt of
                              Just b  -> busyboxOptDecode b
                              Nothing -> id

-------------------------------------------------------------------------------

data RmSSHOpt = SSH_RM_OPT RmOpt | RM_SSH_OPT SSHOpt
  deriving (Eq, Show)

$( makePrisms ''RmSSHOpt )

instance Lift RmSSHOpt where
  lift (SSH_RM_OPT d) = appE (conE 'SSH_RM_OPT) (lift d)
  lift (RM_SSH_OPT s) = appE (conE 'RM_SSH_OPT) (lift s)

instance AsRmPureOpt RmSSHOpt where
  _RmPureOpt = _SSH_RM_OPT . _RM_PURE_OPT

instance AsBusyboxOpt RmSSHOpt where
  _BusyboxOpt = _SSH_RM_OPT . _BUSYBOX_OPT

instance AsSSHOpt RmSSHOpt where
  _SSHOpt = _RM_SSH_OPT

instance AsRmOpt RmSSHOpt where
  _RmOpt = _SSH_RM_OPT

instance AsSSHPureOpt RmSSHOpt where
  _SSHPureOpt = _RM_SSH_OPT . _SSH_PURE_OPT

instance Opt RmSSHOpt where
  -- delegate opt comparisons to the constituents
  cmpOpt = cmpOptPrisms [ cmpOptPrism _SSH_RM_OPT    SSH_RM_OPT
                        , cmpOptPrism _RM_SSH_OPT    RM_SSH_OPT
                        ]

mkOptsRmSSH :: [RmSSHOpt] -> Q Exp
mkOptsRmSSH = mkOpts


-- that's all, folks! ---------------------------------------------------------
