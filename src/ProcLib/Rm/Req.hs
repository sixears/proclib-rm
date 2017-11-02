{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Rm.Req
  ( RmReq, HasRmReq, rmReqDefault, rmPath, rmRecurse, rmBusyBoxOpt )
where

import Prelude ( )

-- base --------------------------------

import Data.Bool   ( Bool( False ) )
import Data.Eq     ( Eq )
import Text.Show   ( Show )

-- lens --------------------------------

import Control.Lens.TH  ( makeClassy )

-- path --------------------------------

import Path  ( Abs, File, Path )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Rm.Paths  as  Paths

-------------------------------------------------------------------------------

data RmReq = RmReq { _rmPath        :: Path Abs File
                   , _rmRecurse     :: Bool
                   , _rmBusyBoxOpt  :: Bool
                   }
  deriving (Eq, Show)

$( makeClassy ''RmReq )

----------------------------------------

rmReqDefault :: RmReq
rmReqDefault = RmReq { _rmPath        = Paths.rm
                     , _rmRecurse     = False
                     , _rmBusyBoxOpt  = False
                     }

-- that's all, folks! ---------------------------------------------------------
