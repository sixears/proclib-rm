{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Rm.Error
  ( AsRmError( _RmError ), RmError, RmSSHError, RmCPError, RmSSHCPError
  , mkRmError )
where 

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( id )
import Data.Maybe         ( Maybe( Just ) )
import Text.Show          ( Show )

-- fluffy ------------------------------

import Fluffy.Lens  ( (##) )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism', prism' )
import Control.Lens.TH     ( makePrisms )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError )
import ProcLib.Types.CmdSpec          ( CmdSpec )
import ProcLib.Types.ExitVal          ( ExitVal )

-- proclib-ssh -------------------------

import ProcLib.SSH.Error  ( AsSSHError( _SSHError ), SSHError )

-------------------------------------------------------------------------------

data RmError = RmError CmdSpec ExitVal
  deriving (Eq, Show)

instance Exception RmError

class AsRmError ε where
  _RmError :: Prism' ε RmError

instance AsRmError RmError where
  _RmError = prism' id Just

mkRmError :: AsRmError ε => CmdSpec -> ExitVal ->  ε
mkRmError cmdspec ev = _RmError ## RmError cmdspec ev

------------------------------------------------------------

data RmSSHError = RmE RmError | SSHE SSHError
  deriving (Eq, Show)

$( makePrisms ''RmSSHError )

instance AsRmError RmSSHError where
  _RmError = _RmE

instance AsSSHError RmSSHError where
  _SSHError = _SSHE

------------------------------------------------------------

data RmCPError = RCRmE RmError | RCCPE CreateProcError
  deriving (Eq, Show)

$( makePrisms ''RmCPError )

instance AsRmError RmCPError where
  _RmError = _RCRmE

instance AsCreateProcError RmCPError where
  _CreateProcError = _RCCPE

------------------------------------------------------------

data RmSSHCPError = RSCRmE RmError | RSCSSHE SSHError | RSCCPE CreateProcError
  deriving (Eq, Show)

$( makePrisms ''RmSSHCPError )

instance AsRmError RmSSHCPError where
  _RmError = _RSCRmE

instance AsSSHError RmSSHCPError where
  _SSHError = _RSCSSHE

instance AsCreateProcError RmSSHCPError where
  _CreateProcError = _RSCCPE

-- that's all, folks! ----------------------------------------------------------
