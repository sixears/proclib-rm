{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Rm.Error
  ( AsRmError( _RmError ), RmError( RmError ), RmSSHError( RmE, SSHE )
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

import ProcLib.Process  ( CmdSpec, ExitVal )

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

data RmSSHError = RmE RmError | SSHE SSHError
  deriving (Eq, Show)

$( makePrisms ''RmSSHError )

instance AsRmError RmSSHError where
  _RmError = _RmE

instance AsSSHError RmSSHError where
  _SSHError = _SSHE

-- that's all, folks! ----------------------------------------------------------
