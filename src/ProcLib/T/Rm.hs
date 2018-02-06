{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.T.Rm
  ( tests )
where

import Prelude ( )

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Function        ( ($) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ), (<|) )
import System.IO            ( IO )

-- path --------------------------------

import Path  ( mkAbsDir, toFilePath )

-- proclib -----------------------------

import ProcLib.CommonOpt.Busybox  ( UseBusybox( NoUseBusybox, UseBusybox ) )
import ProcLib.Process            ( CmdSpec( CmdSpec ) )
import ProcLib.ShellPath          ( ShellPathFragment( SPathF, WildStar ) )

-- proclib-ssh -------------------------

import qualified  ProcLib.SSH.Paths   as  SPaths

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- text --------------------------------

import Data.Text  ( Text, pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Rm.Paths  as  Paths
import qualified  ProcLib.Rm.Opt    as  Rm

import ProcLib.Rm                 ( sshRmCmd, sshRmDirContentsCmd )
import ProcLib.Rm.Opt             ( mkOptsRmSSH )

-------------------------------------------------------------------------------

_test :: IO ()
_test = defaultMain tests

tests :: TestTree
tests = testGroup "ProcLib.Rm" [ sshRmTest ]

sshRmTest :: TestTree
sshRmTest =
  testGroup "sshRmTest"
    [ testCase "sshRmCmd" $
          sshRmCmd "nohost" (pure ("/tmp/remove_me"::Text))
                   ($(mkOptsRmSSH [ Rm.recurse, Rm.busybox ]))
      @?= CmdSpec SPaths.ssh
                  [ "nohost", "-p", "2222", "-l", "root", "--"
                  , "/system/xbin/rm", "-r", "--", "/tmp/remove_me" ]
    , testCase "sshRmCmd/" $
          sshRmCmd "nohost" (pure ("/tmp/remove_me/*"::Text))
                   ($(mkOptsRmSSH [ Rm.recurse, Rm.busybox ]))
      @?= CmdSpec SPaths.ssh
                  [ "nohost", "-p", "2222", "-l", "root", "--"
                  , "/system/xbin/rm", "-r", "--", "/tmp/remove_me/\\*" ]
    , testCase "sshRmCmd*" $
          sshRmCmd "nohost" (pure $ SPathF "/tmp/remove_me/" <| WildStar :| [])
                   ($(mkOptsRmSSH [ Rm.recurse, Rm.busybox ]))
      @?= CmdSpec SPaths.ssh
                  [ "nohost", "-p", "2222", "-l", "root", "--"
                  , "/system/xbin/rm", "-r", "--", "/tmp/remove_me/*" ]

    , testCase "sshRmDirContents (BB)" $
          sshRmDirContentsCmd "nohost" $(mkAbsDir "/tmp/remove_me") UseBusybox
      @?= CmdSpec SPaths.ssh
                  [ "nohost", "-p", "2222", "-l", "root", "--"
                  , "/system/xbin/rm", "-r", "--", "/tmp/remove_me/*" ]

    , testCase "sshRmDirContents (no BB)" $
         sshRmDirContentsCmd "nohost" $(mkAbsDir "/tmp/remove_me") NoUseBusybox
      @?= CmdSpec SPaths.ssh
                  [ "nohost"
                  , "--", pack (toFilePath Paths.rm), "--recursive"
                  , "--", "/tmp/remove_me/*"
                  ]

    ]

-- that's all, folks! ---------------------------------------------------------


