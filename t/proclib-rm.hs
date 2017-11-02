{-# LANGUAGE OverloadedStrings #-}

import Prelude ( )

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.T.Rm  as  Rm

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "proclib-rm" [ Rm.tests ]
