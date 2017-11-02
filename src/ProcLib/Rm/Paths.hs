{-# LANGUAGE QuasiQuotes #-}

module ProcLib.Rm.Paths where

import Path  ( Abs, File, Path, absfile )

rm :: Path Abs File
rm = [absfile|/nix/store/c07gdr6cm43j1cphadzafq185k711vx4-coreutils-8.26/bin/rm|]
