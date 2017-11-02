{-# LANGUAGE QuasiQuotes #-}

module ProcLib.Rm.Paths where

import Path  ( Abs, File, Path, absfile )

rm :: Path Abs File
rm = [absfile|__coreutils__/bin/rm|]
