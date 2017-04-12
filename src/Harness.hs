{-
# Redistribution and use in source and binary forms, with or without modification, are permitted
# provided that the following conditions are met:
# 1.   Redistributions of source code must retain the above copyright notice, this list of
# conditions and the following disclaimer.
# 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
# conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
# OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Harness (runBenchmark) where

import System.Environment
import Data.Time

{- # INLINE runBenchmark # -}
runBenchmark :: ([String] -> IO (IO a, Maybe (a -> IO ()))) -> IO ()
runBenchmark buildIt = do
 args <- getArgs
 bench <- buildIt args
 t0 <- bench `seq` getCurrentTime
 putStrLn $ "Starting kernel"
 let (doIt, showIt) = bench
 b <- doIt
 t1 <- b `seq` getCurrentTime
 let td = diffUTCTime t1 t0
 putStrLn $ "Kernel time: " ++ (show td)
 case showIt of
   Just f   -> f b
   Nothing  -> return ()
