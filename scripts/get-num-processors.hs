#!/usr/bin/env stack
-- stack script --resolver lts-8.5 --package base
import GHC.Conc (getNumProcessors, getNumCapabilities)

main = do
  numProcessors <- getNumProcessors
  numCapabilities <- getNumCapabilities
  print ("numProcessors", numProcessors)
  print ("numCapabilities", numCapabilities)
