#!/usr/bin/runghc
import KS

main = clipboard =<< chompFile =<< inHomeDir . (".clips" </>) =<< getArg

