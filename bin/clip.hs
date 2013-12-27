#!/usr/bin/runghc
import Prelude ()
import KitchenSink

main = clipboard =<< fmap chomp . readFile =<< inHomeDir . (".clips" </>) =<< head <$> getArgs

