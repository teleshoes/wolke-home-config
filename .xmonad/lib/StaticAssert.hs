{-# LANGUAGE TemplateHaskell #-}
-- http://stackoverflow.com/questions/6648764/compile-time-assertions-with-ghc-haskell
module StaticAssert where
import Control.Monad
import Language.Haskell.TH

staticAssert cond msg = unless cond (report True msg) >> return []

