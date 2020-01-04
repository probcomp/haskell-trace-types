{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, TypeOperators, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, OverloadedLabels, InstanceSigs, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, RankNTypes, UndecidableInstances, ConstraintKinds, OverlappingInstances, TypeFamilyDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards, Rank2Types, ConstraintKinds #-}

module Main where

import TraceTypes
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Data.Row.Records

run = sampleIO . runWeighted

main :: IO ()
main = do (b, w) <- run . traceSampler $ sample #b (brn $ mkUnit 0.4)
          putStrLn (show b)
