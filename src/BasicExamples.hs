{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, TypeOperators, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, OverloadedLabels, InstanceSigs, FlexibleContexts, FlexibleInstances, RankNTypes, UndecidableInstances, ConstraintKinds, TypeFamilyDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, Rank2Types, ConstraintKinds #-}

module BasicExamples where

import TraceTypes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted (runWeightedT)
import Control.Monad.Bayes.Sampler.Lazy (runSamplerTIO)
import Data.Row.Records (Rec, (.==), type (.==), (.!), (.+), type (.+),  Empty)
import Data.Row (Label)
import GHC.OverloadedLabels (fromLabel)
import Data.Row.Internal (LT(..), Row(..))
import qualified Data.Vector.Sized as V
import Data.Vector.Sized (Vector)
import Prelude hiding ((>>=), (>>), return)
import Numeric.Log
import Control.Monad.Bayes.Inference.SMC (smcPush, SMCConfig(..))
import Control.Monad.Bayes.Population (resampleMultinomial, evidence)


f >>= g = pBind f g

return :: Monad m => a -> P m Empty a
return = pReturn

f >> g = f >>= \_ -> g

-------------------------------
-------------------------------
--                           --
--  EXAMPLES FROM FIGURE 1   --
-- Valid & Invalid Inference --
--                           --
-------------------------------
-------------------------------


-- weight_model :: MonadMeasure m => P m ("weight" .== Log Double .+ "measurement" .== Double) Double
weight_model = do
  w <- sample #weight (gmma 2 1)
  sample #measurement (nrm (realToFrac w) 0.2)

obs = #measurement .== 0.5


-- Importance Sampling

-- q1 :: MonadMeasure m => P m ("weight" .== Unit) Unit
q1 = sample #weight unif

-- appliedQ1 :: Type Error
-- appliedQ1 = importance weight_model obs q1

-- q1' :: MonadMeasure m => P m ("weight" .== Log Double) (Log Double)
q1' = sample #weight (gmma 2 0.25)

-- appliedQ1' :: MonadMeasure m => m (Rec ("weight" .== Log Double))
appliedQ1' = importance weight_model obs q1'

-- MCMC

-- k1 :: MonadMeasure m => K m ("weight" .== Log Double) ("weight" .== Log Double)
k1 = mh (\t -> sample #weight (truncnrm (t .! #weight) 0.2))

-- k2 :: MonadMeasure m => K m ("weight" .== Log Double) ("weight" .== Log Double)
k2 = mh (\t -> sample #weight (truncnrm (t .! #weight) 1.0))

-- shouldUseK1 :: Rec ("weight" .== Log Double) -> Bool
shouldUseK1 t = t .! #weight <= 2

-- k :: Type Error
-- k = seqK (ifK shouldUseK1 k1)
--          (ifK (not . shouldUseK1) k2)

-- k' :: MonadMeasure m => K m ("weight" .== Log Double) ("weight" .== Log Double)
k' = mh (\t -> let w = t .! #weight in 
                 sample #weight (truncnrm w (if w < 2 then 0.2 else 1)))

-- appliedK' :: MonadMeasure m => Rec ("weight" .== Log Double) -> m (Rec ("weight" .== Log Double))
appliedK' = k' (observe weight_model obs)

-- SVI

-- q2 :: MonadMeasure m => (Double, Log Double) -> P m ("weight" .== Double) Double
q2 (a, b) = sample #weight (nrm a b)

-- appliedQ2 :: Type Error
-- appliedQ2 = svi weight_model obs q2

-- q2' :: MonadMeasure m => (Log Double, Log Double) -> P m ("weight" .== Log Double) (Log Double)
q2' (a, b) = sample #weight (truncnrm a b)

-- appliedQ2' :: MonadMeasure m => (Log Double, Log Double) -> m (Log Double, Log Double)
appliedQ2' = svi weight_model obs q2'




------------------------------
------------------------------
--                          --
--  EXAMPLES FROM FIGURE 3  --
--  Control Flow Constructs --
--                          --
------------------------------
------------------------------


-- simple1 :: MonadMeasure m => P m ("x" .== Double .+ "z" .== Double) Double
simple1 = do
  x <- sample #x (nrm 0 1)
  z <- sample #z (nrm x 1)
  return $ x + z

-- simple2 :: MonadMeasure m => P m ("x" .== Double .+ "z" .== Double) Double
simple2 = do
  z <- sample #z (nrm 0 1)
  x <- sample #x (nrm z 1)
  return $ x + z

-- branch1 :: MonadMeasure m => P m ("b" .== Bool .+ "p" .== Unit .+ "coin" .== Bool) Bool
branch1 = do 
  isBiased <- sample #b (brn $ mkUnit 0.1)
  p <- if isBiased then 
         sample #p (bta 1 2) 
       else 
         sample #p (unif)
  sample #coin (brn p)

-- branch2 
--   :: MonadMeasure m 
--   => P m ("p" .== Either (Rec ("isLow" .== Bool)) (Rec Empty) .+ "coin" .== Bool) Bool
branch2 = do 
  p <- withProbability #p (mkUnit 0.1) (do {
         isLow <- sample #isLow (brn (mkUnit 0.5));
         return (if isLow then (mkUnit 0.01) else (mkUnit 0.99))
       }) (return $ mkUnit 0.5)
  sample #coin (brn p)

-- loop1 :: MonadMeasure m => P m ("pts" .== [Rec ("x" .== Double .+ "y" .== Double)]) [Double]
loop1 = do
  pts <- forRandomRange #pts (pois 3) (\i -> do {
    x <- sample #x (nrm (fromIntegral i) 1);
    sample #y (nrm x 1)
  })
  return $ map (\y -> 2 * y) pts

-- loop2 :: MonadMeasure m => P m ("pts" .== Vector 3 (Rec ("y" .== Double))) [Double]
loop2 = do
  pts <- forEach #pts (V.generate fromIntegral :: Vector 3 Int) (\x -> do {
    sample #y (nrm (fromIntegral x) 1)
  })
  return $ map (\y -> 2 * y) (V.toList pts)



------------------------------
------------------------------
--                          --
--  EXAMPLES FROM FIGURE 4  --
--    End-to-End Example    --
--                          --
------------------------------
------------------------------

xs = V.generate (\i -> 
  case i of
    0 -> -3.0
    1 -> -2.0
    2 -> -1.0
    3 -> 0.0
    4 -> 1.0
    5 -> 2.0
    6 -> 3.0) :: Vector 7 Double


ys = V.generate (\i -> 
  case i of
    0 -> #y .== 3.1
    1 -> #y .== 1.8 
    2 -> #y .== 1.1 
    3 -> #y .== (-0.2) 
    4 -> #y .== (-1.2) 
    5 -> #y .== (-2.1) 
    6 -> #y .== (-3.0)) :: Vector 7 (Rec ("y" .== Double))

-- prior 
--   :: MonadMeasure m 
--   => P m ("noise" .== Log Double .+ "coeffs" .== [Rec ("c" .== Double)]) (Double -> Double, Log Double)
prior = do z <- sample #noise (gmma 1 1)
           terms <- forRandomRange #coeffs (geom $ mkUnit 0.4) (\i -> do {
             coeff <- sample #c (nrm 0 1);
             return (\x -> coeff * (x ^ i))
           })
           let f = \x -> foldr (\t y -> y + (t x)) 0 terms
           return (f, z)

-- p :: MonadMeasure m
--   => P m ("noise" .== Log Double .+ "coeffs" .== [Rec ("c" .== Double)] .+ "data" .== Vector 7 (Rec ("y" .== Double))) (Vector 7 Double)        
p = do (f, z) <- BasicExamples.prior
       forEach #data xs (\x -> sample #y (nrm (f x) z))


data NNResult = NN { nnPi :: Unit, nnMu :: Double, nnSig :: Log Double, nnNoise :: Log Double }
data QState = Q { qN :: Int, qResids :: Vector 7 Double, qNN:: NNResult }
nn = undefined :: Vector 384 Double -> Vector 7 (Double, Double) -> NNResult

-- q :: MonadMeasure m 
--   => (Vector 384 Double, Log Double)
--   -> Rec ("data" .== Vector 7 (Rec ("y" .== Double)))
--   -> P m ("noise" .== Log Double .+ "coeffs" .== [Rec ("c" .== Double)]) (Log Double)
q (theta, sigma) obs = 
  do let ys = V.map (\d -> d .! #y) (obs .! #data)
     let nnInputs n resids = V.zip (V.map (\x -> x^n) xs) resids
     let initialState = Q { qN = 0, qResids = ys, qNN = nn theta (nnInputs 0 ys) }
     finalState <- while #coeffs initialState (nnPi . qNN) (mkUnit 0.99) (\s -> do {
       c <- sample #c $ nrm (nnMu (qNN s)) (nnSig (qNN s));
       let newResids = V.map (\(x, y) -> y - c * (x ^ (qN s))) (V.zip xs (qResids s)) in
       return (Q {qN = qN s + 1, qResids = newResids, qNN = nn theta (nnInputs (qN s + 1) newResids)})
     })
     sample #noise (lognormal (nnNoise (qNN finalState)) sigma)

-- sampleCurve = sampleIO $ runWeightedT $ sampler $ traced p

-- j = trainAmortized p q

-- h = importance p (#data .== ys) prior






------------------------------
------------------------------
--                          --
--  HIDDEN MARKOV MODEL SMC --
--                          --
------------------------------
------------------------------



-- simple_hmm_init :: MonadMeasure m => P m ("init" .== Double) Double
simple_hmm_init = sample #init (nrm 0 10)

-- simple_hmm_trans :: MonadMeasure m => Double -> P m ("x" .== Double .+ "y" .== Double) Double
simple_hmm_trans state = do
  newState <- sample #x (nrm state 1)
  noisyObs <- sample #y (nrm newState 0.2)
  return newState

hmm_steps = [#y .== 3.2, #y .== 5.1, #y .== 6.8, #y .== 8.9, #y .== 10.3]

-- normalNormalPosterior :: MonadMeasure m => (Double, Double) -> Double -> Double -> D m Double
normalNormalPosterior (mu1, sigma1) sigma2 obs = 
  let var = 1 / ((1 / sigma1^2) + (1 / sigma2^2))
      mu = var * (mu1 / sigma1^2 + obs / sigma2^2)
   in nrm mu (Exp (log var / 2))

-- hmm_proposal :: MonadMeasure m => Rec ("y" .== Double) -> Double -> P m ("x" .== Double) Double
hmm_proposal o x = sample #x $ normalNormalPosterior (x, 1) 0.2 (o .! #y)

-- hmm_proposals :: MonadMeasure m => [(Rec ("y" .== Double), Double -> P m ("x" .== Double) Double)]
hmm_proposals = fmap (\o -> (o, hmm_proposal o)) hmm_steps

-- hmm_init_proposal :: MonadMeasure m => P m ("init" .== Double) Double
hmm_init_proposal = sample #init (nrm 0 10)

-- customInference :: Int -> IO (Log Double)
customInference n = runSamplerTIO $ evidence $ smcPush config $ 
  particleFilter simple_hmm_init hmm_init_proposal simple_hmm_trans hmm_proposals
  where config = SMCConfig
          { resampler = resampleMultinomial
          , numSteps = n
          , numParticles = 5
          }



