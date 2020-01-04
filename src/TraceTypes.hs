{-# LANGUAGE DataKinds, NamedFieldPuns, GADTs, KindSignatures, TypeFamilies, TypeOperators, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, OverloadedLabels, InstanceSigs, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, RankNTypes, UndecidableInstances, ConstraintKinds, OverlappingInstances, TypeFamilyDependencies #-}

module TraceTypes where

import           Data.Row.Records        hiding ( Disjoint
                                                , sequence
                                                , map
                                                , zip
                                                )
import           Data.Row.Internal              ( Subset
                                                , type (.\\)
                                                , type (.\/)
                                                )
import           Data.Default
import           Numeric.Log                   as Log
import qualified Data.Vector.Sized             as V
import           Data.Vector.Sized              ( Vector )
import           GHC.OverloadedLabels
import           Data.Finite
import           GHC.TypeLits
import           Control.Monad
import           Control.Monad.Bayes.Class     as Bayes
import Statistics.Distribution (logDensity, complCumulative)
import Statistics.Distribution.Gamma
import Statistics.Distribution.Beta
import Statistics.Distribution.Normal

-- The "Constraint" type is intended only
-- for internal use, and is not user-facing.
-- A Constraint of a certain record type is 
-- a record where each field type `a` has been
-- replaced with the type `Maybe a`; a value of
-- `Nothing` indicates an unconstrained value,
-- whereas `Just x` indicates an observed (i.e. 
-- constrained) value.
type Constraint r = Rec (Map Maybe r)

-- (ConcatTo l r s) means that l, r, and s are all
-- valid record types, and that l .+ r = r .+ l = s.
type ConcatTo l r s
  = ( WellBehaved l
    , WellBehaved r
    , WellBehaved s
    , Subset l s
    , Subset r s
    , (l .+ r) ≈ s
    , (r .+ l) ≈ s
    , s .\\ l ≈ r
    , s .\\ r ≈ l
    )

-- Are the label sets of l and r disjoint?
type Disjoint l r
  = ( ConcatTo l r (l .+ r)
    , ConcatTo (Map Maybe l) (Map Maybe r) (Map Maybe (l .+ r))
    , Forall (Map Maybe l) Default
    , Forall (Map Maybe r) Default
    )

-- Three main types: probability distributions (D), unnormalized density-carrying measures (U), 
-- and probabilistic programs (P).
data U m a = U
  { usampler :: m a
  , udensity :: a -> Log Double
  }
data D m a = D
  { dsampler :: m a
  , ddensity :: a -> Log Double
  }
data P m t a = P
  { traced    :: D m (Rec t)
  , retval    :: Rec t -> a
  , constrain :: Constraint t -> U m (Rec t)
  }

class DensityCarrying f m where
  sampler :: f m a -> m a
  density :: f m a -> a -> Log Double


instance DensityCarrying U m where
  sampler = usampler
  density = udensity

instance DensityCarrying D m where
  sampler = dsampler
  density = ddensity

dist sampler density = D { dsampler = sampler, ddensity = density }
udist sampler density = U { usampler = sampler, udensity = density }

-- DEFAULT INFERENCE
observe
  :: forall m a t s
   . (Disjoint t s, Monad m)
  => P m (t .+ s) a
  -> Rec t
  -> U m (Rec s)
observe p obs =
  let
    -- Turn the observation record `obs` of type `t` into a Constraint of 
    -- type `t .+ s`, by filling in the `s` piece of the record with `Nothing`.
      constraints :: Constraint (t .+ s)
      constraints = map' Just obs .+ (default' @Default def :: Constraint s)

      -- Obtain a constrained distribution over traces.
      u :: U m (Rec (t .+ s))
      u = constrain p constraints
  in 
    -- Modify the distribution u so that it is over only the unobserved values
    -- (Rec s), not full-but-constrained traces (Rec (t .+ s)).
      udist (fmap restrict $ sampler u) (density u . ((.+) obs))

traceSampler = sampler . traced
traceDensity = density . traced

-- UNIT INTERVAL TYPE

newtype Unit = Sigmoid {logit :: Double}

fromUnit :: Unit -> Double
fromUnit (Sigmoid x) = 1.0 / (1.0 + exp (-x))

mkUnit :: Double -> Unit
mkUnit u = if 0 < u && u < 1
  then Sigmoid (log (u / (1.0 - u)))
  else error "Number must be between 0 and 1."

instance Show Unit where
  show x = show (fromUnit x)

------------------------------
-- STPL LANGUAGE CONSTRUCTS --
------------------------------

-- RETURN

pReturn :: Monad m => a -> P m Empty a
pReturn x =
  let sampler = return Data.Row.Records.empty
      density _ = 1
      traced = dist sampler density
      constrain _ = udist sampler density
      retval _ = x
  in  P { traced, retval, constrain }


-- BIND

pBind :: (Disjoint t s, Monad m) => P m t a -> (a -> P m s b) -> P m (t .+ s) b
pBind f g =
  let
    bindSampler = do
      fTrace <- sampler $ traced f
      let x = retval f fTrace
      gTrace <- sampler $ traced (g x)
      return (fTrace .+ gTrace)

    bindDensity tr =
      let fDensity = density (traced f) (restrict tr)
          x        = retval f (restrict tr)
          gDensity = density (traced $ g x) (restrict tr)
      in  fDensity * gDensity

    bindTraced = dist bindSampler bindDensity

    bindRet tr = let x = retval f (restrict tr) in retval (g x) (restrict tr)

    constrainedSampler c = do
      fTrace <- sampler $ constrain f (restrict c)
      let x = retval f fTrace
      gTrace <- sampler $ constrain (g x) (restrict c)
      return (fTrace .+ gTrace)

    constrainedDensity c tr =
      let fDensity = density (constrain f (restrict c)) (restrict tr)
          x        = retval f (restrict tr)
          gDensity = density (constrain (g x) (restrict c)) (restrict tr)
      in  fDensity * gDensity

    bindConstrain c = udist (constrainedSampler c) (constrainedDensity c)
  in
    P { traced = bindTraced, retval = bindRet, constrain = bindConstrain }


-- SAMPLE

singleLabelConstrain
  :: (KnownSymbol l, MonadInfer m)
  => Label l
  -> D m (Rec (l .== a))
  -> Constraint (l .== a)
  -> U m (Rec (l .== a))
singleLabelConstrain lbl traced c = case c .! lbl of
  Just x ->
    let t                  = lbl .== x
        constrainedSampler = do
          score $ density traced t
          return t
        constrainedDensity _ = density traced t
    in  udist constrainedSampler constrainedDensity

  Nothing -> udist (sampler traced) (density traced)


sample :: (KnownSymbol l, MonadInfer m) => Label l -> D m a -> P m (l .== a) a
sample lbl d =
  let sampleTraced = dist
        (do
          x <- sampler d
          return (lbl .== x)
        )
        (\t -> density d (t .! lbl))

      sampleRet t = t .! lbl

      sampleConstrain = singleLabelConstrain lbl sampleTraced
  in  P { traced    = sampleTraced
        , retval    = sampleRet
        , constrain = sampleConstrain
        }


-- WITH PROBABILITY

withProbability
  :: (KnownSymbol l, MonadInfer m)
  => Label l
  -> Unit
  -> P m t a
  -> P m s a
  -> P m (l .== Either (Rec t) (Rec s)) a
withProbability lbl logit f g =
  let p         = fromUnit logit

      wpSampler = do
        r <- random
        if r < p
          then do
            t <- sampler $ traced f
            return (lbl .== Left t)
          else do
            t <- sampler $ traced g
            return (lbl .== Right t)

      wpDensity tr = case tr .! lbl of
        Left  t -> Exp (log p) * density (traced f) t
        Right t -> Exp (log (1.0 - p)) * density (traced g) t

      wpTraced = dist wpSampler wpDensity

      wpRet t = case t .! lbl of
        Left  t -> retval f t
        Right t -> retval g t

      wpConstrain = singleLabelConstrain lbl wpTraced
  in  P { traced = wpTraced, retval = wpRet, constrain = wpConstrain }

-- FOR EACH

forEach
  :: (KnownSymbol l, KnownNat n, MonadInfer m)
  => Label l
  -> Vector n a
  -> (a -> P m t b)
  -> P m (l .== Vector n (Rec t)) (Vector n b)
forEach lbl xs body =
  let
    forEachSampler = do
      traces <- V.mapM (sampler . traced . body) xs
      return (lbl .== traces)

    forEachDensity t = V.product
      (V.map (\(x, tr) -> density (traced $ body x) tr) (V.zip xs (t .! lbl)))

    forEachTraced = dist forEachSampler forEachDensity

    forEachRet t = V.map (\(x, tr) -> retval (body x) tr) (V.zip xs (t .! lbl))

    forEachConstrain = singleLabelConstrain lbl forEachTraced
  in
    P { traced    = forEachTraced
      , retval    = forEachRet
      , constrain = forEachConstrain
      }


-- FOR ... IN RANDOM RANGE ...

forRandomRange
  :: (KnownSymbol l, MonadInfer m)
  => Label l
  -> D m Int
  -> (Int -> P m t a)
  -> P m (l .==  [Rec t]) [a]
forRandomRange lbl d body =
  let
    -- Sample a trace of the for loop.
    forSampler = do
      n      <- sampler d
      traces <- sequence $ map (sampler . traced . body) [0 .. n - 1]
      return (lbl .== traces)

    -- Compute the density of a trace of the for loop.
    forDensity t =
      density d (length $ t .! lbl)
        * product
            (map (\(tr, i) -> density (traced $ body i) tr)
                 (zip (t .! lbl) [0 ..])
            )

    -- The for loop's density-carrying distribution over traces
    forTraced = dist forSampler forDensity

    -- Compute the return value of the for loop, given a trace of an execution.
    -- The return value is just a list of the return values from each iteration.
    forRet t = map (\(tr, i) -> retval (body i) tr) (zip (t .! lbl) [0 ..])

    -- Constrain the random choices made inside the for loop.
    forConstrain = singleLabelConstrain lbl forTraced
  in
    P { traced = forTraced, retval = forRet, constrain = forConstrain }

{- WHILE:

    A stochastic while loop.
    Takes as input:
    - lbl    : The label at which to trace the while loop's random choices.
    - init   : The initial state of the while loop.
    - pi     : A function computing a probability of continuing, based on the current state.
    - pi_max : An upper bound beyond which the probability of continuing is truncated.
    - body   : The body of the while loop, which simulates a new state based on the current state.

    The resulting probabilistic program has type P m (l .== [Rec t]) a:
    - The trace contains, at the specified label, a list of subtraces, one for each iteration.
    - The return value is the final state at the finish of the while loop.

 -}

while
  :: (KnownSymbol l, MonadInfer m)
  => Label l
  -> a
  -> (a -> Unit)
  -> Unit
  -> (a -> P m t a)
  -> P m (l .== [Rec t]) a
while lbl init pi pi_max body =
  let 
    -- Sample a trace of the while loop.
    whileSampler state = do
      shouldContinue <- bernoulli $ min (fromUnit (pi state)) (fromUnit pi_max)
      if shouldContinue
        then do
          nextTrace <- sampler (traced $ body state)
          let nextState = retval (body state) nextTrace
          restOfTraces <- whileSampler nextState
          return (lbl .== (nextTrace : (restOfTraces .! lbl)))
        else return (lbl .== [])

    -- Given traces from each iteration, and an initial state,
    -- compute the final state.
    retvalFromTraces init ts = case ts of
      []     -> init
      t : ts -> retvalFromTraces (retval (body init) t) ts

    -- Given a trace of the entire while loop, compute the final
    -- state.
    whileRet t = retvalFromTraces init (t .! lbl)

    -- Given an initial state and traces from each iteration,
    -- compute the density.
    densityFromTraces init ts = case ts of
        [] -> Exp (log (1.0 - (fromUnit (pi init))))
        t : ts ->
          Exp (log (fromUnit (pi init)))
            * (density (traced $ body init) t)
            * densityFromTraces (retval (body init) t) ts

    -- Given a trace of the entire while loop, compute the density.
    whileDensity t = densityFromTraces init (t .! lbl)

    -- The while loop's density-carrying distribution over traces.
    whileTraced    = dist (whileSampler init) whileDensity

    -- Constrain the choices made inside the loop.
    whileConstrain = singleLabelConstrain lbl whileTraced

  in  P { traced = whileTraced, retval = whileRet, constrain = whileConstrain }

-------------------
-- DISTRIBUTIONS --
-------------------

nrm :: MonadInfer m => Double -> Log Double -> D m Double
nrm mu (Exp sigln) = dist (normal mu (exp sigln)) (normalPdf mu (exp sigln))

brn :: MonadSample m => Unit -> D m Bool
brn logit =
  let p = fromUnit logit
  in  dist (bernoulli p) (\b -> Exp . log $ if b then p else (1.0 - p))

gmma :: MonadSample m => Log Double -> Log Double -> D m (Log Double)
gmma shape scale =
  let toDouble (Exp l) = exp l
      toLogDouble d = Exp (log d)
  in
    dist (fmap toLogDouble (gamma (toDouble shape) (toDouble scale))) 
         (Exp . (logDensity $ Statistics.Distribution.Gamma.gammaDistr 
                                (toDouble shape) (toDouble scale))
              . toDouble)

geom :: MonadSample m => Unit -> D m Int
geom p' = 
  let p = fromUnit p'
  in dist (geometric p)
          (\n -> Exp (fromIntegral n * (log p) + (log (1.0 - p))))

pois :: MonadSample m => Int -> D m Int
pois = undefined

bta :: MonadSample m => Log Double -> Log Double -> D m Unit
bta (Exp lna) (Exp lnb) = 
  let
    a = exp lna
    b = exp lnb
  in
    dist (fmap mkUnit (beta a b))
         (Exp . (logDensity $ Statistics.Distribution.Beta.betaDistr a b)
              . fromUnit)

unif :: MonadSample m => D m Unit
unif = dist (fmap mkUnit Bayes.random) (\x -> 1.0 :: Log Double)

cat :: (KnownNat n, MonadSample m) => Vector n Unit -> D m (Finite n)
cat probs = undefined

lognormal :: forall m. MonadInfer m => Log Double -> Log Double -> D m (Log Double)
lognormal (Exp logmu) sigma =
  dist (fmap Exp $ sampler $ nrm logmu sigma)
       (\(Exp logx) -> density (nrm logmu sigma :: D m Double) logx / (Exp logx))


truncnrm :: MonadInfer m => Log Double -> Log Double -> D m (Log Double)
truncnrm (Exp logmu) (Exp logsigma) =
  let mu = exp logmu
      sigma = exp logsigma
      unbounded = nrm mu (Exp logsigma)
      
      truncNormSampler = do
        x <- sampler unbounded
        if x < 0 then truncNormSampler else return (Exp $ log x)
      
      truncNormPdf (Exp logy) = 
        let y = exp logy
            z = complCumulative (normalDistr mu sigma) 0
        in (density unbounded y / (Exp (log z)))
  in dist truncNormSampler truncNormPdf



----------------------------
-- PROGRAMMABLE INFERENCE --
----------------------------

-- IMPORTANCE SAMPLING

importance
  :: (Disjoint t s, MonadInfer m)
  => P m (t .+ s) a
  -> Rec t
  -> P m s b
  -> m (Rec s)
importance p t q =
  let target = observe p t
  in  do
        tr <- sampler $ traced q
        score ((density target tr) / (density (traced q) tr))
        return tr

-- SMC

unroll 
  :: (MonadInfer m, Disjoint t s) 
  => P m i a 
  -> (a -> P m (t .+ s) a)
  -> [Rec t]
  -> m (Rec i, [Rec s])
unroll init next steps = do
  tInit <- sampler $ traced init
  let processStep soFar t = do {
    (s, results) <- soFar;
    tNext <- sampler $ observe (next s) t;
    return (retval (next s) (tNext .+ t), tNext : results)
  }
  (s, l) <- foldl processStep (return (retval init tInit, [])) steps
  return (tInit, reverse l)

particleFilter
  :: (MonadInfer m, Disjoint t s, 
      Disjoint Empty i, (Empty .+ i) ≈ i)
  => P m i a
  -> P m i b
  -> (a -> P m (t .+ s) a)
  -> [(Rec t, a -> P m s c)]
  -> m (Rec i, [Rec s])
particleFilter init qInit next steps = do
  tInit <- importance init Data.Row.Records.empty qInit
  let processStep soFar (t, q) = do {
    (s, results) <- soFar;
    tNext <- importance (next s) t (q s);
    return (retval (next s) (tNext .+ t), tNext : results)
  }
  (s, l) <- foldl processStep (return (retval init tInit, [])) steps
  return (tInit, reverse l)


-- MCMC

type K m t (s :: Row *) = U m (Rec t) -> Rec t -> m (Rec t)

mh
  :: forall s t m a
   . (Disjoint t s, MonadInfer m)
  => (Rec (t .+ s) -> P m t a)
  -> K m (t .+ s) t
mh q p old =
  let proposal = do
        new <- sampler (traced $ q old)
        return $ new .+ (restrict old :: Rec s)
      rho (x, y) =
          ((density p y) * (density (traced $ q y) (restrict x)))
            / ((density p x) * (density (traced $ q x) (restrict y)))
  in  do
        proposed <- proposal
        r        <- Bayes.random
        if Exp (log r) < (min 1 (rho (old, proposed)))
          then return proposed
          else return old

seqK :: Monad m => K m t s -> K m t r -> K m t (s .\/ r)
seqK k1 k2 p old = do
  t' <- k1 p old
  k2 p t'

mixK :: MonadSample m => Double -> K m t s -> K m t r -> K m t (s .\/ r)
mixK p k1 k2 model old = do
  r <- Bayes.random
  if r < p then k1 model old else k2 model old

repeatK :: Monad m => Int -> K m t s -> K m t s
repeatK n k p old = if n == 0
  then return old
  else do
    t <- k p old
    repeatK (n - 1) k p t

ifK
  :: (Disjoint t s, Monad m)
  => (Rec t -> Bool)
  -> K m (t .+ s) s
  -> K m (t .+ s) s
ifK b k p old = if (b (restrict old)) then k p old else return old

-- VARIATIONAL (STUBBED)

svi
  :: (Disjoint t s, Monad m)
  => P m (t .+ s) a
  -> Rec t
  -> (theta -> P m s b)
  -> theta
  -> m theta
svi p t q theta = undefined
-- do tr <- traceSampler $ q theta
--    let sco  = density (observe p t) tr
--    let gradient = grad (\th -> traceDensity (q th) tr) theta
--    return theta + stepSize * sco * gradient

trainAmortized
  :: (Disjoint t s, Monad m)
  => P m (t .+ s) a
  -> (theta -> Rec t -> P m s b)
  -> theta
  -> m theta
trainAmortized p q theta = undefined
-- do tr <- traceSampler p
--    let gradient = grad (\th -> traceDensity (q th (restrict tr)) (restrict tr)) theta
--    return theta + step * gradient


ifThenElse c t f = if c then t else f
