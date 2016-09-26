# Latent incidence fitting 

## Introduction
- Building a Bayesian framework with hybridizing trick (use continuous processes to 
substitute discrete processes via moment match)
- Starting from the very beginning with a RF model as a baseline model (Chain binomial, 
Discrete time SIR assumes recovery one time step after infection) and build on top of it
$$ S_{1} = N - I_{1} $$
$$ I_{t+1} \sim Binomial(pSI_{t},S_{t}) $$
$$ S_{t+1} = S_{t} - I{t} $$

## Current model
- Using effective population (effective proportion)
- RF baseline model as a latent process 
- Observation process with some reporting proportion

$$ N_{eff} \sim Binomial(effprop, N) $$
$$ S_{1} = N_{eff} - I_{1} $$
$$ I_{t+1} \sim Binomial(pSI_{t},S_{t}) $$
$$ S_{t+1} = S_{t} - I{t} $$
$$ Obs_{t} \sim Binomial(repprop, I_{t}) $$

### Platforms 
- [Jags](http://mcmc-jags.sourceforge.net/)
- [Nimble](http://r-nimble.org/)
- [Stan](http://mc-stan.org/)

### Latent process
- Binomial, beta-binomial, negative-binomial and poisson (discrete and gamma hybrid)
- [Hybridizing step](http://rawgit.com/wzmli/hybrid/master/mm.html)

### Observation process
- Binomial, beta-binomial, negative-binomial and poisson

## How to use it (3 parts)
1. Simulate (or enter a data frame)
2. template
3. fit

- make fit.\$(type).\$(process).\$(observation).\$(seed).\$(iterations).\$(platform).Rout
- type (discrete, hybrid)
- process (b, bb, nb, p)
- observation (b, bb, nb, p)
- seed (simulation only)
- iterations (MCMC chain length)
- platform (Nimble has jags/stan callers but only can do single chains, stan and jags can do multiple chains)


## Simulation results
- [preliminary plots](http://rawgit.com/wzmli/hybrid/master/simplots.html)
- wide credible intervals
- Coverage around 95% band
- deviation among platforms 

## To do/ Future Direction

### Short term
- multiple chains and diagnostics (just finish)
- Check idenifiability (data cloning)
- remove recovery one time step after infection assumption
- forecast

### Long term
- continuous binomial and beta-binomial observational process (take continuous parameters)
- add other forms of complexty
- multi-sub population (a knob to switch from population base to multi-sub pop to ind-base)
- spatial model
- others ...


