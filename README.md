# Latent incidence fitting 

## Introduction
- Building a Bayesian framework with hybridizing trick (use continuous processes to 
substitute discrete processes via moment match)
- Starting from the very beginning with a RF model as a baseline model (Chain binomial, 
Discrete time SIR one time step after infection) and build on top of it
$$ S_{1} = N - I_{1} $$
$$ I_{t+1} \sim Binomial(pSI_{t},S_{t}) $$
$$ S_{t+1} = S_{t} - I{t} $$

## Current model

### Platforms 
- jags (bugs) ## hyperlink?
- nimble () ## hyperlink?
- stan

### Latent process
- Binomial, beta-binomial, negative-binomial and poisson 

### Observation process

## Simulation results

## Problems

## Future Direction (short term/ long term)

