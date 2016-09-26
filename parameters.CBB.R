# epi parameters
R0 <- 2
effprop <- 0.9
repprop <- 0.5

# init values and constants
N <- 10000
i0 <- 1
N0 <- round(N*effprop)
numobs <- 15
eps <- 0.000000001
epsp <- 0.000000001
epso <- 0.000000001

# epi gamma/beta prior parameters
Rshape <- 2
Rrate <- 1
effa <- 9
effb <- 2
repa <- 9
repb <- 9

#dispersion parameters
pDis <- 5
pDshape <- 0.1
pDrate <- 0.1
repDshape <- 0.1
repDrate <- 0.1

repobsa <- 0.1
repobsb <- 0.1
repDis <- 10
pDis <- 1

#hybrid parameters 
SIGshape <- 0.1
SIGrate <- 0.1

#MCMC stuff
#iterations?
mchains <- 4