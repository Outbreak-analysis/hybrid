# real parameters
R0 <- 2
effprop <- 0.9
repprop <- 0.5



#bb observation parameters 
repobsa <- 0.1
repobsb <- 0.1


# init values and constants
N <- 10000
i0 <- 1
repSize <- 1000
initDis <- 0.1
N0 <- round(N*effprop)
numobs <- 15
pSISize <- 1
repobsSize <- 10
repShape <- 0.1
Ndis <- 2
eps <- 1e-10
