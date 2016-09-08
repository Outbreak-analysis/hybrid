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
initDis <- 0.1
N0 <- round(N*effprop)
numobs <- 15
pDis <- 1
repDis <- 10
eps <- 0.000000001
