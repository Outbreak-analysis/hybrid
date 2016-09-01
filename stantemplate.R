if(process == "b"){
if(observation == "p"){
cat("I = c(",sim$I[1],sub("",",",sim$I[-1]),")"
    , "\n" ,"effprop = ",effprop
    , "\n" , "R0 =", R0
    , "\n" , "repprop = ", repprop
    , file = paste(process,observation,seed,iterations,"init.R",sep=".")
)

cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
    , "\n" ,"N = ",N
    , "\n" , "numobs =", numobs
    , "\n" , "i0 = ", i0
    , "\n" , "N0 = ", N0
    # , "\n" , "eps = ", eps
    , file = paste(process,observation,seed,iterations,"data.R",sep=".")
)

#poisson process
cat("data {
int<lower=0> numobs; // number of data points
    int obs[numobs]; // response
    int N;
    int i0;
    int N0;
    real eps;
    }
    parameters {
    real <lower=0> R0;
    real <lower=0,upper=1> repprop;
    real <lower=0,upper=1> effprop;
    real <lower=0> I[numobs];
    }
    model {
    vector[numobs] S;
    vector[numobs] pSI;
    vector[numobs] SIGrate;
    vector[numobs] SIGshape;
    real BETA;
    effprop ~ beta(1,1);
    repprop ~ beta(1,1);
    N0 ~ binomial(N,effprop);
    BETA = R0/N0;
    I[1] ~ gamma(i0,1/repprop);
    S[1] = N0/repprop - I[1];
    pSI[1] = 1 - exp(-I[1]*BETA);
    obs[1] ~ poisson(I[1]);
    SIGrate[1] = 0.1;
    SIGshape[1] = 0.1;
    
    
    for (t in 2:numobs) {
    SIGrate[t] = 1/(1-pSI[t-1]);
    SIGshape[t] = pSI[t-1]*S[t-1]*SIGrate[t]/repprop;
    pSI[t] = 1 - exp(-I[t]*BETA);
    I[t] ~ gamma(SIGshape[t],SIGrate[t]);
    S[t] = S[t-1] - I[t];
    obs[t] ~ poisson(I[t]);
    }
    }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
)

stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}

if(observation == "nb"){
  
  cat("I = c(",sim$I[1],sub("",",",sim$I[-1]),")"
      , "\n" , "obsMean = c(",sim$I[1],sub("",",",sim$I[-1]),")"
      , "\n" ,"effprop = ",effprop
      , "\n" , "R0 =", R0
      , "\n" , "repprop = ", repprop
      , "\n" , "repShape = ", 0.1
      , file = paste(process,observation,seed,iterations,"init.R",sep=".")
  )
  
  cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
      , "\n" ,"N = ",N
      , "\n" , "numobs =", numobs
      , "\n" , "i0 = ", i0
      # , "\n" , "eps = ", eps
      , file = paste(process,observation,seed,iterations,"data.R",sep=".")
  )
  
  #poisson process
  cat("data {
      int<lower=0> numobs; // number of data points
      int obs[numobs]; // response
      int N;
      int i0;
      real eps;
}
parameters {
real <lower=0> R0;
real <lower=0,upper=1> repprop;
real <lower=0,upper=1> effprop;
real <lower=0> repShape;
real <lower=0> I[numobs];
real <lower=0> obsMean[numobs];
}
model {
vector[numobs] S;
vector[numobs] pSI;
vector[numobs] SIGrate;
vector[numobs] SIGshape;
real N0;
real BETA;
effprop ~ beta(1,1);
repprop ~ beta(1,1);
N0 = N*effprop;
BETA = R0/N0;
I[1] ~ gamma(i0,1/repprop);
obsMean[1] ~ gamma(repShape,(repShape/I[1]));
S[1] = N0/repprop - I[1];
pSI[1] = 1 - exp(-I[1]*BETA);
obs[1] ~ poisson(obsMean[1]);
SIGrate[1] = 0.1;
SIGshape[1] = 0.1;


for (t in 2:numobs) {
SIGrate[t] = 1/(1-pSI[t-1]);
SIGshape[t] = pSI[t-1]*S[t-1]*SIGrate[t]/repprop;
pSI[t] = 1 - exp(-I[t]*BETA);
I[t] ~ gamma(SIGshape[t],SIGrate[t]);
obsMean[t] ~ gamma(repShape,(repShape/I[t]));
S[t] = S[t-1] - I[t];
obs[t] ~ poisson(obsMean[t]);
}
}"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
)

stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
}
