if(process == "b"){
if(observation == "p"){
cat("Ihat = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
    , "\n" ,"effprop = ",effprop
    , "\n" , "R0 =", R0
    , "\n" , "repprop = ", repprop
    , file = paste(process,observation,seed,iterations,"init.R",sep=".")
)

cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
    , "\n" ,"N = ", N
    , "\n" , "numobs =", numobs
    , "\n" , "i0 = ", i0
    , "\n" , "effa = ", effa
    , "\n" , "effb = ", effb
    , "\n" , "repa = ", repa
    , "\n" , "repb = ", repb
    , "\n" , "Rshape = ", Rshape
    , "\n" , "Rrate = ", Rrate
    , "\n" , "SIGshape = ", SIGshape
    , "\n" , "SIGrate = ", SIGrate   
    , "\n" , "eps = ", eps
    , file = paste(process,observation,seed,iterations,"data.R",sep=".")
)

#poisson process
cat("data {
int<lower=0> numobs; // number of data points
    int obs[numobs]; // response
    int N;
    int i0;
    real effa;
    real effb;
    real repa;
    real repb;
    real Rshape;
    real Rrate;
    real eps;
    }
    parameters {
    real <lower=0> R0;
    real <lower=0,upper=1> repprop;
    real <lower=0,upper=1> effprop;
    real <lower=0> Ihat[numobs];
    }
    model {
    vector[numobs] Shat;
    vector[numobs] pSI;
    vector[numobs] SIGrate;
    vector[numobs] SIGshape;
    real BETA;
    real N0;
    R0 ~ gamma(2,1);
    effprop ~ beta(9,2);
    repprop ~ beta(9,9);
    N0 = N*effprop;
    BETA = R0/N0;
    Ihat[1] ~ gamma(i0,1/repprop);
    Shat[1] = N0*repprop - Ihat[1];
    pSI[1] = 1 - exp(-Ihat[1]*BETA);
    obs[1] ~ poisson(Ihat[1]);
    
    
    for (t in 2:numobs) {
    SIGrate[t] = 1/(1-pSI[t-1]);
    SIGshape[t] = pSI[t-1]*Shat[t-1]*SIGrate[t]/repprop;
    pSI[t] = 1 - exp(-Ihat[t]*BETA);
    Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
    Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
    obs[t] ~ poisson(Ihat[t]);
    }
    }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
)

stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}

if(observation == "nb"){
  
  cat("Ihat = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
      , "\n" , "obsMean = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
      , "\n" ,"effprop = ",effprop
      , "\n" , "R0 =", R0
      , "\n" , "repprop = ", repprop
      , "\n" , "repDis = ", repDis
      , file = paste(process,observation,seed,iterations,"init.R",sep=".")
  )
  
  cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
      , "\n" ,"N = ",N
      , "\n" , "numobs =", numobs
      , "\n" , "i0 = ", i0
      , "\n" , "eps = ", eps
      , file = paste(process,observation,seed,iterations,"data.R",sep=".")
  )
  

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
real <lower=0> repDis;
real <lower=0> Ihat[numobs];
real <lower=0> obsMean[numobs];
}
model {
vector[numobs] Shat;
vector[numobs] pSI;
vector[numobs] SIGrate;
vector[numobs] SIGshape;
real N0;
real BETA;
effprop ~ beta(9,2);
repprop ~ beta(9,9);
R0 ~ gamma(2,1);
N0 = N*effprop;
BETA = R0/N0;
Ihat[1] ~ gamma(i0,1/repprop);
obsMean[1] ~ gamma(repDis,(repDis/Ihat[1]));
Shat[1] = N0*repprop - Ihat[1];
pSI[1] = 1 - exp(-Ihat[1]*BETA);
obs[1] ~ poisson(obsMean[1]);
SIGrate[1] = 0.1;
SIGshape[1] = 0.1;


for (t in 2:numobs) {
SIGrate[t] = 1/(1-pSI[t-1]);
SIGshape[t] = pSI[t-1]*Shat[t-1]*SIGrate[t]/repprop;
pSI[t] = 1 - exp(-Ihat[t]*BETA);
Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
obsMean[t] ~ gamma(repDis,(repDis/Ihat[t]));
Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
obs[t] ~ poisson(obsMean[t]);
}
}"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
)

stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
}

if(process == "bb"){
  if(observation == "p"){
    cat("Ihat = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" ,"effprop = ",effprop
        , "\n" , "R0 =", R0
        , "\n" , "repprop = ", repprop
        , "\n" , "pDis = ", pDis
        , file = paste(process,observation,seed,iterations,"init.R",sep=".")
    )
    
    cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
        , "\n" ,"N = ",N
        , "\n" , "numobs =", numobs
        , "\n" , "i0 = ", i0
        , "\n" , "eps = ", eps
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
        real <lower=0> Ihat[numobs];
        real <lower=0> pDis;
        }
        model {
        vector[numobs] Shat;
        vector[numobs] pSI;
        vector[numobs] a;
        vector[numobs] b;
        vector[numobs] SIGrate;
        vector[numobs] SIGshape;
        real BETA;
        real N0;
        R0 ~ gamma(2,1);
        effprop ~ beta(9,2);
        repprop ~ beta(9,9);
        pDis ~ gamma(1,1);
        N0 = N*effprop;
        BETA = R0/N0;
        Ihat[1] ~ gamma(i0,1/repprop);
        Shat[1] = N0*repprop - Ihat[1];
        pSI[1] = 1 - exp(-Ihat[1]*BETA);
        a[1] = pDis/(1-pSI[1]);
        b[1] = pDis/(pSI[1]);
        obs[1] ~ poisson(Ihat[1]);
        SIGrate[1] = 0.1;
        SIGshape[1] = 0.1;
        
        
        for (t in 2:numobs) {
        SIGrate[t] = (a[t-1]+b[t-1])*(a[t-1]+b[t-1]+1)/(b[t-1]*(a[t-1]+b[t-1]+Shat[t-1]/repprop));
        SIGshape[t] = (a[t-1]/(a[t-1]+b[t-1]))*Shat[t-1]*SIGrate[t]/repprop;
        pSI[t] = 1 - exp(-Ihat[t]*BETA);
        a[t] = pDis/(1-pSI[t]);
        b[t] = pDis/(pSI[t]);
        Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
        Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
        obs[t] ~ poisson(Ihat[t]);
        }
        }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
    )
    
    stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
  
  if(observation == "nb"){
    
    cat("Ihat = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" , "obsMean = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" ,"effprop = ",effprop
        , "\n" , "R0 =", R0
        , "\n" , "repprop = ", repprop
        , "\n" , "repShape = ", repDis
        , "\n" , "pDis = ", pDis
        , file = paste(process,observation,seed,iterations,"init.R",sep=".")
    )
    
    cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
        , "\n" ,"N = ",N
        , "\n" , "numobs =", numobs
        , "\n" , "i0 = ", i0
        , "\n" , "eps = ", eps
        , file = paste(process,observation,seed,iterations,"data.R",sep=".")
    )
    
    
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
        real <lower=0> repDis;
        real <lower=0> pDis;
        real <lower=0> Ihat[numobs];
        real <lower=0> obsMean[numobs];
        }
        model {
        vector[numobs] Shat;
        vector[numobs] pSI;
	      vector[numobs] a;
	      vector[numobs] b;
        vector[numobs] SIGrate;
        vector[numobs] SIGshape;
        real N0;
        real BETA;
        effprop ~ beta(9,2);
        repprop ~ beta(9,9);
        R0 ~ gamma(2,1);
        repDis ~ gamma(1,1);
        pDis ~ gamma(1,1);
        N0 = N*effprop;
        BETA = R0/N0;
        Ihat[1] ~ gamma(i0,1/repprop);
        obsMean[1] ~ gamma(repDis,(repDis/Ihat[1]));
        Shat[1] = N0*repprop - Ihat[1];
        pSI[1] = 1 - exp(-Ihat[1]*BETA);
        a[1] = pDis/(1-pSI[1]);
        b[1] = pDis/(pSI[1]);
        obs[1] ~ poisson(obsMean[1]);
        SIGrate[1] = 0.1;
        SIGshape[1] = 0.1;
        
        
        for (t in 2:numobs) {
        SIGrate[t] = (a[t-1]+b[t-1])*(a[t-1]+b[t-1]+1)/(b[t-1]*(a[t-1]+b[t-1]+Shat[t-1]/repprop));
        SIGshape[t] = (a[t-1]/(a[t-1]+b[t-1]))*Shat[t-1]*SIGrate[t]/repprop;
        pSI[t] = 1 - exp(-Ihat[t]*BETA);
        a[t] = pDis/(1-pSI[t]);
        b[t] = pDis/(pSI[t]);
        Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
        obsMean[t] ~ gamma(repDis,(repDis/Ihat[t]));
        Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
        obs[t] ~ poisson(obsMean[t]);
        }
        }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
)

stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
}

if(process == "p"){
  if(observation == "p"){
    cat("Ihat = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" ,"effprop = ",effprop
        , "\n" , "R0 =", R0
        , "\n" , "repprop = ", repprop
        , file = paste(process,observation,seed,iterations,"init.R",sep=".")
    )
    
    cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
        , "\n" ,"N = ",N
        , "\n" , "numobs =", numobs
        , "\n" , "i0 = ", i0
        , "\n" , "eps =", eps
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
        real <lower=0> Ihat[numobs];
        }
        model {
        vector[numobs] Shat;
        vector[numobs] pSI;
        vector[numobs] SIGrate;
        vector[numobs] SIGshape;
        real BETA;
        real N0;
        R0 ~ gamma(2,1);
        effprop ~ beta(9,2);
        repprop ~ beta(9,9);
        N0 = N*effprop;
        BETA = R0/N0;
        Ihat[1] ~ gamma(i0,1/repprop);
        Shat[1] = N0*repprop - Ihat[1];
        pSI[1] = 1 - exp(-Ihat[1]*BETA);
        obs[1] ~ poisson(Ihat[1]);
        SIGrate[1] = 1;
        SIGshape[1] = 1;
        
        
        for (t in 2:numobs) {
        SIGrate[t] = 1;
        SIGshape[t] = pSI[t-1]*Shat[t-1]*SIGrate[t]/repprop;
        pSI[t] = 1 - exp(-Ihat[t]*BETA);
        Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
        Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
        obs[t] ~ poisson(Ihat[t]);
        }
        }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
    )
    
    stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
  
  if(observation == "nb"){
    
    cat("Ihat = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" , "obsMean = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" ,"effprop = ",effprop
        , "\n" , "R0 =", R0
        , "\n" , "repprop = ", repprop
        , "\n" , "repDis= ", repDis
        , file = paste(process,observation,seed,iterations,"init.R",sep=".")
    )
    
    cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
        , "\n" ,"N = ",N
        , "\n" , "numobs =", numobs
        , "\n" , "i0 = ", i0
        , "\n" , "eps = ", eps
        , file = paste(process,observation,seed,iterations,"data.R",sep=".")
    )
    
    
    cat("data {
        int<lower=0> numobs; // number of data points
        int obs[numobs]; // response
        int N;
        int i0;
        real repDis;
        real eps;
  }
        parameters {
        real <lower=0> R0;
        real <lower=0,upper=1> repprop;
        real <lower=0,upper=1> effprop;
        real <lower=0> Ihat[numobs];
        real <lower=0> obsMean[numobs];
        }
        model {
        vector[numobs] Shat;
        vector[numobs] pSI;
        vector[numobs] SIGrate;
        vector[numobs] SIGshape;
        real N0;
        real BETA;
        effprop ~ beta(9,2);
        repprop ~ beta(9,9);
        R0 ~ gamma(2,1);
        repDis ~ gamma(1,1);
        N0 = N*effprop;
        BETA = R0/N0;
        Ihat[1] ~ gamma(i0,1/repprop);
        obsMean[1] ~ gamma(repDis,(repDis/Ihat[1]));
        Shat[1] = N0*repprop - Ihat[1];
        pSI[1] = 1 - exp(-Ihat[1]*BETA);
        obs[1] ~ poisson(obsMean[1]);
        SIGrate[1] = 1;
        SIGshape[1] = 1;
        
        
        for (t in 2:numobs) {
        SIGrate[t] = 1;
        SIGshape[t] = pSI[t-1]*Shat[t-1]*SIGrate[t]/repprop;
        pSI[t] = 1 - exp(-Ihat[t]*BETA);
        Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
        obsMean[t] ~ gamma(repDis,(repDis/Ihat[t]));
        Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
        obs[t] ~ poisson(obsMean[t]);
        }
        }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
)

stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
}


if(process == "nb"){
  if(observation == "p"){
    cat("I = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" ,"effprop = ",effprop
        , "\n" , "R0 =", R0
        , "\n" , "repprop = ", repprop
        , "\n" , "pDis = ", pDis
        , file = paste(process,observation,seed,iterations,"init.R",sep=".")
    )
    
    cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
        , "\n" ,"N = ",N
        , "\n" , "numobs =", numobs
        , "\n" , "i0 = ", i0
        , "\n" , "eps = ", eps
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
        real <lower=0> pDis;
        real <lower=0,upper=1> effprop;
        real <lower=0> Ihat[numobs];
        }
        model {
        vector[numobs] Shat;
        vector[numobs] pSI;
        vector[numobs] SIGrate;
        vector[numobs] SIGshape;
        real BETA;
        real N0;
        R0 ~ gamma(2,1);
        pDis ~ gamma(1,1);
        effprop ~ beta(9,2);
        repprop ~ beta(9,9);
        N0 = N*effprop;
        BETA = R0/N0;
        Ihat[1] ~ gamma(i0,1/repprop);
        Shat[1] = N0*repprop - Ihat[1];
        pSI[1] = 1 - exp(-Ihat[1]*BETA);
        obs[1] ~ poisson(Ihat[1]);
        SIGrate[1] = 1;
        SIGshape[1] = pDis;
        
        
        for (t in 2:numobs) {
        SIGrate[t] = pDis/(Shat[t-1]*pSI[t-1]/repprop);
        SIGshape[t] = pDis;
        pSI[t] = 1 - exp(-Ihat[t]*BETA);
        Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
        Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
        obs[t] ~ poisson(Ihat[t]);
        }
        }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
    )
    
    stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
  
  if(observation == "nb"){
    
    cat("Ihat = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" , "obsMean = c(",sim$I[1]*repprop,sub("",",",sim$I[-1]*repprop),")"
        , "\n" ,"effprop = ",effprop
        , "\n" , "R0 =", R0
        , "\n" , "repprop = ", repprop
        , "\n" , "repDis = ", repDis
        , "\n" , "pDis = ", pDis
        , file = paste(process,observation,seed,iterations,"init.R",sep=".")
    )
    
    cat("obs = c(",sim$Iobs[1],sub("",",",sim$Iobs[-1]),")"
        , "\n" ,"N = ",N
        , "\n" , "numobs =", numobs
        , "\n" , "i0 = ", i0
        , "\n" , "eps = ", eps
        , file = paste(process,observation,seed,iterations,"data.R",sep=".")
    )
    
    
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
        real <lower=0> repDis;
        real <lower=0> Ihat[numobs];
        real <lower=0> obsMean[numobs];
        real <lower=0> pDis;
        }
        model {
        vector[numobs] Shat;
        vector[numobs] pSI;
        vector[numobs] SIGrate;
        vector[numobs] SIGshape;
        real N0;
        real BETA;
        pDis ~ gamma(1,1);
        repDis ~ gamma(1,1);
        effprop ~ beta(9,2);
        repprop ~ beta(9,9);
        R0 ~ gamma(2,1);
        N0 = N*effprop;
        BETA = R0/N0;
        Ihat[1] ~ gamma(i0,1/repprop);
        obsMean[1] ~ gamma(repDis,(repDis/Ihat[1]));
        Shat[1] = N0*repprop - Ihat[1];
        pSI[1] = 1 - exp(-Ihat[1]*BETA);
        obs[1] ~ poisson(obsMean[1]);
        SIGrate[1] = 1;
        SIGshape[1] = pDis;
        
        
        for (t in 2:numobs) {
        SIGrate[t] = pDis/(Shat[t-1]*pSI[t-1]/repprop);
        SIGshape[t] = pDis;
        pSI[t] = 1 - exp(-Ihat[t]*BETA);
        Ihat[t] ~ gamma(SIGshape[t],SIGrate[t]);
        obsMean[t] ~ gamma(repDis,(repDis/Ihat[t]));
        Shat[t] = fmax(Shat[t-1] - Ihat[t],eps);
        obs[t] ~ poisson(obsMean[t]);
        }
        }"
    , file = paste(process,observation,seed,iterations,"stan",sep=".")
)

stanmod = paste(process,observation,seed,iterations,"stan",sep=".")
}
}
