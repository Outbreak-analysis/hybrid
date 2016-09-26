library(methods)
library(coda)
library(nimble)
library(R2jags)
library(rstan)

## This is the part we want to sub data in instead of simulated data

dat <- NULL
if(is.null(dat)){dat <- sim}

nimbleOptions(verifyConjugatePosteriors=TRUE)
nimdata <- lme4:::namedList(obs=dat$Iobs)
nimcon <- lme4:::namedList(N,numobs,i0,Rshape,Rrate,effa,effb,repa,repb)
niminits <- lme4:::namedList(I=dat$I,effprop,R0,repprop,N0)

if(tail(niminits$I,1)<1){
  quit()
}

if(process == "bb"){
  niminits <- c(niminits,lme4:::namedList(pDis,phat=rep(0.5,numobs)))
  nimcon <- c(nimcon,lme4:::namedList(pDshape,pDrate))
}

if(process == "nb"){
  niminits <- c(niminits,lme4:::namedList(Imean=dat$I,pDis))
  nimcon <- c(nimcon,lme4:::namedList(pDshape,pDrate,epsp))
}

if(observation == "bb"){
  niminits <- c(niminits, lme4:::namedList(repDis,reporting=repprop))
  nimcon <- c(nimcon,lme4:::namedList(repDshape,repDrate))
}

if(observation == "nb"){
  niminits <- c(niminits,lme4:::namedList(obsMean=dat$I,repDis))
  nimcon <- c(nimcon,lme4:::namedList(repDshape,repDrate,epso))
  }

params <- c("R0","effprop","repprop")

source(paste("templates",type,process,observation,seed,iterations,plat,"nimcode",sep="."))
mcmcs <- c("jags"
           ,"nimble"
           ,"nimble_slice") 
stanmod <- ""
if(type=="hyb"){
  niminits <- lme4:::namedList(Ihat=dat$I*repprop,effprop,R0,repprop)
  nimcon <- lme4:::namedList(N,numobs,i0,Rshape,Rrate,effa,effb,repa,repb)
  if(process == "bb"){
    niminits <- c(niminits,lme4:::namedList(pDis))
    nimcon <- c(nimcon, lme4:::namedList(pDshape,pDrate))
  }
  if(process == "nb"){
    niminits <- c(niminits,lme4:::namedList(pDis))
    nimcon <- c(nimcon, lme4:::namedList(pDshape,pDrate))
  }
  if(observation == "nb"){
    niminits <- c(niminits,lme4:::namedList(obsMean=dat$I*repprop,repDis))
    nimcon <- c(nimcon, lme4:::namedList(repDshape,repDrate))
  }
  mcmcs <- c("jags"
               ,"nimble"
               ,"nimble_slice"
               ,"stan"
             )
  stanmod <- paste(process,observation,seed,iterations,"stan",sep=".")
}

if(plat == "nim"){
FitModel <- MCMCsuite(code=nimcode,
                      data=nimdata,
                      inits=niminits,
                      constants=nimcon,
                      MCMCs=mcmcs,
                      stan_model=stanmod,
                      monitors=params,
                      calculateEfficiency=TRUE,
                      niter=iterations,
                      makePlot=FALSE,
                      savePlot=FALSE)

print(FitModel$summary)
}

if(plat == "jags"){
  modfile <- paste("templates",type,process,observation,seed,iterations,plat,"bug",sep=".")
  mult = 1:mchains
  jagsinits <- lapply(mult, function(m){
    inc <- 1+sim$I
    return(list(
      effprop = effprop
      , R0 = R0
      , repprop = repprop
      , I = inc + m
      )
    )})
  
  ## inits (need to do multiple chains here) 
  FitModel <- jags(data=c(nimdata,nimcon)
                   , inits=jagsinits
                   , param = params
                   , model.file = modfile
                   , n.iter = iterations
                   , n.chains = length(jagsinits)
  )
  print(FitModel)
}


if(plat == "stan"){
  mult = 1:mchains
  staninits <- lapply(mult, function(m){
    inc <- 1+sim$I
    return(list(
      effprop = effprop
      , R0 = R0
      , repprop = repprop
      , I = inc + m
    )
    )})
  
  ## inits (need to do multiple chains here) 
  FitModel <- stan(file=stanmod
                   , data=c(nimdata,nimcon)
                   , init=staninits
                   , pars=params
                   , iter=500
                   , chains=length(staninits)
  
  )
  print(FitModel)
}


saveRDS(FitModel,file=paste(type,process,observation,seed,iterations,plat,"Rds",sep="."))

