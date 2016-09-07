library(methods)
library(coda)
library(nimble)

nimbleOptions(verifyConjugatePosteriors=TRUE)
nimdata <- lme4:::namedList(obs=sim$Iobs)
nimcon <- lme4:::namedList(numobs ,N ,i0)

niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop, N0)

if(process == "bb"){
  niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop,N0
                               , pSIa=rep(0.1,numobs)
                               , pSIb=rep(0.1,numobs))
  nimcon <- lme4:::namedList(numobs ,N ,i0, pSISize, eps)
}

if(observation == "nb"){
  niminits <- lme4:::namedList(I=sim$I,obsMean=sim$I,effprop,R0,repprop,N0, repShape)
}

if(observation == "bb"){
  niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop,N0,repobsa,repobsb
                               , pSIa=rep(0.1,numobs)
                               , pSIb=rep(0.1,numobs))
  nimcon <- lme4:::namedList(numobs ,N ,i0,repobsSize,pSISize)
  }

params <- c("R0","effprop","repprop")

source(paste("templates",type,process,observation,seed,iterations,"nimcode",sep="."))
mcmcs <- c(#"jags"
           "nimble"
           ,"nimble_slice") 
stanmod <- ""
if(type=="hyb"){
  niminits <- lme4:::namedList(I=sim$I*repprop,effprop,R0,repprop)
  if(observation == "nb"){
    niminits <- lme4:::namedList(I=sim$I*repprop,obsMean=sim$I*repprop,effprop,R0,repprop,repShape)
  }
  mcmcs <- c("jags"
               ,"nimble"
               ,"nimble_slice"
               ,"stan"
             )
  stanmod <- paste(process,observation,seed,iterations,"stan",sep=".")
}

aa <- nimbleModel(code=nimcode,constants=nimcon,data=nimdata,inits=niminits)
bb <- aa$getGraph()

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

saveRDS(FitModel,file=paste(type,process,observation,seed,iteration,".Rds"))

