library(methods)
library(coda)
library(nimble)

nimbleOptions(verifyConjugatePosteriors=TRUE)
nimdata <- lme4:::namedList(obs=sim$Iobs)
nimcon <- lme4:::namedList(numobs ,N ,i0)
niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop,N0)

if(process == "bb"){
  niminits <- c(niminits,lme4:::namedList(pSIa=rep(0.1,numobs), pSIb=rep(0.1,numobs)))
  nimcon <- c(nimcon,lme4:::namedList(pSISize))
}

if(process == "nb"){
  niminits <- c(niminits,lme4:::namedList(Imean=sim$I,pSISize))
}

if(observation == "bb"){
    niminits <- c(niminits,lme4:::namedList(repobsa,repobsb))
    nimcon <- c(nimcon,lme4:::namedList(repobsSize))
}

if(observation == "nb"){
  niminits <- c(niminits,lme4:::namedList(obsMean=sim$I, repShape))
}

params <- c("R0","effprop","repprop")

source(paste("templates",type,process,observation,seed,iterations,"nimcode",sep="."))
mcmcs <- c(#"jags"
           "nimble"
           ,"nimble_slice") 
stanmod <- ""
if(type=="hyb"){
  niminits <- lme4:::namedList(I=sim$I*repprop,effprop,R0,repprop)
  nimcon <- lme4:::namedList(numobs ,N ,i0)
  if(process == "nb"){
    niminits <- c(niminits,lme4:::namedList(pSISize))
  }
  if(observation == "nb"){
    niminits <- c(niminits,lme4:::namedList(obsMean=sim$I*repprop,repShape))
  }
  mcmcs <- c("jags"
               ,"nimble"
               ,"nimble_slice"
               ,"stan"
             )
  stanmod <- paste(process,observation,seed,iterations,"stan",sep=".")
}

# aa <- nimbleModel(code=nimcode,constants=nimcon,data=nimdata,inits=niminits)
# bb <- aa$getGraph()

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

saveRDS(FitModel,file=paste(type,process,observation,seed,iterations,"Rds",sep="."))

