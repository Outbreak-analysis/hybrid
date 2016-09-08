library(methods)
library(coda)
library(nimble)

nimbleOptions(verifyConjugatePosteriors=TRUE)
nimdata <- lme4:::namedList(obs=sim$Iobs)
nimcon <- lme4:::namedList(numobs ,N ,i0)
niminits <- lme4:::namedList(I=sim$I,effprop,R0,repprop,N0)

if(tail(niminits$I,1)<1){
  quit()
}

if(process == "bb"){
  nimcon <- c(nimcon,lme4:::namedList(pDis))
}

if(process == "nb"){
  niminits <- c(niminits,lme4:::namedList(Imean=sim$I))
  nimcon <- c(nimcon,lme4:::namedList(pDis))
}

if(observation == "bb"){
    nimcon <- c(nimcon,lme4:::namedList(repDis))
}

if(observation == "nb"){
  niminits <- c(niminits,lme4:::namedList(obsMean=sim$I))
  nimcon <- c(nimcon,lme4:::namedList(repDis))
  }

params <- c("R0","effprop","repprop")

source(paste("templates",type,process,observation,seed,iterations,"nimcode",sep="."))
mcmcs <- c(#"jags"
           "nimble"
           ,"nimble_slice") 
stanmod <- ""
if(type=="hyb"){
  niminits <- lme4:::namedList(Ihat=sim$I*repprop,effprop,R0,repprop)
  nimcon <- lme4:::namedList(numobs ,N ,i0)
  if(process == "bb"){
    niminits <- c(niminits,lme4:::namedList(pDis))
  }
  if(process == "nb"){
    niminits <- c(niminits,lme4:::namedList(pDis))
  }
  if(observation == "nb"){
    niminits <- c(niminits,lme4:::namedList(obsMean=sim$I*repprop,repDis))
  }
  mcmcs <- c(#"jags"
               "nimble"
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

