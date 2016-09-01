observation_code <- NULL

if(observation == "b"){
  observation_code <- c("
      obs[1] ~ dbin(repprop,I[1])"
      , "
      obs[t] ~ dbin(repprop,I[t])"
      )
}

if(observation == "p"){
  observation_code <- c("
      obs[1] ~ dpois(I[1])"
      , "
      obs[t] ~ dpois(I[t])")
}

if(observation == "nb"){
  observation_code <- c("
      repShape ~ dgamma(1,1)
      obsMean[1] ~ dgamma(repShape,I[1]/repShape)
      obs[1] ~ dpois(obsMean[1])"
                        , "
      obsMean[t] ~ dgamma(repShape,I[t]/repShape)
      obs[t] ~ dpois(obsMean[t])")
}


