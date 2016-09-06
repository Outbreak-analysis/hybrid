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
      obsMean[1] ~ dgamma(repShape,repShape/I[1])
      obs[1] ~ dpois(obsMean[1])"
                        , "
      obsMean[t] ~ dgamma(repShape,repShape/I[t])
      obs[t] ~ dpois(obsMean[t])")
}

if(observation == "bb"){
  observation_code <- c("
      repobsa ~ dgamma(repobsSize/(1-repprop), 1)
	    repobsb ~ dgamma(repobsSize/repprop, 1)
      reporting <- repobsa/(repobsa + repobsb)
      obs[1] ~ dbin(reporting,I[1])"
                        , "
      obs[t] ~ dbin(reporting,I[t])")
}
