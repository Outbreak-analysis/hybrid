
process_code <- NULL

## Binomial Process ----
if(process == "b"){
  if(type == "dis"){
    process_code <- c("
      I[1] ~ dbin(1,i0)
      beta <- R0/N0
	    pSI[1] <- 1 - exp(-I[1]*beta)
      "
      ,"
      I[t] ~ dbin(pSI[t-1],S[t-1])
      pSI[t] <- 1 - exp(-I[t]*beta)
      "
    )
  }
  if(type == "hyb"){
    process_code <- c("
      I[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-I[1]*beta)
      SIGrate[1] <- 0.1
      SIGshape[1] <- 0.1
      "
      ,"
      SIGrate[t] <- 1/(1-pSI[t-1])
      SIGshape[t] <- pSI[t-1]*SIGrate[t]*(S[t-1]/repprop)
      I[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-I[t]*beta)
      "
    )
  }
}

## Beta-Binomial Process ----
if(process == "bb"){
  if(type == "dis"){
    process_code <- c("
      I[1] ~ dbin(1,i0)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-I[1]*beta) + eps
      pSIa[1] ~ dgamma(pSISize/(1-pSI[1]),1)
      pSIb[1] ~ dgamma(pSISize/(pSI[1]),1)
                      "
      ,"
      I[t] ~ dbin(pSIa[t-1]/(pSIa[t-1]+pSIb[t-1]),S[t-1])
      pSI[t] <- 1 - exp(-I[t]*beta) + eps
      pSIa[t] ~ dgamma(pSISize/(1-pSI[t]),1)
      pSIb[t] ~ dgamma(pSISize/(pSI[t]),1)
      "
    )
  }
  if(type == "hyb"){
    process_code <- c("
      I[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-I[1]*beta)
      a[1] <- pSISize/(1-pSI[1])
      b[1] <- pSISize/(pSI[1])
      "
      ,"
      SIGrate[t] <- (a[t-1]+b[t-1])*(a[t-1]+b[t-1]+1)/(b[t-1]*(a[t-1]+b[t-1]+S[t-1]/repprop))
      SIGshape[t] <- (a[t-1]/(a[t-1]+b[t-1]))*S[t-1]*SIGrate[t]/repprop
      I[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-I[t]*beta)
      a[t] <- pSISize/(1-pSI[t])
      b[t] <- pSISize/(pSI[t])
      "
    )
  }
}

## Poisson Process ----
if(process == "p"){
  if(type == "dis"){
    process_code <- c("
                      I[1] ~ dpois(i0)
                      beta <- R0/N0
                      pSI[1] <- 1 - exp(-I[1]*beta)
                      "
                      ,"
                      I[t] ~ dpois(pSI[t-1]*S[t-1])
                      pSI[t] <- 1 - exp(-I[t]*beta)
                      "
    )
  }
  if(type == "hyb"){
    process_code <- c("
                      I[1] ~ dgamma(i0,1/repprop)
                      beta <- R0/N0
                      pSI[1] <- 1 - exp(-I[1]*beta)
                      SIGrate[1] <- 1
                      SIGshape[1] <- 1
                      "
      ,"
      SIGrate[t] <- 1
      SIGshape[t] <- pSI[t-1]*SIGrate[t]*(S[t-1]/repprop)
      I[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-I[t]*beta)
      "
    )
  }
}

## Poisson Process ----
if(process == "nb"){
  if(type == "dis"){
    process_code <- c("
                      pSISize ~ dgamma(1,1)
                      Imean[1] ~ dgamma(pSISize,pSISize/i0)
                      I[1] ~ dpois(Imean[1])
                      beta <- R0/N0
                      pSI[1] <- 1 - exp(-I[1]*beta)
                      "
                      ,"
                      Imean[t] ~ dgamma(pSISize,pSISize/(S[t-1]*pSI[t-1]))
                      I[t] ~ dpois(Imean[t])
                      pSI[t] <- 1 - exp(-I[t]*beta)
                      "
    )
  }
  if(type == "hyb"){
    process_code <- c("
                      pSISize ~ dgamma(1,1)
                      I[1] ~ dgamma(i0,1/repprop)
                      beta <- R0/N0
                      pSI[1] <- 1 - exp(-I[1]*beta)
                      SIGrate[1] <- 0.1
                      SIGshape[1] <- 0.1
                      "
      ,"
      SIGrate[t] <- pSISize/(S[t-1]*pSI[t-1]/repprop)
      SIGshape[t] <- pSI[t-1]*SIGrate[t]*(S[t-1]/repprop)
      I[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-I[t]*beta)
      "
    )
  }
}
