
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
      Ihat[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-Ihat[1]*beta)
      SIGrate[1] <- 0.1
      SIGshape[1] <- 0.1
      "
      ,"
      SIGrate[t] <- 1/(1-pSI[t-1])
      SIGshape[t] <- pSI[t-1]*SIGrate[t]*(Shat[t-1]/repprop)
      Ihat[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-Ihat[t]*beta)
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
      pDis ~ dgamma(1,1)
      pSI[1] <- 1 - exp(-I[1]*beta) + eps
      pSIa[1] ~ dgamma(pDis/(1-pSI[1]),1)
      pSIb[1] ~ dgamma(pDis/(pSI[1]),1)
                      "
      ,"
      I[t] ~ dbin(pSIa[t-1]/(pSIa[t-1]+pSIb[t-1]),S[t-1])
      pSI[t] <- 1 - exp(-I[t]*beta) + eps
      pSIa[t] ~ dgamma(pDis/(1-pSI[t]),1)
      pSIb[t] ~ dgamma(pDis/(pSI[t]),1)
      "
    )
  }
  if(type == "hyb"){
    process_code <- c("
      Ihat[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pDis ~ dgamma(1,1)
      pSI[1] <- 1 - exp(-Ihat[1]*beta)
      a[1] <- pDis/(1-pSI[1])
      b[1] <- pDis/(pSI[1])
      "
      ,"
      SIGrate[t] <- (a[t-1]+b[t-1])*(a[t-1]+b[t-1]+1)/(b[t-1]*(a[t-1]+b[t-1]+Shat[t-1]/repprop))
      SIGshape[t] <- (a[t-1]/(a[t-1]+b[t-1]))*Shat[t-1]*SIGrate[t]/repprop
      Ihat[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-Ihat[t]*beta)
      a[t] <- pDis/(1-pSI[t])
      b[t] <- pDis/(pSI[t])
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
      Ihat[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-Ihat[1]*beta)
      SIGrate[1] <- 1
      SIGshape[1] <- 1
                      "
      ,"
      SIGrate[t] <- 1
      SIGshape[t] <- pSI[t-1]*SIGrate[t]*(Shat[t-1]/repprop)
      Ihat[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-Ihat[t]*beta)
      "
    )
  }
}

## Poisson Process ----
if(process == "nb"){
  if(type == "dis"){
    process_code <- c("
      pDis ~ dgamma(1,1)
      Imean[1] ~ dgamma(pDis,pDis/i0)
      I[1] ~ dpois(Imean[1])
      beta <- R0/N0
      pSI[1] <- 1 - exp(-I[1]*beta)
      "
      ,"
      Imean[t] ~ dgamma(pDis,pDis/(S[t-1]*pSI[t-1]))
      I[t] ~ dpois(Imean[t])
      pSI[t] <- 1 - exp(-I[t]*beta)
                      "
    )
  }
  if(type == "hyb"){
    process_code <- c("
      pDis ~ dgamma(1,1)
      Ihat[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-Ihat[1]*beta)
      SIGrate[1] <- 0.1
      SIGshape[1] <- 0.1
                      "
      ,"
      SIGrate[t] <- pDis/(Shat[t-1]*pSI[t-1]/repprop)
      SIGshape[t] <- pDis
      Ihat[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-Ihat[t]*beta)
      "
    )
  }
}
