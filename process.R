
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
      pSI[1] <- 1 - exp(-I[1]*beta)
      pDis ~ dgamma(pDshape,pDrate)
      phat[1] ~ dbeta(pDis/(1-pSI[1]),pDis/pSI[1])
      "
      ,"
      I[t] ~ dbin(phat[t-1],S[t-1])
      pSI[t] <- 1 - exp(-I[t]*beta)
      phat[t] ~ dbeta(pDis/(1-pSI[t]),pDis/pSI[t])
      "
    )
  }
  if(type == "hyb"){
    process_code <- c("
      Ihat[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-Ihat[1]*beta)
      pDis ~ dgamma(pDshape,pDrate)
      "
      ,"
      SIGrate[t] <- (pDis+1)/((1-pSI[t-1])*(pDis+Shat[t-1]/repprop))
      SIGshape[t] <- pSI[t-1]*Shat[t-1]*SIGrate[t]/repprop
      Ihat[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-Ihat[t]*beta)
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

## Negative-Binomial Process ----
if(process == "nb"){
  if(type == "dis"){
    process_code <- c("
      pDis ~ dgamma(pDshape,pDrate)
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
      pDis ~ dgamma(pDshape,pDrate)
      Ihat[1] ~ dgamma(i0,1/repprop)
      beta <- R0/N0
      pSI[1] <- 1 - exp(-Ihat[1]*beta)
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
