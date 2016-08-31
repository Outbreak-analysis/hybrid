
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
      "
      ,"
      SIGrate[t] <- 1/(1-pSI[t-1])
      SIGshape[t] <- pSI[t-1]*S[t-1]*SIGrate[t]/repprop
      I[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-I[t]*beta)
      "
    )
  }
}

## Beta-Binomial Process ----
if(process == "bb"){
  if(type == "dis"){
    process_code <- c("I[1] ~ dbin(initDis,i0)
                      beta <- exp(-R0/N0)
                      pSI[1] <- 1 - beta
                      "
      ,"I[t] ~ dbin(pSI[t-1],S[t-1])
      pSI[t] <- 1 - exp(I[t]*log(beta))
      "
    )
  }
  if(type == "hyb"){
    process_code <- c("I[1] ~ dgamma(i0,1/repprop)
                      beta <- R0/N0
                      pSI[1] <- 1 - exp(-I[1]*beta)
                      "
      ,"SIGrate[t] <- 1/(1-pSI[t-1])
      SIGshape[t] <- pSI[t-1]*S[t-1]*SIGrate[t]/repprop
      I[t] ~ dgamma(SIGshape[t],SIGrate[t])
      pSI[t] <- 1 - exp(-I[t]*beta)
      "
    )
  }
}