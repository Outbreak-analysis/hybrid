priors <- c("
      #prior
      effprop ~ dbeta(effa,effb)
      repprop ~ dbeta(repa,repb)
            
      R0 ~ dgamma(Rshape,Rrate)
      N0 ~ dbin(effprop,N)
      "
)

S <- c("
      S[1] <- N0 - I[1]
      "
      ,"
      S[t] <- S[t-1] - I[t]
      ")
if(type=="hyb"){
  priors <- c("
      #prior
      effprop ~ dbeta(effa,effb)
      repprop ~ dbeta(repa,repb)
            
      R0 ~ dgamma(Rshape,Rrate)
      N0 <- effprop*N
      "
  )
  S <- c("
      Shat[1] <- N0*repprop - Ihat[1]
      "
         ,"
      Shat[t] <- Shat[t-1] - Ihat[t]
      ")
}

iterloop <- c("
      for(t in 2:numobs){"
      ,"
      }")

nimstart <- ("
  nimcode <- nimbleCode({
")

cat(nimstart
     , priors
     , process_code[1]
     , S[1]
     , observation_code[1]
     , iterloop[1]
     , process_code[2]
     , S[2]
     , observation_code[2]
     , iterloop[2]
     , "})",file=paste(rtargetname,".nimcode",sep=""))
