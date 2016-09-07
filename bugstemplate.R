priors <- c("
      #prior
      effprop ~ dbeta(9,2)
      repprop ~ dbeta(9,9)
            
      R0 ~ dgamma(2,1)
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
      effprop ~ dbeta(9,2)
      repprop ~ dbeta(9,9)
            
      R0 ~ dgamma(2,1)
      N0 <- effprop*N
      "
  )
  S <- c("
      S[1] <- N0*repprop - I[1]
      "
         ,"
      S[t] <- S[t-1] - I[t]
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
