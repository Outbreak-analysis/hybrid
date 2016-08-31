priors <- c("
      #prior
      repprop ~ dbeta(1,1)
      effprop ~ dbeta(1,1)
      initDis ~ dbeta(1,1)
            
      R0 ~ dgamma(0.1,0.1)
      N0 ~ dbinom(effprop,N)
      "
)

S <- c("
      S[1] <- N0 - I[1]
      "
      ,"
      S[t] <- S[t-1] - I[t]
      ")
if(type=="hyb"){
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
