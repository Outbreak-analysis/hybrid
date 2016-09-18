library(nimble)
library(coda)

filenames <- list.files(pattern=".Rds")

temp <- head(filenames)
getsum <- function(n){
  nimbleobject <- readRDS(n)
  name <- unlist(strsplit(n,"[.]"))
  # type/process/observation/sample/iteration/RDS
  sum_output <- nimbleobject$summary
  if(name[1]=="dis"){
    sumdf <- rbind(sum_output[,,1],sum_output[,,2],sum_output[,,3])
    nplat <- 3
    temp_platform <- rep(c("jags","nimble","nimble_slice"),3)
  }
  if(name[1]=="hyb"){
    sumdf <- rbind(sum_output[,,1],sum_output[,,2],sum_output[,,3]) 
    nplat <- 4
    temp_platform <- rep(c("jags","nimble","nimble_slice","stan"),3)
  }
  rownames(sumdf) <- NULL
  sumdf <- data.frame(sumdf)
  fulldf <- data.frame(sample=name[4]
                       , type=name[1]
                       , process=name[2]
                       , observation=name[3]
                       , parameter=rep(c("R0","effprop","repprop"),each=nplat)
                       , true_parameter=rep(c(R0,effprop,repprop),each=nplat)
                       , sumdf
                       , platform=temp_platform
                       , timing=c(rep(c(nimbleobject$timing[1:nplat]),3))
  )
}

combineDat <- function(nlist){
  dat <- data.frame()
  for(i in nlist){
    dat <- rbind(dat,getsum(i))
  }
  return(dat)
}

dat <- combineDat(filenames)

saveRDS(dat,file="temp.RDS")
