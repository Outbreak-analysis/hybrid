targetname <-unlist(strsplit(rtargetname,"[.]"))

type <- targetname[2]
process <- targetname[3]
observation <- targetname[4]
seed <- as.numeric(targetname[5])
iterations <- as.numeric(targetname[6])