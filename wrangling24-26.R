library(dplyr)
setwd("C:/Users/Vincent/workspace/CS 360 Final Project")

age24 = read.csv("age24.csv")
age24$age = rep(24,dim(age24)[1])
age24[,5] = as.double(strsplit(as.character(age24[,5]),'%',fixed=TRUE))
age24[,6] = as.double(strsplit(as.character(age24[,6]),'%',fixed=TRUE))
age25 = read.csv("age25.csv")
age25$age = rep(25,dim(age25)[1])
age25[,5] = as.double(strsplit(as.character(age25[,5]),'%',fixed=TRUE))
age25[,6] = as.double(strsplit(as.character(age25[,6]),'%',fixed=TRUE))
age26 = read.csv("age26.csv")
age26$age = rep(26,dim(age26)[1])
age26[,5] = as.double(strsplit(as.character(age26[,5]),'%',fixed=TRUE))
age26[,6] = as.double(strsplit(as.character(age26[,6]),'%',fixed=TRUE))
first = TRUE

for(i in 1:length(age24$playerid)){
  if(age24$playerid[i] %in% age25$playerid & age24$playerid[i] %in% age26$playerid){
    season25 = age25[age25$playerid==age24$playerid[i],]
    season26 = age26[age26$playerid==age24$playerid[i],]
    totalPA = age24$PA[i] + season25$PA + season26$PA
    avgPA = totalPA/3
    avgBB. = age24$PA[i]/totalPA * age24$BB.[i] + season25$PA/totalPA * season25$BB. + season26$PA/totalPA * season26$BB.
    avgK. = age24$PA[i]/totalPA * age24$K.[i] + season25$PA/totalPA * season25$K. + season26$PA/totalPA * season26$K.
    avgBB.K = age24$PA[i]/totalPA * age24$BB.K[i] + season25$PA/totalPA * season25$BB.K + season26$PA/totalPA * season26$BB.K
    avgAVG = age24$PA[i]/totalPA * age24$AVG[i] + season25$PA/totalPA * season25$AVG + season26$PA/totalPA * season26$AVG
    avgOBP = age24$PA[i]/totalPA * age24$OBP[i] + season25$PA/totalPA * season25$OBP + season26$PA/totalPA * season26$OBP
    avgSLG = age24$PA[i]/totalPA * age24$SLG[i] + season25$PA/totalPA * season25$SLG + season26$PA/totalPA * season26$SLG
    avgOPS = age24$PA[i]/totalPA * age24$OPS[i] + season25$PA/totalPA * season25$OPS + season26$PA/totalPA * season26$OPS
    avgISO = age24$PA[i]/totalPA * age24$ISO[i] + season25$PA/totalPA * season25$ISO + season26$PA/totalPA * season26$ISO
    avgSpd = age24$PA[i]/totalPA * age24$Spd[i] + season25$PA/totalPA * season25$Spd + season26$PA/totalPA * season26$Spd
    avgBABIP = age24$PA[i]/totalPA * age24$BABIP[i] + season25$PA/totalPA * season25$BABIP + season26$PA/totalPA * season26$BABIP
    avgUBR = age24$PA[i]/totalPA * age24$UBR[i] + season25$PA/totalPA * season25$UBR + season26$PA/totalPA * season26$UBR
    avgwGDP = age24$PA[i]/totalPA * age24$wGDP[i] + season25$PA/totalPA * season25$wGDP + season26$PA/totalPA * season26$wGDP
    avgwSB = age24$PA[i]/totalPA * age24$wSB[i] + season25$PA/totalPA * season25$wSB + season26$PA/totalPA * season26$wSB
    avgwRC = age24$PA[i]/totalPA * age24$wRC[i] + season25$PA/totalPA * season25$wRC + season26$PA/totalPA * season26$wRC
    avgwRAA = age24$PA[i]/totalPA * age24$wRAA[i] + season25$PA/totalPA * season25$wRAA + season26$PA/totalPA * season26$wRAA
    avgwOBA = age24$PA[i]/totalPA * age24$wOBA[i] + season25$PA/totalPA * season25$wOBA + season26$PA/totalPA * season26$wOBA
    avgwRC. = age24$PA[i]/totalPA * age24$wRC.[i] + season25$PA/totalPA * season25$wRC. + season26$PA/totalPA * season26$wRC.
    if(first){
      avg.df = data.frame(matrix(c(season26$ï..Season,as.character(season26$Name),as.character(season26$Team),totalPA,avgBB.,avgK.,avgBB.K,avgAVG,avgOBP,avgSLG,avgOPS,avgISO,avgSpd,avgBABIP,avgUBR,avgwGDP,avgwSB,avgwRC,avgwRAA,avgwOBA,avgwRC.,season26$playerid,26),nrow=1))
      names(avg.df) = names(age24)
      first = FALSE
    }
    else{
      newrow = data.frame(matrix(c(season26$ï..Season,as.character(season26$Name),as.character(season26$Team),totalPA,avgBB.,avgK.,avgBB.K,avgAVG,avgOBP,avgSLG,avgOPS,avgISO,avgSpd,avgBABIP,avgUBR,avgwGDP,avgwSB,avgwRC,avgwRAA,avgwOBA,avgwRC.,season26$playerid,26),nrow=1))
      if(length(names(newrow))==length(names(age24))){
        names(newrow) = names(age24)
        avg.df = rbind(avg.df,newrow)
      }
      else{
        print(newrow)
      }
    }
  }
}
for(i in c(1,4:21)){
  avg.df[,i] = as.numeric(levels(avg.df[,i])[avg.df[,i]])
}
avg.df[,22] = as.integer(levels(avg.df[,22])[avg.df[,22]])
avg.df[,23] = as.integer(levels(avg.df[,23])[avg.df[,23]])
clusterData = scale(avg.df[,c(5,6,12,13,14)])

#Scree Plot
NumClusters = seq(2,10,1)
SumWithinss = sapply(2:10, function(x) sum(kmeans(clusterData,
                                                  centers=x, iter.max=1000)$withinss))
plot(NumClusters, SumWithinss, type="b")

set.seed(10)
NumClusters = 3
avg.df$cluster = kmeans(clusterData,centers=NumClusters,iter.max=1000,nstart=50)$cluster
clusterAddition = avg.df[,c(22,24)]

otherAges = read.csv("age27.csv")
otherAges = otherAges[otherAges$playerid %in% avg.df$playerid,]
otherAges[,5] = as.double(strsplit(as.character(otherAges[,5]),'%',fixed=TRUE))
otherAges[,6] = as.double(strsplit(as.character(otherAges[,6]),'%',fixed=TRUE))
otherAges$age = rep(27,dim(otherAges)[1])
otherAges = left_join(otherAges,clusterAddition)
for(i in 1:NumClusters){
  washouts = dim(avg.df[(!(avg.df$playerid %in% otherAges$playerid)) & avg.df$cluster==i,])[1] - dim(avg.df[avg.df$cluster==i & (avg.df[,1]+1)>2015,])[1]
  replacement = data.frame(0,"Mario Mendoza","Pittsburgh Pirates",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,.3,0,1,27,i)
  names(replacement) = names(otherAges)
  otherAges = rbind(otherAges,replacement[rep(1,washouts),])
}

for(age in 28:35){
  filename = paste("age",as.character(age),".csv",sep="")
  temp = read.csv(filename)
  temp = temp[temp$playerid %in% avg.df$playerid,]
  temp[,5] = as.double(strsplit(as.character(temp[,5]),'%',fixed=TRUE))
  temp[,6] = as.double(strsplit(as.character(temp[,6]),'%',fixed=TRUE))
  temp$age = rep(age,dim(temp)[1])
  temp = left_join(temp,clusterAddition)
  for(i in 1:NumClusters){
    washouts = dim(avg.df[(!(avg.df$playerid %in% temp$playerid)) & avg.df$cluster==i,])[1] - dim(avg.df[avg.df$cluster==i & (avg.df[,1]+age-24)>2015,])[1]
    replacement = data.frame(0,"Mario Mendoza","Pittsburgh Pirates",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,.3,0,1,age,i)
    names(replacement) = names(otherAges)
    otherAges = rbind(otherAges,replacement[rep(1,washouts),])
  }
  otherAges = rbind(otherAges,temp)
}

output = rbind(avg.df,otherAges)
names(output)[1] = "Season"
write.csv(output,"agingclustersUpdate.csv")