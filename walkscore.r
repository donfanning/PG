setwd("E:/Projects/Emil2016cluster")
df <- read.csv("hahaha.csv")

addzero <- function(data){
  len <- nchar(as.character(data))
  ml <- max(len) -1 
  data[len == ml] <- paste("0",data[len == ml],sep = "")
  return(data)  
}



files <- list.files("./company/", pattern= ".+csv")
files
full <- data.frame()
for (i in 1:length(files)){
  fuck <- read.csv(paste("./company/",files[i],sep=""))[,c("City","company_na","GEOID","full_name","ALAND","X","Y","Match_addr","INTPTLAT","INTPTLON")]
  full <- rbind(full,fuck)
}
full <- full[complete.cases(full),]

df <- full
df$id <- 1: nrow(df)
df$GEOID <- as.character(df$GEOID)
df$geolen <- nchar(df$GEOID)
summary(df$geolen)
df$GEOID[df$geolen == 10] <- paste("0",df$GEOID[df$geolen == 10],sep="")
summary(nchar(df$GEOID))


geo <- df[!duplicated(df$GEOID),]
library(walkscoreAPI)
library(jsonlite)
library(rjson)
key <- '7b5f5718994a3e81d53c70ea908b43a0'
x <- df$X[327]
y <- df$Y[327]

getwalkscore <- function(x,y){
URL <- paste("http://api.walkscore.com/score?format=json&lat=", y, "&lon=", 
             x, "&transit=1&bike=1&wsapikey=", key, sep = "")
score <- jsonlite::fromJSON(URL)
walk <-score$walkscore
bike <-score$bike$score
transit <- score$transit$score
return(c(walk,bike,transit))
}

getwalkscore(df$X[3],df$Y[3])
scoredf3 <- data.frame()

for (i in 1:nrow(geo))
{
  print(i)
  scoredf3 <- rbind(scoredf2,c(geo$GEOID[i],getwalkscore(geo$INTPTLON[i],geo$INTPTLAT[i])))
}
scoredf2.b <- scoredf2
scoredf2 <- scoredf2[,-1]
colnames(scoredf2) <- c("walkscore","bikescore")
scoredf2$GEOID <- geo$GEOID
write.csv(scoredf,"walkscore.csv")

http://api.walkscore.com/score?address=1119%8th%20Avenue%20Seattle%20WA%2098101&lat=47.6085&on=-122.3295&transit=1&bike=1&wsapikey=7b5f5718994a3e81d53c70ea908b43a0


