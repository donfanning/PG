library(foreign)
library(plotly)
library(ggplot2)
library(lm.beta)
library(stargazer)
library(ggplot)
library(ggmap)
library(RDSTK)
library(lme4)
library(car)
library(lmtest)



setwd("E:/Projects/Emil2016cluster")

# set the function --------------------------------------------------------


addzero <- function(data)
{
  len <- nchar(as.character(data))
  ml <- max(len) -1 
  data[len == ml] <- paste("0",data[len == ml],sep = "")
  return(data)  
}


# get  the file list  -----------------------------------------------------


files <- list.files("./company/", pattern= ".+csv")
files

# read files for pivot table ----------------------------------------------
full <- data.frame()

for (i in 1:length(files))
{
  fuck <- read.csv(paste("./company/",files[i],sep=""))[,c("City","company_na","GEOID","full_name","ALAND","X","Y","Match_addr","year","industry","employees","revenue","growth","foundingye","INTPTLON","INTPTLAT")]
  full <- rbind(full,fuck)
}
full <- full[complete.cases(full),]


# set the group -----------------------------------------------------------


full$employeesgroup  <- 1
full$employeesgroup[full$employees>= 20 & full$employees < 100] <- 2 
full$employeesgroup[full$employees>= 100 & full$employees < 500] <- 3 

full$employeesgroup[full$employees> 500] <- 4 

table(full$employeesgroup)

full$revenuegroup <- 1 
full$revenuegroup[full$revenue > 10000000 & full$revenue <= 50000000] <- 2 
full$revenuegroup[full$revenue > 50000000 & full$revenue <= 100000000] <- 3 
full$revenuegroup[full$revenue > 100000000 ] <- 4

table(full$revenuegroup)

full$growthgroup  <- 1 
full$growthgroup[full$growth > 1 & full$growth <= 3 ]  <- 2 
full$growthgroup[full$growth> 3]  <- 3 

table(full$growthgroup)

full$age <- 2017 - full$foundingye
full$agegroup  <- 1 
full$agegroup[full$age >= 10 & full$age < 20] <- 2 
full$agegroup[full$age >= 20] <- 3 
full$agegroup[full$age == 2017 ] <- 4

write.csv(full,"pivotdatanew.csv")

full <- read.csv("pivotdatanew.csv")

# read data for regression ------------------------------------------------


df <- data.frame()
for (i in 1:length(files))
{
  fuck <- read.csv(paste("./company/",files[i],sep=""))[,c("City","company_na","GEOID","full_name","ALAND","cbsa","employees")]
  df <- rbind(df,fuck)
}
df <- df[complete.cases(df),]
df$id <- 1: nrow(df)

df$GEOID <- as.character(df$GEOID)
df$geolen <- nchar(df$GEOID)
summary(df$geolen)
df$GEOID[df$geolen == 10] <- paste("0",df$GEOID[df$geolen == 10],sep="")
summary(nchar(df$GEOID))

count <- as.data.frame(table(df$GEOID))
head(count)
colnames(count)<- c("GEOID","count")

epcount <- aggregate(employees ~ GEOID, data= df, FUN = "sum")
#table(df$GEOID)

df1 <- read.csv("./household.csv")
df1$Geo_FIPS <- as.character(df1$Geo_FIPS)
df1$geolen <- nchar(df1$Geo_FIPS)
summary(df1$geolen)
df1$Geo_FIPS[df1$geolen == 10] <- paste("0",df1$Geo_FIPS[df1$geolen == 10],sep="")

household <- data.frame(GEOID = df1$Geo_FIPS, HH = df1$SE_T058_001)





count <- merge(count,household, by = "GEOID", all.x = T)


# get job from LECD -------------------------------------------------------


files <- list.files("./job/", pattern= ".+csv")
files
job <- data.frame()
for (i in 1:length(files)){
  j <- read.csv(paste("./job/",files[i],sep=""))
  job <- rbind(job,j)
}


job<- job[complete.cases(job),]
jobs <- data.frame(GEOID= job$w_geocode, jobs = job$C000)
jobs$GEOID <- as.character(jobs$GEOID)
jobs$geolen <- nchar(jobs$GEOID)
summary(jobs$geolen)
df$GEOID[df$geolen == 10] <- paste("0",df$GEOID[df$geolen == 10],sep="")


# get the data from SLD ---------------------------------------------------
slddata <- read.dbf("F:/SUMMER PROJECT/sld.dbf")

summary(nchar(as.character(slddata$GEOID10)))
sldgeoid  <- substr(as.character(slddata$GEOID10),1,11) 
slddata$GEOID <- sldgeoid



#try <- JH[JH$GEOID %in% count$GEOID,]
try <- slddata[slddata$GEOID %in% count$GEOID,]


JH <- data.frame(GEOID = sldgeoid, jobs = slddata$EMPTOT, wrks = slddata$WORKERS, HH = slddata$HH, CBSA_Name =as.character(slddata$CBSA) , CSA_Name =as.character(slddata$CSA) ,stringsAsFactors = F)
try <- JH[JH$GEOID %in% count$GEOID,]
try2 <- aggregate (cbind(jobs,HH,wrks)~ GEOID , try , sum)
write.csv(try2,"hahaha.csv")
try2 <- read.csv("hahaha.csv")
try2$GEOID <- addzero(try2$GEOID)
count <- merge(count, try2, by = "GEOID")

head(count)
sumc <- sum(count$count)
count$percent <- count$count / sumc


#get the all msa data

allmsa <- JH[JH$CBSA_Name %in% c("14500","10740","15540"),]

allmsa2 <- aggregate (cbind(jobs,HH)~ GEOID , allmsa , sum)

nmsa <- nrow(allmsa2)


# 2d ----------------------------------------------------------------------


count2 <- count[order(count$jobs),]
jobcount <- data.frame(job = count2$jobs, count = cumsum(count$percent))
job <- data.frame(job = count2$jobs, count = cumsum(count$percent))





# 3d ----------------------------------------------------------------------
dplot <- data.frame()

for (i in 0:100){
  for(j in 0:100){
    iHH =  36*i
    jjobs = 300*j
    subdf <- count[count$HH.x >= iHH | count$jobs >= jjobs,]
    subcum = sum(subdf$count)
    n1 <- nrow(subdf)
    n2 <- nrow(allmsa2[allmsa2$HH >= iHH | allmsa2$jobs >= jjobs,])
    percent <- n1/n2
    dplot <- rbind(dplot,c(iHH,jjobs,subcum,percent))
    
  }
}

colnames(dplot) <- c("households","jobs","companies","percent")
dplot <- dplot[dplot$companies > 0 ,]

plot_ly(dplot, x =~households, y =~jobs, z =~companies, color = ~percent) %>% add_markers(text = ~paste("Percentage of company census tract: ", percent))



# regression data -------------------------------------------------------------------

try <- slddata[slddata$GEOID %in% count$GEOID,]

try$d1ds <- try$D1D * try$AC_UNPR
try$d3bm3 <- try$D3bmm3 * try$AC_LAND
try$d3bm4 <- try$D3bmm4 * try$AC_LAND
try$d3bp3 <- try$D3bpo3 * try$AC_LAND
try$d3bp4 <- try$D3bpo4 * try$AC_LAND
try$d4aland <- try$D4a * try$AC_LAND
try$d4dland  <- try$D4d * try$AC_LAND

try2 <- aggregate (cbind(EMPTOT,HH,WORKERS,AC_UNPR,AC_LAND, E8_ENT10,d3bm3,d3bm4,d3bp3,d3bp4,d4aland,d4dland,D5ae,D5be)~ GEOID , try , sum)


try2$d1ds <- (try2$EMPTOT + try2$HH) /try2$AC_UNPR

try2$d2ajphh <- try2$EMPTOT / try2$HH
try2$d2rwrkemp <- try2$WORKERS / try2$EMPTOT
try2$d2entjob <- try2$E8_ENT10/try2$EMPTOT
try2$d2wrkemix <-  exp(-1 * abs((try2$WORKERS/try2$EMPTOT) -1))
try2$d2entden <- try2$E8_ENT10/try2$AC_UNPR

try2$d3b = (try2$d3bm3 *0.667) + try2$d3bm4 + (try2$d3bp3 *0.667) + try2$d3bp4
try2$d3b = try2$d3b/ try2$AC_LAND


try2$d4d <- try2$d4dland /try2$AC_LAND
try2$d5ae <- try2$D5ae
try2$d5be <- try2$D5be

scoredf <- read.csv("walkscore.csv", colClasses = "character")

try2<- merge(try2,scoredf,by="GEOID")

try2$walkscore <- as.numeric(try2$walkscore)

#try3 <-as.data.frame(try2[,c(2,16:25,27)])
try3 <-as.data.frame(scale(try2[,c(2,16:25,27)]))
try3$GEOID <- try2$GEOID


try3totry4 <- df[!duplicated(df$GEOID),]
try4 <- merge(try3,try3totry4,by = "GEOID",all.x = T, all.y= F)
try4$company_na <- NULL
try4$d2ajphh <- NULL
try4 <- merge(try4,count,by = "GEOID")
try4$ACRE <- try4$ALAND / 4046.86


head(try4)
try4$logden <- log(try4$count/try4$ACRE)
try5<- try4

write.csv(try5,"regressiondatanew.csv")
#try5 <- read.csv("regressiondatanew.csv")
try5$GEOID  <- addzero(try5$GEOID)
try5 <- merge(try5, epcount, by= "GEOID")

fit.1 <- lm(logden~ EMPTOT + d1ds +d2entjob + d3b + d4d +  d5ae +d5be, data = try4)
summary(fit.1)

fit.2 <- lm(logden~ EMPTOT + d1ds +d2entden + d3b  + d5ae +d5be, data = try4)
summary(fit.2)

fit.3 <-lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + d5be, data = try5)
summary(fit.3)
vif(fit.3)
core <- cor(fit.3$model[,2:7])
corrplot::corrplot(core)
write.csv(core, "correlationmatrix.csv")
fit.4 <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae , data = try5)
summary(fit.4)
core <- cor(fit.4$model[,2:6])
corrplot::corrplot(core)




# log density plot -----------------------------------------------------------------

head(try5)
ggplot(data = try5, aes(logden,fill = full_name)) + 
  geom_histogram() + 
  guides(fill=guide_legend(ncol=1))+ 
  theme(text = element_text(size=20),legend.title=element_blank())  + labs(x="Log of firm density", y = "Frequency")
ggsave("mtcars.eps", width = 14, height = 10)

# logall ------------------------------------------------------------------


try4$logEMPTOT <- log(try4$EMPTOT)
try4$logd1ds <- log(try4$d1ds)
try4$logd2entden <- log(try4$d2entden)
try4$logd3b <- log(try4$d3b)
try4$logd5ae <- log(try4$d5ae)
try4$logd5be <- log(try4$d5be)

fit.3 <- lm(logden~ logEMPTOT + logd1ds  +logd2entden + logd3b   +  logd5ae  +logd5be, data = try4)
fit.3 <- lm(logden~ EMPTOT + logd1ds +logd2entden + d3b  +  d5ae +d5be, data = try4)
fit.3 <- lm(logden~  logd1ds  +logd2entden + logd3b   +  logd5ae  +logd5be, data = try4)


summary(fit.3)


# betacoefficient ---------------------------------------------------------


install.packages("lm.beta")
fit.3 <- lm.beta(fit.2)



coef(fit.3)

# create index ------------------------------------------------------------


attach(try5)
index <- data.frame(d1ds ,walkscore ,d3b ,d5ae,d5be)
index$d5be[index$d5be < 0] <- 0
indexs <- as.data.frame(scale(index))
detach(try5)

colnames(indexs) <- c("d1ds" ,"walkscore" ,"d3b" ,"d5ae","d5be" )
attach(indexs)
#index <-  d1ds *0.3 +walkscore*0.2 +d3b*0.2 + d5ae*0.1 + d5be*0.2
index <-  d1ds  +walkscore +d3b+ d5ae + d5be
detach(indexs)


try5$index <- index

indexs$index <- index
indexs$logden <- try5$logden
indexs$EMPTOT <- try5$EMPTOT
indexs$GEOID <- try5$GEOID
censustractcor <- aggregate (cbind(INTPTLON,INTPTLAT)~ GEOID , full , mean)
censustractcor$GEOID <- addzero(censustractcor$GEOID )
indexs <- merge(indexs,censustractcor, by = "GEOID")

write.csv(indexs,"finalindex.csv")

fit.index <- lm(logden ~ EMPTOT + index , data = indexs)
summary(fit.index)

librar

stargazer(fit.1)
cord <- as.data.frame(scale(try2[,16:25]))

cord$d2ajphh <- NULL
corm <- cor(cord)



corrplot::corrplot(corm)	



# STlouis -----------------------------------------------------------------

stlouis <- full[full$City =="St Louis",]
qmplot(X, Y, data = stlouis, colour = I('red'), size = I(3), darken = .3)


# geocode -----------------------------------------------------------------
write.csv(full,"full.csv")
full <- read.csv("C:/Users/wahol/Google Drive/0_works/Emil2016cluster/full.csv")

\install.packages("RDSTK")

stlouis <- full[full$City =="St Louis",]
#geocode <-  street2coordinates(as.character(stlouis$Match_addr), session=getCurlHandle())
geo <- geocode(as.character(stlouis$Match_addr))
qmplot(lon, lat, data = geo, colour = I('red'), size = I(3), darken = .3)


durham <- full[full$full_name =="Atlanta-Sandy Springs-Marietta, GA",]
geod <- geocode(as.character(durham$Match_addr))
qmplot(X, Y, data = durham, colour = I('red'), size = I(2), darken = .3, maptype = "roadmap", source = "google")
#qmplot(lon, lat, data = geod, colour = I('red'), size = I(3), darken = .3)

Seattle <- full[full$City =="Seattle",]
sm <- merge(Seattle,scoredf,by = "GEOID")
qmplot(INTPTLON, INTPTLAT, data = Seattle, colour = I('red'), size = I(3), darken = .3)


install.packages("stringi")
library(stringi)

names <- as.data.frame(table(full$full_name))
names <- names[-c(7,11),]
len <- nrow(names)



ggmap(myMap)
dens <- kde2d.weighted(indexs$INTPTLON, indexs$INTPTLAT, indexs$index)
indexs$value <- indexs$index 

indexs$value[indexs$value > 3] <- 3


ggmap(myMap)+ geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))), data = cty  , color="black", size = 1, alpha = .7) 


myMap <- get_map(location=mylocation,source="google", maptype= "roadmap", crop=FALSE)
ggmap(myMap) +  
  
  geom_point(aes(x = as.numeric(as.character(INTPTLON)), y = as.numeric(as.character(INTPTLAT)),color= index), alpha = .5 ,data = indexs, size = 7 ) +  
  scale_color_gradientn( colours = heat.colors(2)) 





for (i in 1: len)
{
  cty <- full[full$full_name == names$Var1[i],]
  print(names$Var1[i])
  mylocation <- c(min(cty$X)-0.01,min(cty$Y)-0.01,max(cty$X)+0.01,max(cty$Y)+0.01)
  myMap <- get_map(location=mylocation,source="google", maptype= "roadmap", crop=FALSE)
  ggmap(myMap) +  
    
    geom_point(aes(x = as.numeric(as.character(INTPTLON)), y = as.numeric(as.character(INTPTLAT)),color= as.numeric(as.character(value))), alpha = .5 ,data = indexs, size = 7 ) +  
    scale_color_gradient2( low = "black",high = "yellow", mid = "red",midpoint = 1) + 
    geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))), data = cty  , color="black", size = 1, alpha = .7)   
  
  
  #qmplot(X, Y, data = cty, colour = I('red'), size = I(1.5), darken = .3,  source = "google")
  fn <- gsub("/", "_", names$Var1[i], fixed=TRUE)
  fn <- gsub("-", "_", fn, fixed=TRUE)
  filenames <- paste(fn,".png",sep="")
  ggsave(filenames)
}



cty <- full[full$full_name == "Seattle-Tacoma-Bellevue, WA",]
mylocation <- c(-122.4,47.542598,-122.1,47.7)
myMap <- get_map(location=mylocation, maptype= "toner-lite", crop=T,color = "bw")


ggmap(myMap, extent = "device") +  geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))), data = cty  , color="black", size = 0.7)   


ggsave("Seattle.png", dpi = 500)


cty <- full[full$full_name == "Washington-Arlington-Alexandria, DC-VA-MD-WV",]
mylocation <- c(-77.5,38.6,-76.7,39.2)
myMap <- get_map(location=mylocation, maptype= "toner-lite", crop=T,color = "bw")
myMap <- get_map(location=mylocation, maptype= "roadmap", crop=T,color = "bw")

ggmap(myMap, darken = c(0.7, "white"))

ggmap(myMap, extent = "device",darken = c(0.5, "white")) +  
  geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))), 
             data = cty  , color="black", size = 0.1)   


ggsave("figure2.tif", dpi = 800,width = 5,device = "tiff")


cty <- full[full$full_name == "Denver-Aurora-Broomfield, CO",]
mylocation <- c(-105.2,39.6,-104.7,39.78)
myMap <- get_map(location=mylocation, maptype= "toner-lite", crop=T,color = "bw")
ggmap(myMap)

ggmap(myMap, extent = "device",darken = c(0.5, "white")) 
+  geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))), data = cty  , color="black", size = 0.7)   


ggsave("Denver.png", dpi = 800,width = 5, device = "tiff")


cty <- full[full$full_name == "Boston-Cambridge-Quincy, MA-NH",]
mylocation <- c(min(cty$X)-0.01,42.2,-70.8,42.6)
myMap <- get_map(location=mylocation, maptype= "roadmap", crop=T,color = "bw")
meuse_basemap_attributes <- attributes(myMap)
meuse_basemap_transparent <- matrix(adjustcolor(myMap, 
                                                alpha.f = 0.5), 
                                    nrow = nrow(myMap))
attributes(meuse_basemap_transparent) <- meuse_basemap_attributes

ggmap(meuse_basemap_transparent, extent = "device",darken = c(0.5, "white")) +
  geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))), 
             data = cty  , color="black", size = 0.1)   +   
  scale_y_continuous(limits = c(42.1, 42.6)) +  scale_x_continuous(limits = c(-71.4, -70.8)) 



ggsave("figure3.tif", dpi = 800, width = 5, device= "tiff")

cty <- full[full$full_name == "Atlanta-Sandy Springs-Marietta, GA",]
mylocation <- c(-84.7,33.5,-83.9,34.3)

myMap <- get_map(location=mylocation, maptype= "roadmap", crop=T,color = "bw")
ggmap(myMap, extent = "device",darken = c(0.5, "white")) +  
  geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))),
             data = cty  , color="black", size = 0.1) 

ggsave("figure4.tif",device = "tiff", dpi = 800,width = 5)


cty <- full[full$full_name == "Phoenix-Mesa-Glendale, AZ",]
mylocation <- c(min(cty$X)-0.01,min(cty$Y)-0.01,max(cty$X)+0.01,33.75)

myMap <- get_map(location=mylocation, maptype= "roadmap", crop=T,color = "bw")
ggmap(myMap) 
ggmap(myMap, extent = "device",darken = c(0.5, "white")) +  
  geom_point(aes(x = as.numeric(as.character(X)), y = as.numeric(as.character(Y))), 
             data = cty  , color="black", size = 0.1) 

ggsave("figure5.tif", dpi = 800,device = "tiff",width =5 )


# multiple t test ---------------------------------------------------------

highgeoid <- as.character(count$GEOID[count$count > 5])
lowgeoid <- as.character(ount$GEOID[count$count == 1])

high <- slddata[slddata$GEOID %in% highgeoid,]
low <- slddata[slddata$GEOID %in% lowgeoid,]

nums <- sapply(high, is.numeric)
high <- high[,nums]
low <- low[,nums]

#mylist <- list(a=high,b=low)
output <- data.frame()
for (i in 1:111){
  test <- t.test(high[, i], low[, i])
  out <- data.frame("var1" = colnames(high)[i]
                    , "var2" = colnames(low[i])
                    , "t.value" = sprintf("%.3f", test$statistic)
                    ,  "df"= test$parameter
                    ,  "p.value" = as.numeric(sprintf("%.3f", test$p.value))
                    , "diff" = as.numeric(test$estimate[1] - test$estimate[2])
                    , stringsAsFactors = F)
  
  output <- rbind(output,out)               
}

output$p.value <- as.numeric(output$p.value)

apply(mylist, 2, function(x) {
  
  test <- t.test(Data[, x[1]], Data[, x[2]])
  
  out <- data.frame("var1" = colnames(Data)[x[1]]
                    , "var2" = colnames(Data[x[2]])
                    , "t.value" = sprintf("%.3f", test$statistic)
                    ,  "df"= test$parameter
                    ,  "p.value" = sprintf("%.3f", test$p.value)
  )
  return(out)
  
})



# multilevel model --------------------------------------------------------

install.packages("multilevel")
# create data 
metrodf <- read.csv("metro.csv")
#metrodf <- read.csv("metronew.csv")
attach(metrodf)
metrodf2 <- data.frame(Code,Metro,TotPerY,PopGrow ,College.,HT.LQ,HT.LabPool,Pat.100K,Res.I,NIH.cap,VC.cap,TurnOver,VC.cap,PopFlux,Dealmkrs,PctCommute)
detach(metrodf)
metrocor <- metrodf2[,3:ncol(metrodf2)]
stargazer(cor(metrocor), type = "html")

metrosrd <- metrodf2[,3:ncol(metrodf2)]

metrosrd <- as.data.frame(scale(metrosrd))


#colnames(metrosrd) <- colnames(metrodf2[,3:ncol(metrodf2)])
metrosrd$Code <- metrodf2$Code
metrosrd$Metro <- metrodf2$Metro
#multidf <- merge(try5,metrodf, by.x = "cbsa", by.y  ="CBSA")
multidf <- merge(try5,metrosrd, by.x = "cbsa", by.y  ="Code")

multidf$talents <- multidf$College. + multidf$HT.LabPool
multidf$innovation <- multidf$HT.LQ + multidf$Pat.100K



multidf$X.x <- NULL
write.csv(multidf,"multidf.csv")
multidf <- read.csv("multidf.csv")
# see multicol 

attach(multidf)
regressdata <- data.frame(logden,logempden,employees,EMPTOT,d1ds,walkscore,d3b,d4d,d5ae,d5be,TotPerY,PopGrow ,College.,HT.LQ,HTLabPool,
                          Pat.100K,Res.I,NIH.cap,VC.cap,TurnOver,VC.cap,PopFlux)
detach(multidf)

#attach(multidf)
#regressdata <- data.frame(logden,EMPTOT,d1ds,walkscore,d3b,d4d,d5ae,d5be,H.G.firms,TotPerY,PopGrow,College.,HT.LQ ,HT.LabPool,TurnOver,Res.I,
#                          NIH.cap ,VC.cap,Pat.100K,Central,Dealmkrs,Emerging,PctCommute,PopFlux,BusStarts,HTStarts,HighSchool,Centrality )
#detach(multidf)
regressdata <- apply(regressdata,2,as.numeric)
apply(regressdata,2,class)

corr <- cor(regressdata)
corrplot::corrplot(corr)

#run the regression

#multilevel test 
# Dealmkrs + BusStarts + HTStarts +HighSchool
fit.1 <-  lm (logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + TotPerY + PopGrow + College.+
                HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Central + Dealmkrs + Emerging +
                PctCommute + PopFlux + BusStarts + HTStarts + HighSchool +Centrality + LN
              , data = multidf)
#fit.2 <-  lm (logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + TotPerY + PopGrow + 
#                  HT.LQ + HTLabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + 
#                  X.Commute + PopFlux + BusStarts + HTStarts + HighSchool 
#              , data = multidf)

fit.1 <-  lm (logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + TotPerY + PopGrow + College.+
                HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Dealmkrs +
                PctCommute + PopFlux
              , data = multidf)

sort(vif(fit.1))
library(randomForest)
fit.2 <- randomForest(count ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + TotPerY + PopGrow + College.+
                        HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Central + Dealmkrs + Emerging +
                        PctCommute + PopFlux + BusStarts + HTStarts + HighSchool +Centrality + LN
                      , data = multidf)

fit.3 <- lm (logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + full_name, data = multidf)
varImpPlot(fit.2)
m1 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + (1 + TotPerY+ PopGrow  + HT.LQ |full_name), data = multidf)


m.empty <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + (1 |full_name), data = multidf)

m2 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + College.+
             HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Dealmkrs +
             PctCommute + PopFlux+ (1 |full_name), data = multidf)
display(m2)

m3 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + (1 + TotPerY + PopGrow + College.+
                                                                   HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Dealmkrs +
                                                                   PctCommute + PopFlux |full_name), data = multidf)


display(m3)

#real starts 
attach(multidf)
regressdata <- data.frame(logden,EMPTOT,d1ds,walkscore,d3b,d4d,d5ae,d5be,H.G.firms,TotPerY,PopGrow,College.,HT.LQ ,HT.LabPool,TurnOver,Res.I,
                          NIH.cap ,VC.cap,Pat.100K,Central,Dealmkrs,Emerging,PctCommute,PopFlux,BusStarts,HTStarts,HighSchool,Centrality )
detach(multidf)
library(ICC)head

m2 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + College.+
             HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Dealmkrs +
             PctCommute + PopFlux+ (1 |full_name), data = multidf)

ICCbare(full_name, logden, multidf)
srd <- as.data.frame(scale(regressdata))
srd$full_name <- multidf$full_name
m3 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + (1 |full_name), data = srd)

m4 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + (1 |full_name), data = srd)
summary(m4)

m5 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae+ d5be  + TotPerY + PopGrow + PopFlux + (1 |full_name), data = multidf)
summary(m5)


m6 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  +  College.+
             HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Dealmkrs + (1 |full_name), data = srd)
summary(m6)

m7 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  +  College.+
             HT.LQ + HT.LabPool + TurnOver + Res.I + NIH.cap + VC.cap + Pat.100K  + Dealmkrs + PctCommute+ (1 |full_name), data = srd)
summary(m7)


srd$talents <- srd$College. + srd$HT.LabPool
srd$innovation <- srd$HT.LQ + srd$Pat.100K

m8<- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + talents + innovation  + TurnOver + Res.I + NIH.cap + VC.cap   + Dealmkrs + (1 |full_name), data = srd)
summary(m8)

m9 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae +d5be + talents + innovation  + TurnOver + 
             Res.I + NIH.cap + VC.cap   + Dealmkrs +  PctCommute + (1 |full_name), data = multidf)
summary(m9)

m10 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae +d5be + TotPerY + PopGrow+ 
              PopFlux + TurnOver  + talents + innovation + Res.I + NIH.cap + VC.cap   + Dealmkrs +
              PctCommute + (1 |full_name), data = multidf)
summary(m10)



mul  <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + TurnOver  + talents + innovation + 
             Res.I + NIH.cap + VC.cap  + Dealmkrs +
             PctCommute + PopFlux , data = multidf)

m11 <- lmer(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + d5be + (1 |full_name), data = multidf)
olscollage  <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae +College. , data = multidf)

ols  <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  +d5be , data = multidf)
olsdv  <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  +d5be + full_name, data = multidf)
olspartdv  <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + TurnOver + PopFlux +full_name, data = multidf)
olspart  <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + TurnOver + PopFlux, data = multidf)

olsfull  <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + TurnOver  + talents + innovation + Res.I + NIH.cap +
                 VC.cap   + Dealmkrs +
                 PctCommute + PopFlux , data = multidf)

olsfulldv <- lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  + TotPerY + PopGrow + TurnOver  + talents + innovation + Res.I + NIH.cap + VC.cap   + Dealmkrs +
                  PctCommute + PopFlux +full_name, data = multidf)
summary(olsfull)
sort(vif(mul))


# plot --------------------------------------------------------------------
library(stargazer)
stargazer(m11,m5,m9,type = "html")
stargazer(ols,olsdv, type = "html")

stargazer(m3,m5,m9,m10,type = "html")
stargazer(ols,olspart,olsfull,olsdv,olspartdv,olsfulldv,type = "html",column.labels = c("OLS","OLSsupply","OLSmetro","OLS with dv","OLSsupply with dv","OLSmetro with dummy"))

x <- ranef(m2)$full_name
xx <- as.data.frame(x)
colnames(xx) <- 'intercept'
xx$name <- row.names(xx)
#xx <- xx[order(xx$intercept),]
se <- se.ranef(m2)
xx$se <- as.numeric(se$full_name)

ggplot(data = xx, aes(x = name, y= intercept)) + geom_point() + coord_flip() + geom_hline(yintercept = 0)

ggplot(data = xx, aes(x = name, y= intercept, ymin = intercept - 1.96* se, ymax = intercept + 1.96 * se)) + geom_pointrange() + coord_flip() + geom_hline(yintercept = 0)

ggplot(data = x, aes(`(Intercept)`)) + geom_bar()


# ols plot ----------------------------------------------------------------


x <- coef(olsdv)[7:length(coef(olsdv))]
xx <- as.data.frame(x)
colnames(xx) <- 'coef'
xx$name <- row.names(xx)
se <- summary(ols)[["coefficients"]][7:34,2] 
xx$se <- as.numeric(se)
ggplot(data = xx, aes(x = name, y= coef, ymin = coef - 1.96* se, ymax = coef + 1.96 * se)) + geom_pointrange() + 
  coord_flip() + geom_hline(yintercept = 0) #+ geom_hline(yintercept=0.27670 )



# fixeffect ---------------------------------------------------------------
library(plm)
fix <- plm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  , data=srd, index=c("full_name"), model="within")
random <- plm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae  , data=srd, index=c("full_name"), model="random")
phtest(fix, random)

summary(random)





# dignose test ------------------------------------------------------------
try5 <- read.csv("regressiondata.csv")
fit.3 <-lm(logden ~ EMPTOT + d1ds + walkscore + d3b  +  d5ae + d5be, data = try5[try5$d5be >= 0,])

summary(fit.3)
library(lmtest)
ncvTest(fit.3)
try5$logd1ds <- log(try5$d1ds)
try5$logEMPTOT <- log(try5$EMPTOT)
try5$logd3b <- log(try5$d3b)
try5$logd5ae <- log(try5$d5ae)
try5$d5be
try5$logd5be <- log(try5$d5be)
fit.3<-lm(logden ~    logd1ds + walkscore + logd3b  +d5ae , data = try5)
summary(fit.3)
fit.4<-lm(logden ~     d1ds + walkscore + d3b  +d5ae + d5be, data = try5[try5$d5be >= 0,])
vif(fit.4)
ncvTest(fit.4)

summary(fit.4)
fit.5<-lm(logden ~    logd1ds + walkscore + d3b  +d5ae , data = try5)
summary(fit.5)

fit.6<-lm(logden ~  logd1ds + walkscore + logd3b  , data = try5)
summary(fit.6)
ncvTest(fit.6)
vif(fit.6)

head(fit.6$model)

modelresult <- fit.6$model
modelresult$residual <- residuals
try5$GEOID  <- addzero(try5$GEOID)
modelresult$GEOID <- try5$GEOID
library(foreign)
write.dbf(modelresult,"modelresult3.dbf")
modelresult <- read.dbf("modelresult.dbf")
library(het.test)
library(vars)

model1 <- vars::VAR(as.data.frame(fit.4$model), p = 1) 
whites.htest(model1) 
fit.7<-lm(logden ~  EMPTOT + index , data = indexs)
summary(fit.7)
ncvTest(fit.7)
vif(fit.7)
Model.8.gls = gls(logden ~ log(d1ds) + walkscore + log(d3b),
                  data=try5)
summary(Model.8.gls)


Model.8.gls = gls(logden ~ log(d1ds) + walkscore + log(d3b) +factor(full_name),
                  data=try5)




# ############## FINAL ----------------------------------------------------

lm.result = lm(logden ~ index +factor(full_name), data=try5)
lm.result2 = lm(logden ~ EMPTOT + index, data=try5)

vif(lm.result)
bptest(lm.result)

gls.result = nlme::gls(logden ~ EMPTOT + index +factor(full_name), data=try5, weights = varPower())

vif(gls.result)
bptest(gls.result)


summary(lm.result)
summary(gls.result)

stargazer(lm.result, type = "html")
summary(Model.8.gls)
Sum.Model.8.gls = round(summary(Model.8.gls)$tTable, dig=4)
bptest(Model.8.gls)
bptest(fit.3)
bptest(fit.4)
bptest(fit.6)
bptest(fit.7)

library(stargazer)
attach(try5)
summ <- data.frame(count,ACRE,logden,EMPTOT,d1d = d1ds,walkscore,d3b,d5ae, d5be)
detach(try5)
stargazer(summ, type = "html")
x <- as.data.frame(summary(summ))


# metro paper  -------------------------------------------------------------

#reg BusStarts   TotPerY PopGrow  Dense1mi  Pat_100K Res_I NIH_cap HT_LQ College_Plus  HTLabPool TurnOver  PctForBorn  No_within_50km NE MA EMW SA ESC WSC MT PAC, vce(robust)

#reg HTStarts   TotPerY PopGrow  Dense1mi  Pat_100K Res_I NIH_cap HT_LQ College_Plus  HTLabPool TurnOver  PctForBorn  No_within_50km NE MA EMW SA ESC WSC MT PAC, vce(robust)
s = lm(BusStarts  ~ TotPerY + PopGrow + Dense1mi + Pat_100K + Res_I +NIH_cap +HT_LQ +College_Plus  +HTLabPool +TurnOver  +PctForBorn  +No_within_50km +NE +MA +EMW +SA +ESC+ WSC+ MT +PAC,
       data=data)
Model.BusStarts2 = nlme::gls(BusStarts ~ TotPerY + PopGrow + Dense1mi + Pat_100K + Res_I +NIH_cap +HT_LQ +College_Plus  +HTLabPool +TurnOver  +PctForBorn  +No_within_50km +NE +MA +EMW +SA +ESC+ WSC+ MT +PAC,
                             data=data,weights = varPower())
fm2 <- update(Model.BusStarts2, weights = varIdent(form = ~1 | TotPerY))
fm3 <- update(Model.BusStarts2, weights = varPower())

Model.HTStarts = nlme::gls(BusStarts  ~ TotPerY + PopGrow + Dense1mi + Pat_100K + Res_I +NIH_cap +HT_LQ +College_Plus  +HTLabPool +TurnOver  +PctForBorn  +No_within_50km +NE +MA +EMW +SA +ESC+ WSC+ MT +PAC,
                           data=data,weights = varPower())


library(stargazer)


stargazer(Model.BusStarts,Model.HTStarts)


