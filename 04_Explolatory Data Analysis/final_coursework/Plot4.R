library(lubridate)
#library(sqldf)
png(file="Plot4.png",bg="transparent",width=480,height=480)
NEI<- readRDS("summarySCC_PM25.rds")
SCC<- readRDS("Source_Classification_Code.rds")
mergedData <- merge(NEI, SCC, as.x = "SCC", as.y = "SCC")
# names(mergedData)[names(mergedData)=="Short.Name"] <- "Short"
#a2s <- sqldf("select fips,SCC,Pollutant,Emissions,type,year from mergedData where Short like '%Coal%’”)
md<-data.table(mergedData)
a2s <- md[grep("Coal", md$Short.Name), ]
coal<-with(a2s,tapply(Emissions,year,sum,na.rm=TRUE))
d1<-data.frame(year=names(coal),total=coal)
rownames(d1) <- NULL
with(d1, plot(year, total, main = "US Coal use", pch = 20))
dev.off()

