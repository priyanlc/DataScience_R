# Question 6
library(lubridate)
library(sqldf)
png(file="Plot6.png",bg="transparent",width=480,height=480)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
on_road <- sqldf("select fips,SCC,Pollutant,Emissions,type,year from NEI where type like '%ON-ROAD%’")
on_road_baltimore <- sqldf("select fips,SCC,Pollutant,Emissions,type,year from on_road where fips= '24510’")
on_road_los_angel <- sqldf("select fips,SCC,Pollutant,Emissions,type,year from on_road where fips= '06037’")
on_road_beltimore_summary <-with(on_road_beltimore,tapply(Emissions,year,sum,na.rm=TRUE))
on_road_los_angle_summary<-with(on_road_los_angel, tapply(Emissions,year,sum,na.rm=TRUE))
d1<-data.frame(year=names(on_road_beltimore_summary),total= on_road_beltimore_summary)
d2<-data.frame(year=names(on_road_los_angle_summary),total= on_road_los_angle_summary)
d1<-data.frame(year=namd1$year<-year(d1$year))
d2<-data.frame(year=namd1$year<-year(d2$year))
rownames(d1) <- NULL
rownames(d2) <- NULL
with(d1, plot(year, total, main = "Los Angeles motor vehicle emissions", xlab="Year",ylab="Total",pch = 20))
with(d2, plot(year, total, main = "Los Angeles motor vehicle emissions", xlab="Year",ylab="Total",pch = 20))
dev.off()
