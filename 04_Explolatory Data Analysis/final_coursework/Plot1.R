###plot1.R ###
library(lubridate)
png(file="Plot1.png",bg="transparent",width=480,height=480)
NEI<- readRDS("summarySCC_PM25.rds")
NEI$year<- NEI$year %>% as.character%>% as.Date("%Y") %>% year
ne01<-with(NEI,tapply(Emissions,year,sum,na.rm=TRUE))
d1<-data.frame(Year=names(ne01),Total=ne01)
rownames(d1) <- NULL
#ggplot(data=d1,aes(x=year,y=log10(total))) +geom_point()+geom_line()
with(d1, plot(Year, log10(Total), main = "Total Emissions in the US", xlab="Year",ylab="Total Tonnage- log10", pch = 20))
dev.off()
