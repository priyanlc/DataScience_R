# Plot3.R
library(lubridate)
library(ggplot2)
png(file="Plot3.png",bg="transparent",width=480,height=480)
NEI<- readRDS("summarySCC_PM25.rds")
ne02<-NEI[(NEI$fips==24510),]
ne02$year<- ne02$year %>% as.character%>% as.Date("%Y") %>% year
qplot(year,Emissions,data=ne02,color=type,xlab="Year",ylab="Tonnage",main="Baltimore City Emissions by Type")+geom_smooth(method="lm")+ylim(0,400)
dev.off()
