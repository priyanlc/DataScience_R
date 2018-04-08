###plot2.R ###
library(lubridate)
png(file="Plot2.png",bg="transparent",width=480,height=480)
NEI<- readRDS("summarySCC_PM25.rds")
ne02<-NEI[(NEI$fips==24510),]
ne02$year<- ne02$year %>% as.character%>% as.Date("%Y") %>% year
ne02<-with(ne02,tapply(Emissions,year,sum,na.rm=TRUE))
d1<-data.frame(Year=names(ne02),Total=ne02)
rownames(d1) <- NULL
with(d1, plot(Year, Total, main = "Baltimore City",xlab="Year",ylab="Total tonnage", pch = 20))
# total emissions have decreased
model <- lm(Year ~ Total, d1)
abline(model, lwd = 2)
dev.off()
