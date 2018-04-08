remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# set.seed(1)
# x <- rnorm(100)
# x <- c(-10, x, 10)
# y <- remove_outliers(x)
# par(mfrow = c(1, 2))
# boxplot(x)
# boxplot(y)


plot(storm_data_death_sum$FATALITIES,storm_data_inj_sum$INJURIES)

model1 <- lm(storm_data_inj_sum$INJURIES~storm_data_death_sum$FATALITIES)

model2 <- lm(storm_data_death_sum$FATALITIES~storm_data_inj_sum$INJURIES)

abline(model1, lwd = 2,col=3)

abline(model2, lwd = 2,col=4)


a <- remove_outliers(storm_data_death_sum$FATALITIES)
b <- remove_outliers(storm_data_inj_sum$INJURIES)


length(a)
length(b)


fet<-storm_data_death_sum$FATALITIES

inj<-storm_data_death_sum$INJURIES

fet_arr<-arrange(storm_data_death_sum,desc(FATALITIES))

inj_arr<-arrange(storm_data_inj_sum,desc(INJURIES))

inj<-arrange(desc(inj))




length(storm_data_death_sum$FATALITIES)
length(storm_data_inj_sum$INJURIES)


plot(a,b)


print(fet_arr$FATALITIES)

print(inj_arr$INJURIES)

head(fet_arr,10)

head(inj_arr,10)


year <- c(2000 ,   2001  ,  2002  ,  2003 ,   2004)
rate <- c(9.34 ,   8.50  ,  7.62  ,  6.93  ,  6.60)

plot(year,rate,
       main="Commercial Banks Interest Rate for 4 Year Car Loan",
       sub="http://www.federalreserve.gov/releases/g19/20050805/")
cor(year,rate)

fit <- lm(rate ~ year)
fit


