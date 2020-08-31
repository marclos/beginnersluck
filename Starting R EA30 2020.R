#  R - Day #1

x <- seq(to= 20, from = 1, by= 1)
y <- x + rnorm(n= 20, mean=0, sd=5)

plot(x, y, pch=20, las=1, 
     main="y is function of x", 
     xlab="x (cm)", ylab="y (kg)")

# y = mx + b + error
bestfitline.lm = lm(y~x)
summary(bestfitline.lm)
coef(bestfitline.lm)
abline(coef(bestfitline.lm), col="purple", lwd=2)

# R - Day #2

temp <- round(rnorm(366, 65, 10),1)
jan <- mar <- may <- jul <- aug <- oct  <-dec <- 1:31
feb <- 1:29
apr <- jun <- sep <- nov <- 1:30

temprecords = data.frame(Year = 2020, Month = c(rep(1, 31), rep(2, 29), rep(3, 31), 
                     rep(4, 30), rep(5, 31), rep(6, 30),
                     rep(7, 31), rep(8, 31), rep(9, 30),
                     rep(10, 31), rep(11, 30), rep(12, 31)), 
           Day = c(jan, feb, mar, apr, may, jun, jul, aug, 
                   sep, oct, nov, dec),
           Temp = temp)

feb <- 1:28; temp <- round(rnorm(365, 63, 10),1)
temprecords2 = data.frame(Year = 2019, Month = c(rep(01, 31), rep(2, 28), rep(03, 31), 
                                   rep(04, 30), rep(05, 31), rep(06, 30),
                                   rep(07, 31), rep(08, 31), rep(09, 30),
                                   rep(10, 31), rep(11, 30), rep(12, 31)), 
                         Day = c(jan, feb, mar, apr, may, jun, jul, aug, 
                                 sep, oct, nov, dec),
                         Temp = temp)

paste(temprecords$Year, temprecords$Month, temprecords$Day, sep="")
temprecords = rbind(temprecords, temprecords2)

aggregate(Temp~Month, temprecords, mean)

aggregate(temprecords$Temp, by=list(temprecords$Month), mean)
aggregate(temprecords$Temp, by=list(temprecords$Month, temprecords$Year), mean)
temprecords$Date

temprecords$Date = paste(temprecords$Month,"-", 
                            temprecords$Day, "-", temprecords$Year, sep="")
str(temprecords)
plot(Temp~Date, data=temprecords)
newdate = as.character(temprecords$Date)
temprecords$NewDate = as.Date(newdate, format = c("%m-%d-%Y"))

str(temprecords)

plot(Temp~NewDate, data=temprecords)

abline(coef(lm(Temp~NewDate, data=temprecords)), col="red")
