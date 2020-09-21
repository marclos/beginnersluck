#  R - Day #1
n <- 100
x <- seq(to= n, from = 1, by= 1)
set.seed(123)
y <- x + rnorm(n= n, mean=0, sd=5)
y <- x + rlnorm(n, meanlog = 0, sdlog = 2) - rlnorm(n, meanlog = 0, sdlog = 2)
plot(x, y, pch=20, las=1, 
     main="y is function of x", 
     xlab="x (cm)", ylab="y (kg)")

# y = mx + b + error
bestfitline.lm = lm(y~x)
summary(bestfitline.lm)
coef(bestfitline.lm)
abline(coef(bestfitline.lm), col="purple", lwd=2)


## R Day # 5 Statistical Assumptions

par(mfrow=c(2,2))
plot(bestfitline.lm)
par(mfrow=c(1,1))

Anscombe = as.data.frame(matrix(c(1, 10, 10, 10, 8, 8.04, 9.14, 7.46, 6.58, 
2, 8, 8, 8, 8, 6.95, 8.14, 6.77, 5.76,
3, 13, 13, 13, 8, 7.58, 8.74, 12.74, 7.71,
4,  9, 9, 9, 8, 8.81, 8.77, 7.11, 8.84,
5, 11, 11, 11, 8, 8.33, 9.26, 7.81, 8.47,
6, 14, 14, 14, 8, 9.96, 8.10, 8.84, 7.04,
7,  6, 6, 6, 8, 7.24, 6.13, 6.08, 5.25,
8,  4, 4, 4, 19, 4.26, 3.10, 5.39, 12.50,
9, 12, 12, 12, 8, 10.84, 9.13, 8.15, 5.56,
10, 7, 7, 7, 8, 4.82, 7.26, 6.42, 7.91,
11, 5, 5, 5, 8, 5.68, 4.74, 5.73, 6.89), 
nrow=11,  byrow=TRUE))

names(Anscombe) = c("No", "x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")


plot(y1 ~ x1, Anscombe); A.lm = lm(y1 ~ x1, Anscombe)
par(mfrow=c(2,2)); plot(A.lm); par(mfrow=c(1,1))

plot(y2 ~ x2, Anscombe); B.lm = lm(y2 ~ x2, Anscombe)
par(mfrow=c(2,2)); plot(B.lm); par(mfrow=c(1,1))

plot(y2 ~ x2, Anscombe); C.lm = lm(y2 ~ x2, Anscombe)
par(mfrow=c(2,2)); plot(C.lm); par(mfrow=c(1,1))

plot(y2 ~ x2, Anscombe); D.lm = lm(y1 ~ x1, Anscombe)
par(mfrow=c(2,2)); plot(D.lm); par(mfrow=c(1,1))


# R - Day #2

temp19 <- round(rnorm(365, 64, 10),1)
temp20 <- round(rnorm(366, 65, 10),1)

date = seq(as.Date("2018/08/31"), by = "day", length.out = 731)

records <- data.frame(Date=date, Temp = c(temp19, temp20))

library(lubridate)
records$Month = month(records$Date)
records$Year = year(records$Date)

aggregate(Temp~Month, records, mean)

str(records)
plot(Temp~Date, data=records)

str(records)
plot(Temp~Date, data=records)
abline(coef(lm(Temp~Date, data=records)), col="red")

aggregate(records$Temp, by=list(records$Month), mean)

records2 <- within(records, {
  monthlies <- paste(year(Date),
                     month(Date))
})

monthly = aggregate(records2$Temp, by=list(records2$monthlies), mean, na.rm = TRUE)
names(monthly)=c("Date", "Temp")
str(monthly)
newdate = as.character(monthly$Date)
NewDate = as.Date(monthly$Date, format=c("%Y %m"))

plot(Temp~Date, data=monthly)
abline(coef(lm(Temp~Date, data=monthly)), col="red")

temprecords$Date = paste(temprecords$Month,"-", 
                         temprecords$Day, "-", temprecords$Year, sep="")
str(temprecords)
plot(Temp~Date, data=temprecords)
newdate = as.character(temprecords$Date)
temprecords$NewDate = as.Date(newdate, format = c("%m-%d-%Y"))

str(temprecords)
library(dplyr)
df <- records %>%
  mutate( X1 = dmy(Date),Year = year(X1), Month = month(X1)) %>%
  group_by(Month, Year) %>%
  summarise(result = mean(Temp) )

plot(Temp~NewDate, data=temprecords)

abline(coef(lm(Temp~NewDate, data=temprecords)), col="red")

## Statistic Assumptions
