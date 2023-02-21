## Read csv

sonde.csv = "/home/mwl04747/beginnersluck/EA30_SP23/Field Data Recording (master sheet) - Data.csv"

## read data into R... many statements in R are have specific vocabulary

sonde = read.csv(sonde.csv)

## linear model

## lm(y~x)

lm(HDO..Sat ~ Depth.m, data=sonde)

DO.lm = lm(HDO..Sat ~ Depth.m, data=sonde)

coef(DO.lm)
## plot the data and add best fit line...

plot(HDO..Sat ~ Depth.m, data=sonde, xlab="Depth (m)", ylab="DO (% Saturation)", 
     las=1, main="pHake Lake Dissolved Oxygen vs. Depth")

abline(coef(DO.lm), col="red")
PlotPl