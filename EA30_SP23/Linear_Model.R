## Lineaer Model


# linear model
# some y ~ x
# DO ~ depth
# Temp ~ Depth
# DO ~ Temp

names(import)

lm(HDO..Sat ~ Depth.m, data=import)

DO.lm <- lm(HDO..Sat ~ Depth.m, data=import)
summary(DO.lm)

