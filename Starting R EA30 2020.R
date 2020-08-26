#  R - Day #1

x <- seq(to= 20, from = 1, by= 1)


y <- x + rnorm(n= 20, mean=0, sd=5)
# y = mx + b + error


plot(x, y, pch=20, las=1, main="y is function of x", xlab="x (cm)", ylab="y (kg)")
bestfitline.lm = lm(y~x)
summary(bestfitline.lm)
coef(bestfitline.lm)
abline(coef(bestfitline.lm), col="purple", lwd=2)
