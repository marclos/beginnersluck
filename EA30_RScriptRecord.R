#  R - Day #1
n <- 100
x <- seq(to= n, from = 1, by= 1)
set.seed(123) #
y <- x + rnorm(n= n, mean=0, sd=5)
y <- x + rlnorm(n, meanlog = 0, sdlog = 2) - rlnorm(n, meanlog = 0, sdlog = 2)
plot(x, y, pch=20, las=1, 
     main="y is function of x", 
     xlab="x (cm)", ylab="y (kg)")

hist(y); hist(log10(y))
y = log10(y)
# y = mx + b + error
bestfitline.lm = lm(y~x)
summary(bestfitline.lm)
coef(bestfitline.lm)
abline(coef(bestfitline.lm), col="purple", lwd=2)


## R Day # 5 Statistical Assumptions

par(mfrow=c(2,2))
plot(bestfitline.lm)
par(mfrow=c(1,1))

Anscombe = as.data.frame(
  matrix(c(
    1, 10, 10, 10, 8, 8.04, 9.14, 7.46, 6.58, 
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

names(Anscombe) = c("Obs.", "x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")

Anscombe

xmin = min(Anscombe[, 2:5]); xmin
xmax = max(Anscombe[, 2:5]); xmax

ymin = min(Anscombe[, 6:9]); ymin
ymax = max(Anscombe[, 6:9]); ymax

par(mfrow=c(2,2))
plot(y1 ~ x1, Anscombe, main="Dataset A", xlim=c(xmin,xmax), ylim=c(ymin,ymax)); 
  A.lm = lm(y1 ~ x1, Anscombe); abline(coef(A.lm)); 
  text(14,5, paste("slope =", round(coef(A.lm)[2], 3)));
  text(14,4, paste("intercept =", round(coef(A.lm)[1], 3)))

plot(y2 ~ x2, Anscombe, main="Dataset B", xlim=c(xmin,xmax), ylim=c(ymin,ymax)); 
  B.lm = lm(y2 ~ x2, Anscombe); abline(coef(B.lm))
  text(14,6, paste("slope =", round(coef(B.lm)[2], 3)));
  text(14,5, paste("intercept =", round(coef(B.lm)[1], 3)))

plot(y3 ~ x3, Anscombe, main="Dataset C", xlim=c(xmin,xmax), ylim=c(ymin,ymax)); 
  C.lm = lm(y3 ~ x3, Anscombe); abline(coef(C.lm))
  text(14,6, paste("slope =", round(coef(C.lm)[2], 3)));
  text(14,5, paste("intercept =", round(coef(C.lm)[1], 3)))

  plot(y4 ~ x4, Anscombe, main="Dataset D", xlim=c(xmin,xmax), ylim=c(ymin,ymax)); 
  D.lm = lm(y4 ~ x4, Anscombe); abline(coef(D.lm))
  text(14,6, paste("slope =", round(coef(D.lm)[2], 3)));
  text(14,5, paste("intercept =", round(coef(D.lm)[1], 3)))


summary(A.lm); summary(B.lm); summary(C.lm); summary(D.lm)


par(mfrow=c(2,2)); plot(A.lm); par(mfrow=c(1,1))


par(mfrow=c(2,2)); plot(B.lm); par(mfrow=c(1,1))


par(mfrow=c(2,2)); plot(C.lm); par(mfrow=c(1,1))


par(mfrow=c(2,2)); plot(D.lm); par(mfrow=c(1,1))

summary(A.lm)


