library(mosaic)

soln = integrateODE(dx ~ r*x/k, x-0, tdur=list(from=0, to=4))

ode(y, times, func, parms, method=c("lsoda")

y state variable.names

time

func

parms 

method

library(deSolve)
egrowth <- function(times, y, parms){
  dN.dt <- p[1]*y[1]
  return(list(dN.dt))}

p = .5; y0 <- 2; t=0:20
sol <-ode(y=y0, times=t, func= egrowth, parms=p)

str(sol); plot(t, sol[,2], type='l')

lgrowth <- function(times, y, parms){
  dN.dt <- p[1]*y[1]*(1-y[1]/K)
  return(list(dN.dt))}

p = .5; y0 <- 2; t=0:20; K=1000

sol <-ode(y=y0, times=t, func= lgrowth, parms=p)
str(sol); plot(t, sol[,2], type='l')

# use data to make function...
new = as.vector(sol[,2]); new[7:21]=NA; new

lgrowth2 <- function(times, y, parms){
  dN.dt <- p[1]*y[1]*(1-(y[1]/y[2]))
  return(list(dN.dt))}

p = c(.5); y1= 1000; y0 <- new[1]; t=0:20

sol <-ode(y=c(y0, y1), times=t, func= lgrowth2, parms=p)
str(sol); plot(t, sol[,2], type='l')




Covid <- function(times, y, parms){
  dN.dt <- p[1]*C*(1-(C/y[1]))
  return(list(dN.dt))}

C = sort(LA$Confirmed)[32:length(LA$Confirmed)]
t = 1:length(C); t
p = c(.5)
y0 = 100000

sol <-ode(y = y0, times=t, func= Covid, parms=p)

str(sol); plot(t, sol[,2], type='l')


#### SIR ###
install.packages("devtools")
library(devtools)
install_github("sbfnk/fitR")
library(fitR)
data(SIR)
names(SIR)


theta <- c(R0 = 3, D_inf = 2)
init.state <- c(S = 999, I = 1, R = 0)
times <- 1:100
traj <- SIR$simulate(theta, init.state, times)
head(traj)

plotTraj(traj)

####

####
SIR Model
http://web.stanford.edu/class/earthsys214/notes/fit.html

fit.logistic <- function(par,y){
  r <- par[1]; k <- par[2]
  t <- y[,2]
  n <- y[,1]
  n0 <- y[1]
  tmp <- n[1] *exp(r*t)/(1 + n[1] * (exp(r*t)-1)/k)
  sumsq <- sum((n - tmp)^2)
}


usa <- c(3.929214,   5.308483,   7.239881,   9.638453,  12.866020,  
         17.866020, 23.191876,  31.443321,  38.558371,  50.189209,
         62.979766,  76.212168, 92.228496, 106.021537, 123.202624,
         132.164569, 151.325798, 179.323175, 203.302031, 226.542199,
         248.709873, 281.421906, 308.745538)
year <- seq(1790,2010,10) # decennial census
years = 140 
r.guess <- (log(usa[15])-log(usa[1]))/years
k.guess <- usa[15] #1930 US population
par <- c(r.guess,k.guess)
census1930 <- cbind(usa[1:15], seq(0,years,by=10))
usa1930.fit <- optim(par,fit.logistic,y=census1930)
usa1930.fit$par



logistic.int <- expression(n0 * exp(r*t)/(1 + n0 * (exp(r*t) - 1)/k))
r <- usa1930.fit$par[1]
k <- usa1930.fit$par[2]
n0 <- usa[1]
t <- seq(0,220,by=10)
plot(seq(1790,2010,by=10), usa, type="n", xlab="Year", 
     ylab="Total Population Size (Millions)")
lines(seq(1790,2010,by=10), eval(logistic.int), 
      lwd=2, col=grey(0.85))
points(seq(1790,2010,by=10),usa, pch=16, col="black")
abline(v=1930, lty=3, col="red")

### Attempt at Covid19

C
t = 1:length(C); days=max(t); days

r.guess <- (log(C[length(C)])-log(C[1]))/days; r.guess
k.guess <- C[length(C)]
par <- c(r.guess,k.guess)
covid19 <- cbind(C, t); covid19
covid19.fit <- optim(par,fit.logistic,y=covid19)
covid19.fit$par

logistic.int <- expression(n0 * exp(r*t.new)/(1 + n0 * (exp(r*t.new) - 1)/k))
r <- covid19.fit$par[1]
k <- covid19.fit$par[2]
n0 <- C[1]
plot(t, C, type="n", xlab="Days", 
     ylab="Infected", xlim=c(1,100), ylim=c(0,10000), las=1)
t.new = 1:100
lines(t.new, eval(logistic.int), lwd=2, col=grey(0.85))
points(t, C, pch=16, col="red", cex=.5)
#abline(v=1930, lty=3, col="red")


### SIR Approach

require(deSolve)
sir <- function(t,x,parms){
  S <- x[1]
  I <- x[2]
  R <- x[3]
  with(as.list(parms),
       {
         dS <- -beta*S*I
         dI <- beta*S*I - nu*I
         dR <- nu*I
         res <- c(dS,dI,dR)
         list(res)
       })
}

require(bbmle)
# likelihood function
sirLL <- function(lbeta, lnu, logN, logI0) {
  parms <- c(beta=plogis(lbeta), nu=plogis(lnu))
  x0 <- c(S=exp(logN), I=exp(logI0), R=0)
  out <- ode(y=x0, weeks, sir, parms)
  SD <- sqrt(sum( (cumbombay-out[,4])^2)/length(weeks) )
  -sum(dnorm(cumbombay, mean=out[,4], sd=SD, log=TRUE))
}

bombay <- c(0, 4, 10, 15, 18, 21, 31, 51, 53, 97, 125, 183, 292, 390, 448,
            641, 771, 701, 696, 867, 925, 801, 580, 409, 351, 210, 113, 65, 
            52, 51, 39, 33)
cumbombay <- cumsum(bombay)
weeks <- 0:31
plot(weeks, cumbombay, pch=16, xlab="Weeks", ylab="Cumulative Deaths")
# minimize negative-log-likelihood
fit <- mle2(sirLL, 
            start=list(lbeta=qlogis(1e-5), 
                       lnu=qlogis(.2), 
                       logN=log(1e6), logI0=log(1) ),  
            method="Nelder-Mead",
            control=list(maxit=1E5,trace=0),
            trace=FALSE)

summary(fit)

theta <- as.numeric(c(plogis(coef(fit)[1:2]),
                      exp(coef(fit)[3:4])) )

parms <- c(beta=theta[1], nu = theta[2])
times <- seq(0,30,0.1)
x0 <- c(theta[3],theta[4],0)
stateMatrix1 <- ode(y=x0, times, sir, parms)
colnames(stateMatrix1) <- c("time","S","I","R")
plot(stateMatrix1[,"time"], stateMatrix1[,"R"], type="l", lwd=2, 
     xaxs="i", xlab="Time", ylab="Cumulative Deaths")
points(weeks, cumbombay, pch=16, col="red")

### Covid19 application
require(bbmle)

# likelihood function
sirLL <- function(lbeta, lnu, logN, logI0) {
  parms <- c(beta=plogis(lbeta), nu=plogis(lnu))
  x0 <- c(S=exp(logN), I=exp(logI0), R=0)
  out <- ode(y=x0, t, sir, parms)
  SD <- sqrt(sum( (C-out[,4])^2)/length(t) )
  -sum(dnorm(C, mean=out[,4], sd=SD, log=TRUE))
}


plot(t, C, pch=16, xlab="Weeks", ylab="Cumulative Deaths")
# minimize negative-log-likelihood
covid.fit <- mle2(sirLL, 
            start=list(lbeta=qlogis(1e-5), 
                       lnu=qlogis(.2), 
                       logN=log(1e6), logI0=log(1) ),  
            method="Nelder-Mead",
            control=list(maxit=1E5,trace=0),
            trace=FALSE)

summary(covid.fit)


theta <- as.numeric(c(plogis(coef(covid.fit)[1:2]),
                      exp(coef(covid.fit)[3:4]))); theta

parms <- c(beta=theta[1], nu = theta[2])

times <- seq(0,100,0.1)

x0 <- c(theta[3],theta[4],0)
stateMatrix1 <- ode(y=x0, times, sir, parms)
colnames(stateMatrix1) <- c("time","S","I","R")
plot(stateMatrix1[,"time"], stateMatrix1[,"R"], type="l", lwd=2, 
     xaxs="i", xlab="Days", ylab="Confirmed Cases")
points(t, C, pch=16, col="red", cex=.5)

covid.fit@vcov

# adding trace to see progress...
#fit2 <- mle2(sirLL, start=as.list(coef(fit)), fixed=list(logN=coef(fit)[3],  logI0=coef(fit)[4]), method="Nelder-Mead",control=list(maxit=1E5,trace=2),trace=TRUE)

# WTF?

#We can think of the outcomes as a process-error framework. Rather than using a normal model for the number of deaths as measured with error, we model the deaths directly as a Poisson random variable.


## A different approach
sirLL2 <- function(lbeta, lnu, logN, logI0) {
  parms <- c(beta=plogis(lbeta), nu=plogis(lnu))
  x0 <- c(S=exp(logN), I=exp(logI0), R=0)
  out <- ode(y=x0, t, sir, parms)
  -sum(dpois(C, lambda=out[,4], log=TRUE))
}

fit.pois <- mle2(sirLL2, 
                 start=list(lbeta=qlogis(1e-5), 
                            lnu=qlogis(.2), 
                            logN=log(1e6), logI0=log(1) ),  
                 method="Nelder-Mead",
                 control=list(maxit=1E5,trace=2),
                 trace=TRUE)

summary(fit.pois)

theta2 <- as.numeric(c(plogis(coef(fit.pois)[1:2]),
                       exp(coef(fit.pois)[3:4])) )

parms <- c(beta=theta2[1], nu = theta2[2])
times <- seq(0,30,0.1)
x0 <- c(theta2[3],theta2[4],0)
stateMatrix2 <- ode(y=x0, times, sir, parms)
colnames(stateMatrix2) <- c("time","S","I","R")
plot(stateMatrix2[,"time"], stateMatrix2[,"R"], type="l", lwd=2, 
     xaxs="i", xlab="Time", ylab="Cumulative Deaths")
lines(stateMatrix1[,"time"], stateMatrix1[,"R"], col=grey(0.85), lwd=2)
points(weeks, cumbombay, pch=16, col="red")
legend("topleft", c("Poisson", "Gaussian"), lwd=2, col=c("black",grey(0.85)))