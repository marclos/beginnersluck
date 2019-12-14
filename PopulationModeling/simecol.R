#install.packages("simecolModels", repos="http://R-Forge.R-project.org")
library(simecolModels)

## load individual-level model (DEB and phyto, differential equations)
submodel <- daphnia_deb_phyto()

## test one outer step alone
m <- sim(submodel)
out(m)

### create individual-based model (IBM, discrete, population dynamics)
### with a newly initialized daphnia_deb_phyto() object as submodel
deb_ibm <- daphnia_deb_ibm()

## set simulation parameters
times(deb_ibm)["to"] <- 2
## the following takes a while !
## Not run: 
times(deb_ibm)["to"] <- 200

## End(Not run)
deb_ibm <- sim(deb_ibm)
o <- out(deb_ibm)


### plot results
par(mfrow=c(3,1))
plot(o$time, o$x1, type="l",
     main="Phytoplankton", col="red", ylim=c(0,0.5),xlab="Day",ylab="mg C / L")
lines(o$time, o$x2, col="blue", lwd=2)
lines(o$time, o$x3, col="darkgreen")
plot(o$time, o$abundance, type="l", main="Abundance",xlab="Day", ylab="Ind / L", lwd=2)
bm.mgc <- o$weight * o$abund / 1000
plot(o$time, bm.mgc , type="l", main="Biomass",xlab="Day", ylab="mg C / L", lwd=2, col=)