## Making a histogram

names(import)

import$SpCond.uS.cm

mean(import$SpCond.uS.cm)
## look at structure

str(import)
names(import)

# histogram of pH
hist(import$pH.units)
mean(import$pH.units)
sd(import$pH.units)