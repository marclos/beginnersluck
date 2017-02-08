# Creating a Plot with two vectors
# Sept 7th

# First let's create vectors
x <- c(4, 6, 39, 23, 12, 45, 12, 22)
y <- c(1, 8, 44, 34, 23, 29, 21, 32)

# What did we make?
x

#what's the first observation in x
x[1]

#third ?
x[3]


#fifth in y?
y[5]

plot(x, y)

#if y is a function of x, y=f(x) or y ~x

plot(y~x)

# Let's create a best fit line -- using a linear model

mymodel <- lm(y~x)

# Let's extract the coefficients. 
coef(mymodel)

summary(mymodel)

(rsq <- round(summary(mymodel)$r.squared, 2))

summary(mymodel)$fstatistic

as.vector(summary(mymodel)$fstatistic)
(pvalue <- round(pf(10.31, 1, 6, lower.tail=F), 3))

# Below doesn't work, I don't know why!
# pf(as.vector(summary(mymodel)$fstatistic), lower.tail=F)
# Now let's plot them
abline(coef(mymodel))

# Add text of results
text(20, 40, paste("R^2 =", rsq, ", p-value = ", pvalue))
