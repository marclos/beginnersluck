library(deSolve)
library(ggplot2)
library(gridExtra)
library(grid)

# Set the time period and step. Define the stocks and auxiliaries.
START <- 0
FINISH <- 100
STEP <- 0.25
simtime <- seq(START, FINISH, by = STEP)
stocks <- c(sStock = 100)
auxs <- c(aCapacity = 10000, 
          aRef.Availability = 1,
          aRef.GrowthRate = 0.10)

# Create the function
model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    aAvailability <- 1 - sStock / aCapacity
    aEffect <- aAvailability / aRef.Availability
    aGrowth.Rate <- aRef.GrowthRate * aEffect
    fNet.Flow <- sStock * aGrowth.Rate
    dS_dt <- fNet.Flow
    return(list(c(dS_dt), NetFlow = fNet.Flow,
                GrowthRate = aGrowth.Rate, 
                Effect = aEffect,
                Availability = aAvailability))
  })
}

# Create the data frame
sModel <- data.frame(ode(y = stocks, times = simtime, func = model,
                         parms = auxs, method = "euler"))

head(sModel)

# Run the following snippet to enable `multiplot`
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Plot the outputs using `multiplot`
stockTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, sStock), color = "purple")

availTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, Availability), color = "blue")

netflowTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, NetFlow), color = "red")

growthrateTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, GrowthRate), color = "green")

multiplot(stockTimePlot, netflowTimePlot, availTimePlot, growthrateTimePlot, cols = 2)
