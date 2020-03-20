df <- data.frame(v0=100, # initial velocity
                 theta=1.4,  # angle in radians
                 gravity = 5,  # this is just picked for the scale
                 adj=0, # used in the bouncing effect
                 decay= 0.8,  # the "bounciness" of the ball
                 color="steelblue",  # color of the ball
                 cex=2,  # size of the ball
                 t=0,  # time position of this ball
                 xpos=0,  # current x position, will be overwritten
                 ypos=0)  # current y position, will be overwritten



# create a function, accepting in a data frame and counter
snapshot <- function(df, ct, outdir = "bouncing") {
  tval <- 0.3
  # open PNG device
  png(filename = sprintf("%s/bounce%04d.png", outdir, ct), width = 960, height = 540)
  # remove any margin
  par(mar = c(0, 0, 0, 0))
  # create blank canvas
  plot(c(0, 0), type = "n", col = "white", xlim = c(-1, 960), 
       ylim = c(-5, 540), yaxt = "n", ann = FALSE, xaxt = "n", bty = "n")
  
  # add baseline calculate new position using projectile formula
  df$ypos <- df$v0 * df$t * sin(df$theta) - (df$gravity * (df$t^2))
  df$xpos <- df$v0 * df$t * cos(df$theta) + df$adj
  # draw the point(s)
  points(df$xpos, df$ypos, type = "p", cex = df$cex, pch = 16, col = df$color)
  # check for anything bouncing
  for (x in seq(nrow(df))) {
    if (df$ypos[x] < 0) {
      # reset the bounce
      df$adj[x] <- df$xpos[x]
      df$v0[x] <- df$v0[x] * df$decay[x]
      df$t[x] <- -tval
    }
  }
  # if stuck, settle it.
  df$v0 <- ifelse(df$v0 < 0.01, 0, df$v0)
  df$t <- df$t + tval
  dev.off()
  df
}

for (i in seq(280)) {
  df <- snapshot(df, i, outdir = "/home/CAMPUS/mwl04747/github/beginnersluck/Epidemiology/oneball")
  # could put some status messages in here, this may take some time.
}

# system("avconv -f image2 -y -i oneball/bounce%04d.png  -r 25 -b 50000000 -s 1920x1080 -an oneball.mp4")


num <- 1500
set.seed(1492)  # to be repeatable
df <- data.frame(v0 = rnorm(num, 95, 15), theta = rnorm(num, 1.25, 0.16), gravity = 5.2, 
                 adj = 0, decay = rnorm(num, 0.8, 0.05), color = sample(colours(), num, replace = T), 
                 cex = rnorm(num, 2, 0.4), t = 0, xpos = 0, ypos = 0)
atime <- seq(2)
realdf <- df[atime, ]
df <- df[-atime, ]
for (i in seq(1100)) {
  realdf <- snapshot(realdf, i, outdir = "multiball")
  if (nrow(df)) {
    realdf <- rbind(realdf, df[atime, ])
    df <- df[-atime, ]
  }
  if (i%%20 == 0) {
    cat("executing on", i, "\n")
  }
}
