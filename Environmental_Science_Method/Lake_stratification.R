## Lake Stratification

profile = data.frame(
  Date = rep(seq.Date(from = as.Date("2020/1/1"), 
                  to = as.Date("2020/12/31"), 
                  by = "day"), each = 20),
  Depth = rep(seq(1,20,1), 366), Temperature = NA)

winterspring = 20
summerfall = 200
temp = NA
depth=1
for(depth in 1:2){
temp = data.frame(Day=1:366, Depth = depth, Temp = c(approx(x=c(1, winterspring),  
                y = c(3, 4), method = "linear", n=winterspring,
                rule = 1, f = 0, ties = mean)$y,
              approx(x=c(1, summerfall), 
                y = c(4, 10), method = "linear", n=summerfall-winterspring,
                rule = 1, f = 0, ties = mean)$y, 
              approx(x=c(1, summerfall), 
                y = c(10, 3), method = "linear", n=366-summerfall, 
                rule = 1, f = 0, ties = mean)$y))
temp = rbind(temp, temp)
}

print(temp)
