# Time of Use TOU-D-B

# Define TOU parameters
peak_hrs = 6 * 5 
offpeak_hrs = 8 * 5 + 14 * 2
superoff_hrs = 10 * 5 + 10 * 2

categories = c("Peak", "Off", "Super")
TOU = data.frame(TOU = categories, Hours = c(peak_hrs, offpeak_hrs, superoff_hrs)); TOU
TOU

TOU$Rate_summer = c(0.36538, 0.16255, 0.11835)
TOU$Rate_winter = c(0.24795, 0.15350, 0.12149); TOU
total = sum(TOU$Hours); total

TOU$Percent = round(TOU$Hours/total*100, 0)
# My Data csv?

july = c(396, 823, 924); 
aug = c(358, 880, 1011)
sep = c(600, 1220, 950)
oct = c(156, 377, 483)
nov = c(147, 375, 453)
dec = c(200, 400, 600)
jan = c(200, 400, 500)
feb = c(152, 389, 481)

my.usage = data.frame(Year = 2018, Month = c("July", "Aug", "Sept", "Oct", "Nov"), 
                      rbind(july, aug, sep, oct, nov), 
                      Total=c(sum(july), sum(aug), sum(sep), sum(oct), sum(nov))); 
                      my.usage 
names(my.usage) = c("Year", "Month", categories, "Total"); my.usage
#my.usage$Total = sum(my.usage$Peak, my.usage$`Off peak`); my.usage


###########################
# Calculate TOU Pricing
###########################

(my = t(my.usage[, c(3:5)])) # hours matrix

(tou.sum = (TOU[,3])) # pricing matrix for summer
(tou.wtr = (TOU[,4])) # pricing matrix for winter

(my.tou = rbind(round(t(tou.sum * my[,1:3]), 2), round(t(tou.wtr * my[, 4:5]), 2)))  # calculate cost


(my.tou.total = rowSums(my.tou))  # total monthly cost for TOU

(temp = cbind(my.usage, my.tou, my.tou.total))
rownames(temp) <- c()
names(temp) = c("Year", "Month", "Peak", "OffPeak", "SuperOffPeak", 
                "TotalHours", "PeakCost", "OffPeakCost", "SuperOffPeakCost", "TOUCost")
temp
###########################
# Calculate Tiered Pricing
###########################

tiered_rates = data.frame(MaxUse = c(298, 680, NA), 
                    Rate = c(.18, .25, .35)); tiered_rates

tiering = function(Total){
if(Total > tiered_rates$MaxUse[1]) {
  Tier1 <- tiered_rates$MaxUse[1]
  Tier2 <- Total - tiered_rates$MaxUse[1]  
  if(Tier2 > tiered_rates$MaxUse[2]) {
    Tier3 <- Tier2 - tiered_rates$MaxUse[2]
    Tier2 <- tiered_rates$MaxUse[2]
  } else
  { Tier3 = 0
  } 
  } else {
  Tier1 <- Total
  Tier2 = 0
  Tier3 = 0
  }
tiered_hours <- c(Tier1, Tier2, Tier3, Total)

tiered_costs <- c(Tier1*tiered_rates$Rate[1],
                  Tier2*tiered_rates$Rate[2],
                  Tier3*tiered_rates$Rate[3], 
                  sum(Tier1*tiered_rates$Rate[1],
                      Tier2*tiered_rates$Rate[2],
                      Tier3*tiered_rates$Rate[3]))

tiered_results = data.frame(Tier = c("Tier1", "Tier2", "Tier3", "Total"),
                            Hours = tiered_hours,
                            Cost = tiered_costs)

return(tiered_results)
}

tiering(80)
tiering(978)
tiering(2143)[4,c(2,3)]

#tiered_results

tiering(temp$Total[1])


##########################
# Combine Systems
##########################


temp$Tiered_hrs = 01
temp$Tiered_cost = -9999

for(i in 1:nrow(temp)){
  print(i)
  temp$Tiered_hrs[i] = tiering(temp$TotalHours[i])[4,c(2)]
  temp$Tiered_cost[i] = tiering(temp$TotalHours[i])[4,c(3)]
  }

temp$Per_OffPeakCost = 
  round(temp$OffPeakCost/temp$TOUCost * 100, 0)
temp$Per_PeakCost = 
  round(temp$PeakCost/temp$TOUCost * 100, 0)
temp$Per_SuperOffPeakCost = 
  round(temp$SuperOffPeakCost/temp$TOUCost *100, 0); head(temp)


# Weighted Hours...

(PeakPerUse = round(temp$Peak/temp$TotalHours * 100, 0))
(OffPeakPerUse = round(temp$OffPeak/temp$TotalHours * 100, 0))
(SuperOffPeakPerUse = round(temp$SuperOffPeak/temp$TotalHours * 100, 0))

(RelativePeakUse = round(PeakPerUse/TOU$Percent[1], 2))
(RelativeOffPeakUse = round(OffPeakPerUse/TOU$Percent[2], 2))
(RelativeSuperOffPeakUse = round(SuperOffPeakPerUse/TOU$Percent[3], 2))

temp = cbind(temp, RelativePeakUse)

summary <- subset(temp, 
                  select= c(Year, Month, TotalHours, Tiered_cost,
                            TOUCost, RelativePeakUse,
                            Per_PeakCost, Per_OffPeakCost, Per_SuperOffPeakCost 
                  )); summary

names(summary)  = c("Year", "Month", "kWh", "Tier Rate Total", 
                    "TOU Total", "Relative Peak Use", "%Peak", "%Off Peak", "%Super Off Peak")

summary

