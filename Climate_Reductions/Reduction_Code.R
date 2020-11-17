

# current emissions totals

Calculating the annualized performance of an investment or index using yearly data uses the following data points:
  
  P = principal, or initial investment
  
  G = gains or losses
  
  n = number of years
  
  AP = annualized performance rate
  
  The generalized formula, which is exponential to take into account compound interest over time, is:
    
    AP = ((P + G) / P) ^ (1 / n) - 1

# reduction level

rate.fun <- function(refyear, goalyear, ref, current, pergoal){
  ref_reduction = ref*pergoal; goal=ref-ref_reduction; n = goalyear-refyear
  reduction=current-goal
  rate = round(((current - reduction)/current)^(1/n)-1,4)
  # test fv = 1000*(1+.1)^5; fv
  future = current*(1+ rate)^n
  output = data.frame(Ref_Year=refyear, Goal_Year=goalyear, 
        Ref_Emmissions=ref, Current_Emmissions=current, Goal=goal, Reduction_Rate=rate, Future_Emmissions=future)
  return(output)
}

rate.fun(refyear = 2020, goalyear= 2050, ref=5000, current= 5400, pergoal=.40)
