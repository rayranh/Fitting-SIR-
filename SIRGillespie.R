rm(list = ls())
library(ggplot2) 
library(dplyr) 
library(reshape2) 
library(tidyr)

time <- 0 

S<- numeric(0)
I <- numeric(0) 
R <- numeric(0)   
t <- numeric(0) 

timeValues <- seq(from = 0, to = 100, by = 0.1)

set.seed(234)   
GillespFunc <- function(beta, gamma, timeEnd, S0, I0, R0) {   
  while (time < timeEnd) {  
    
    #need to update S and I 
    S<-  c(S,S0) 
    I <- c(I,I0) 
    R <- c(R,R0)
    t <- c(t,time)
    
    #updating time value with time till next event, the rate is the total rate for any event 
    if (I0 == 0) break
    dt <- rexp(1,rate = beta*S0*I0 + gamma*I0)    
    time <- time + dt     
    
    
    #creating random event 
    
    randEvent <- runif(1) 
    
    w <- (beta*S0*I0)/(beta*S0*I0+ gamma*I0) 
    if (randEvent < w) { 
      S0 = S0 -1 
      I0 = I0 + 1 
      R0 = R0 
    } else {  
      S0 = S0 
      I0 = I0 - 1 
      R0 = R0 + 1 
    } 
  }  
  return(cbind(S,I,R,t))
  }

#df <- as.data.frame(GillespFunc(beta = 0.001, gamma = 1/20, timeEnd = 200, S0 = 999, I0 = 1, R0 = 0)) 

#list of matrices 
out <- replicate(10, GillespFunc(beta = 0.01, gamma = 1/20, timeEnd = 100, S0 = 50, I0 = 1, R0 = 0), simplify = FALSE) 

#standardizing time 
finalList <- lapply(out, function(simMatrix) { 
  SimTime <- simMatrix[, "t"]  
  
  
  # For each standardized time value, find the most recent simulation time (t <= tv)
  rowTime <- sapply(timeValues, function(tv) { 
    w <- which(SimTime <= tv)
    if (length(w) == 0) NA else max(w) 
  }) 
  
  # Extract t and S values based on matched rows, t() transposing so rows and columns are like dflong 
  result <- t(sapply(rowTime, function(i) {
    if (!is.na(i)) simMatrix[i, c("S", "I", "R")] else c(NA, NA, NA)
  })) 
  
  df <- as.data.frame(result)
  df$time <- timeValues  # standardized time column   
  df
}) 



#adding sim id, sim 1, sim 2, etc. 
sim_dfs <- bind_rows(finalList, .id = "sim")  
df_long <- melt(sim_dfs, , id.vars = c("time", "sim"))

CI <- df_long %>% group_by(time, variable) %>% summarise(average = mean(value), 
                                                         lower = quantile(value, 0.025),
                                                         upper = quantile(value, 0.975), 
                                                         med = median(value), 
                                                         .groups = "drop")

# Plot
ggplot(df_long, aes(x = time, y = value, color = variable, group = interaction(sim, variable))) +
  geom_line(aes(alpha = 10)) + scale_color_manual (values = c("S"="black", "I" = "red", "R" = "blue"))+
  geom_ribbon(data = CI, aes(x = time, ymin = lower, ymax = upper, fill = variable, group = variable), alpha = 0.3, inherit.aes = FALSE)+
  scale_fill_manual(values = c("S" = "black", "I" = "red", "R" = "blue")) +
  labs(title = "Gillespie SIR Simulations", x = "Time", y = "Count") +
  theme_minimal()



