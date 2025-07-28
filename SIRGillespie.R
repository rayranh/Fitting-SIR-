rm(list = ls())
library(ggplot2) 
library(dplyr) 
library(reshape2)

time <- 0 

S<- numeric(0)
I <- numeric(0) 
R <- numeric(0)   
t <- numeric(0)

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


# df <- as.data.frame(GillespFunc(beta = 0.001, gamma = 1/20, timeEnd = 200, S0 = 999, I0 = 1, R0 = 0)) 

out <- replicate(5, GillespFunc(beta = 0.001, gamma = 1/20, timeEnd = 100, S0 = 900, I0 = 100, R0 = 0), simplify = FALSE)


# Convert each simulation to a data.frame and add an ID column
sim_dfs <- lapply(seq_along(out), function(i) {
  df <- as.data.frame(out[[i]])
  colnames(df) <- c("S", "I", "R", "time")
  df$sim <- paste0("Sim_", i)
  return(df)
})

# Combine all into one data frame
combined_df <- bind_rows(sim_dfs)

# Reshape to long format for ggplot
df_long <- melt(combined_df, id.vars = c("time", "sim"))

# Plot
ggplot(df_long, aes(x = time, y = value, color = variable)) +
  geom_line() + scale_color_manual (values = c("S"="black", "I" = "red", "R" = "blue"))+ 
  labs(title = "Gillespie SIR Simulations", x = "Time", y = "Count") +
  theme_minimal()
