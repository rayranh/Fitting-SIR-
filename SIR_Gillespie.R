
library(ggplot2) 
library(dplyr) 
library(reshape2)
rm(list = ls())

time <- 0 
timeEnd<- 1000000000


beta = 0.00001  
gamma = 1/500  

S0 = 999 
I0 = 1
R0 =  0  


S<- numeric(0)
I <- numeric(0) 
R <- numeric(0)   
t <- numeric(0)



while (time < timeEnd) {  
  
  #need to update S and I 
  S<-  c(S,S0) 
  I <- c(I,I0) 
  R <- c(R,R0)
  t <- c(t,time)
  
  #updating time value with time till next event, the rate is the total rate for any event  
  if (I0 == 0) break # the stupid order 
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

out_df <- data.frame (time = t, S=S, I=I, R=R) 


df_long <- melt(out_df, id.vars = c("time"))

# Plot
ggplot(df_long, aes(x = time, y = value, color = variable)) + geom_line() + 
  scale_color_manual (values = c("S"="black", "I" = "red", "R" = "blue"))+ 
  labs(title = "Stochastic SIR Simulation", x = "Time", y = "Individuals") +
  theme_minimal()
