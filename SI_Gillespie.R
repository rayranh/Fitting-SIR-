library(ggplot2) 
library(dplyr)
rm(list = ls())



sir <- function(beta, gamma, N, S0, I0, R0, tf) { 
  time <- 0 
  S <- S0 
  I <- I0 
  R <- R0 
  
  #empty vectors 
  ta <- numeric(0)  
  Sa <- numeric(0) 
  Ia <- numeric(0) 
  Ra <- numeric(0)
  #while time is less than final time, update ta with new time 
  while (time < tf) {  
    ta <- c(ta, time)
    Sa <- c(Sa, S)
    Ia <- c(Ia, I)
    Ra <- c(Ra, R)
    
    t <- beta*S*I + gamma*I
    if (t == 0) break
    
    #selecting random time, updating time 
    dt <- rexp(1, t) 
    time = time + dt 
    if (time > tf) break
    
    w <- (beta*S*I)/ (beta*S*I + gamma*I)
    randEvent = runif(1)
    if (randEvent < w) { 
      S<- S-1 
      I<- I+1
    } else {  
      I<- I-1 
      R<- R+1
    } 
  }
  
  return(data.frame(time = ta, S = Sa, I = Ia, R= Ra))
}  

sir_out = sir(beta = 0.1, gamma = 1/100, N= 1000, S0 = 999,I0 = 1, R0= 0,tf = 40000)
sir_out_long = melt(sir_out, "time") 
ggplot(sir_out_long, aes(x = time, y=value, colour = variable, group = variable)) + 
  geom_line(lwd=0.5) + 
  xlab("time") + ylab("Number")


sir_out_long %>% filter(variable == "R")





