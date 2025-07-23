library(deSolve)  
library(dplyr) 
library(ggplot2) 
library(reshape2)
rm(list = ls())
#create a model 

SIR_odes <- function (t,x,params){ 
  #so i dont have to name the beta$# etc. 
  with(as.list(c(x,params)), { 
    
    dS <- -beta*S*I     
    dI <- beta*S*I - gamma*I
    dR <- gamma*I 
    
    return(list(c(dS,dI,dR)))  
  })
}

#parameters 
pars <- c( 
  beta = 0.001, 
  gamma = 1/20) 

initialValues <- c(
  S = 999,  
  I = 1,  
  R=0)



#times 

times <- seq(0,100,by= 0.1)  

results <- as.data.frame(ode(y = initialValues, 
                             times = times, func = SIR_odes, parms = pars))  

longResults <- melt(results, id.vars = "time", value.name = "value")

ggplot(data = longResults, mapping = aes(x = time , y = value, colour = variable, group = variable)) +
  geom_line() + scale_color_manual (values = c("S"="black", "I" = "red", "R" = "blue")) + theme_minimal() + 
  labs(title = "Stochastic SIR Simulation", x = "Time", y = "Individuals") 


