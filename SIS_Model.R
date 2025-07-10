library(deSolve)  
library(dplyr)
rm(list = ls())
#create a model 

SIR_odes <- function (t,x,params){ 
  #so i dont have to name the beta$# etc. 
  with(as.list(c(x,params)), { 
    
    dS <- -beta*S*I + gamma*I     
    dI <- beta*S*I - gamma*I 
    
    return(list(c(dS,dI)))  
  })
}

#parameters 
pars <- c( 
  beta = 0.0001, 
  gamma = 1/30) 

initialValues <- c(
  S = 999,  
  I = 1) 

#times 

times <- seq(0,1000,by= 1)  

results <- as.data.frame(ode(y = initialValues, 
                             times = times, func = SIR_odes, parms = pars)) 


plot(x =results$time, y=results$S, type = 'l', col = 'black', ylim = c(0,1000),  main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$time, y= results$I, type = 'l', col= 'red')
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("black","red", "blue"),
       lty=c(1,1,1)) 
