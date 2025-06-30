library(deSolve)  
library(dplyr)
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
  beta = 0.0001, 
  gamma = 1/100) 

initialValues <- c(
  S = 999, 
  I = 1,  
  R=0) 

#times 

times <- seq(0,300,by= 1)  

results <- as.data.frame(ode(y = initialValues, 
                             times = times, func = SIR_odes, parms = pars)) 


plot(x =results$time, y=results$I, type = 'l', col = 'red', ylim = c(0,1000), main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$S , type = 'l', col= 'black')
lines(x = results$R,  type = 'l', col = 'blue') 
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("black","red", "blue"),
       lty=c(1,1,1)) 
