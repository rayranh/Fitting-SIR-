library(deSolve)  
library(dplyr)
rm(list = ls())
#create a model 

SIR_odes <- function (t,x,params){ 
  #so i dont have to name the beta$# etc. 
  with(as.list(c(x,params)), { 
    
  dS <- -beta*S*(I+I2+I3+I4+I5)  
  dI <- beta*S*(I+I2+I3+I4+I5) - gamma*I 
  dI2 <- gamma*I-gamma*I2  
  dI3 <- gamma*I2-gamma*I3 
  dI4 <- gamma*I3-gamma*I4 
  dI5 <- gamma*I4-gamma*I5
  dR <- gamma*I5  
  
  return(list(c(dS,dI,dI2,dI3,dI4,dI5,dR)))  
  })
}

#parameters 
pars <- c( 
  beta = 0.0001, 
  gamma = 1/100*5) 

initialValues <- c(
  S = 999, 
  I = 1,  
  I2 = 0, 
  I3 = 0, 
  I4 = 0, 
  I5 = 0, 
  R=0) 

#times 

times <- seq(0,300,by= 1)  

results <- as.data.frame(ode(y = initialValues, 
                             times = times, func = SIR_odes, parms = pars)) 

results <- results %>% mutate(I_total = I + I2 + I3 + I4 + I5)

plot(x =results$time, y=results$I_total, type = 'l', col = 'red', ylim = c(0,1000), main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$S , type = 'l', col= 'black')
lines(x = results$R,  type = 'l', col = 'blue') 
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("black","red", "blue"),
       lty=c(1,1,1)) 
