library(deSolve)  
library(dplyr)
rm(list = ls())
#create a model 

SIR_odes <- function (t,x,params){ 
  #so i dont have to name the beta$# etc. 
  with(as.list(c(x,params)), { 
    
    dS <- b-beta*S*I - mu*S     
    dE <- beta*S*I - lambda*E -mu*E 
    dE2 <- lambda*E - lambda*E2 - mu*E2
    dE3 <- lambda*E2 - lambda*E3 - mu*E3 
    dE4 <- lambda*E3 - lambda*E4 - mu*E4 
    dE5 <- lambda*E4- lambda*E5 - mu*E5 
    dI <- lambda*E5 - gamma*I - mu*I 
    dR <- gamma*I  - mu*R 
    
    return(list(c(dS,dE,dE2,dE3,dE4,dE5,dI,dR)))  
  })
}

#parameters 
pars <- c( 
  beta = 0.1, 
  gamma = 1/100, 
  lambda = 0.04, 
  mu = 0.001, 
  b = 10) 

initialValues <- c(
  S = 999,  
  E = 1, 
  E2 = 0, 
  E3 = 0, 
  E4 = 0, 
  E5 = 0, 
  I = 0,  
  R=0) 

#times 

times <- seq(0,10000,by= 1)  

results <- as.data.frame(ode(y = initialValues, 
                             times = times, func = SIR_odes, parms = pars)) 


plot(x =results$time, y=results$S, type = 'l', col = 'black', ylim = c(0,10000), xlim = c(0,1000),  main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$time, y= results$I, type = 'l', col= 'red')
lines(x = results$time, y=results$R, type = 'l', col = 'blue') 
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("black","red", "blue"),
       lty=c(1,1,1)) 
