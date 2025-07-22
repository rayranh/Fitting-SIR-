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

# resultLong <- melt(results, id.vars = c("time"))
# 
# ggplot(data = resultLong, mapping = aes(x = time, y = S)) + 
#   geom_line(color = 'variable') + geom_line(mapping = aes(x = time, y = I, color = '')) + 
#   geom_line(mapping = aes(x = time, y = R, color = 'blue')) 

plot(x =results$time, y=results$S, type = 'l', col = 'black', ylim = c(0,1000), xlim = c(0,100),  main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$time, y= results$I, type = 'l', col= 'red')
lines(x = results$time, y=results$R, type = 'l', col = 'blue') 
legend("right", title="key", legend=c("susceptible", "infected", "recovered"),  
       bty = "n",
       cex = 0.6,
       col = c("black","red", "blue"),
       lty=c(1,1,1)) 
