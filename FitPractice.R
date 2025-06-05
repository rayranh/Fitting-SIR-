library(deSolve) 

#create a model 

SIR_odes <- function (t,x,params){ 
  S <- x[1] 
  I <- x[2]
  R <- x[3] 
  beta <- params[1] 
  gamma <- params[2] 
  dS <- -beta*S*I 
  dI <- beta*S*I - gamma*I - alpha*I 
  dR <- gamma*I 
  list(c(dS,dI,dR))
  
  
  }

#parameters 

#gamma = 0.005 
gamma <- 0.005

#beta = 0.0001 
beta <- 0.0001  

alpha <- 0.005 



#times 

t<- seq(0,30000,by= 1) 

results <- as.data.frame(ode(y = c(S=100, I=1, R=0), 
                             times = t, func = SIR_odes, parms = c(beta,gamma, alpha))) 

plot(x =results$time, y=results$I, type = 'l', col = 'red', ylim = c(0,100), main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$S , type = 'l', col= 'black')
lines(x = results$R,  type = 'l', col = 'blue') 
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("black","red", "blue"),
        lty=c(1,1,1)) 









