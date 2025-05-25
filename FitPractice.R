library(deSolve) 

#create a model 

SIR_odes <- function (t,x,params){ 
  S <- x[1] 
  I <- x[2]
  R <- x[3] 
  beta <- params[1] 
  gamma <- params[2] 
  dS <- -beta*S*I 
  dI <- beta*S*I - gamma*I 
  dR <- gamma*I 
  list(c(dS,dI,dR))
  
  
  }

#parameters 
gamma <- 1/55
beta <- 0.0016

#times 

t<- seq(0,1000,by= 1) 

results <- as.data.frame(ode(y = c(S=100, I=1, R=0), 
                             times = t, func = SIR_odes, parms = c(beta,gamma))) 

plot(x =results$time, y=results$I, type = 'l', col = 'red', ylim = c(0,100), main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$S , type = 'l', col= 'green')
lines(x = results$R,  type = 'l', col = 'blue') 
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("green","red", "blue"),
        lty=c(1,1,1)) 









