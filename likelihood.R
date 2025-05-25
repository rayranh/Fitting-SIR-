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
R0 <- 1.5 
gamma <- 1/5 
beta <- R0*gamma/1000

#times 

t<- seq(0,364,by= 1) 

results <- as.data.frame(ode(y = c(S=999, I=1, R=0), 
                             times = t, func = SIR_odes, parms = c(beta,gamma))) 

plot(x =results$time, y=results$I, type = 'l', col = 'red', ylim = c(0,1000), main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = results$S , type = 'l', col= 'green')
lines(x = results$R,  type = 'l', col = 'blue') 
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("green","red", "blue"),
       lty=c(1,1,1)) 


#######################################
##creating data and compare to model ##

S0 <- 999
I0 <- 1 
R0 <- 0 

repro_number <- 1.5 
beta_2 <- 3e-04
gamma <- 1/5 

#times 
days <- seq(1,364, by=1) 

#starting conditions for pop size/per capita 
results <- as.data.frame(ode(y=c(S=S0,I=I0,R=R0), times = days, func= SIR_odes, parms = c(beta_2,gamma) ))

#real data is discrete so generating 'real data' 

#this is subtracting across the numbers 
incidence <- c(0,-diff(results$S)) #from model 

#weekly incidence 
weekly_incidence <- colSums(matrix(incidence, nrow=7)) #from model 

#turning into real data by using rpois, randomly choosing numbers on poisson 
real_data <- data.frame(times=seq(1,364,by=7), inc=rpois(length(weekly_incidence), weekly_incidence)) 

plot(real_data, type= 'l', col = 'red', xlab = 'times(days)', ylab="number infected people", main = "observed weekly incidents")  

lines(weekly_incidence, type = 'l', col = 'green') 
legend("topright", title="key", legend=c("Real Data", "Model"), 
       col = c("red","green"),
       lty=c(1,1)) 


################
## likelihood ## 

likelihood_fun <- function(pars){ 
  beta_2 <- pars[1] 
  gamma <- pars[2] 
  results <- results <- as.data.frame(ode(y=c(S=S0,I=I0,R=R0), 
                                          times = days, func= SIR_odes,
                                          parms = c(beta_2,gamma) ))
  incidence <- c(0,-diff(results$S)) 
  weekly_incidence <- colSums(matrix(incidence, nrow=7)) 
  
  lik <- sum(dpois(real_data$inc, weekly_incidence, log = TRUE)) 
  
  lik
  
}

real_pars <- c(beta_2,gamma)

#plugging in real pars 
print(likelihood_fun(real_pars))




