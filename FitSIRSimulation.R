library(deSolve) 
library(dplyr) 
rm(list = ls())


#Initial state parameters 
initial_state <- c(S=999/1000, I=1/1000, R=0) 


#parameters answer: 0.14669257 0.05299332
params<- c(0.8,0.05) 
t<- seq(0,150,by= 1) 
#times 
timeValues <- seq(5,150,by=5)

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

#running model 
df_Model <- as.data.frame(ode(y = initial_state, 
                              times = t, func = SIR_odes, parms = params))

sample_size <- 10 


MximumLikelihood <- function(parms){  
  beta <- params[1] 
  gamma <- params[2] 
  results <- as.data.frame(ode(y = initial_state, 
                               times = t, func = SIR_odes, parms =c(beta = beta, gamma = gamma)))  
  set.seed(123)
  MaxLike <- results %>%
    filter(time %in% timeValues) %>% 
    mutate(sample_size = sample_size, InfectedProb = I/(S+I+R), 
           NumInf= rbinom(n(),10,InfectedProb),fracInf = NumInf/sample_size , 
           loglike = log(dbinom(NumInf, sample_size, InfectedProb)))
   
  return(MaxLike) 
  
} 

#using function to create df for simulated data plot 
dfMax <- as.data.frame(MximumLikelihood(params))

#creating another fxn for optim  
optimHood <- function(parms){ 
  observed <- MximumLikelihood(parms) 
  return(-sum(observed$loglike))}

optimHood(params)

print(optim(par = params, fn = optimHood, method = "Nelder-Mead"))


plot(x =dfMax$time, y= dfMax$fracInf, type = 'p', col = 'red', ylim = c(0,1), main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = df_Model$S , type = 'l', col= 'black') 
lines(x = df_Model$I, type = 'l', col= 'green')
lines(x = df_Model$R,  type = 'l', col = 'blue')  
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("black","green", "blue"), cex = 0.5,
       lty=c(1,1,1)) 




