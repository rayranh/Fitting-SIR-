library(deSolve) 
library(dplyr) 
rm(list = ls())

set.seed(123)


#Initial state parameters 
initial_state <- c(S=999/1000, I=1/1000, R=0) 


#parameters answer: 0.14669257 0.05299332
params<- c(0.14,0.05) 
t<- seq(0,150,by= 1) 
#times 
timeValues <- seq(5,150,by=5)


SIR_odes <- function (t,x,parms){ 
  S <- x[1] 
  I <- x[2]
  R <- x[3]  
  beta <- parms[1] 
  gamma <- parms[2] 
  dS <- -beta*S*I 
  dI <- beta*S*I - gamma*I 
  dR <- gamma*I 
  list(c(dS,dI,dR))
} 

odeFunc <- function(parms){
  #running model 
  df_Model <- as.data.frame(ode(y = initial_state, 
                                times = t, func = SIR_odes, parms = params))
  return(df_Model)
} 

df <- odeFunc(params)

#sample size of people that were looked at 
sample_size <- 10 


SimDataFunc <- function(parms){  
  beta <- parms[1] 
  gamma <- parms[2] 
  results <- as.data.frame(ode(y = initial_state, 
                               times = t, func = SIR_odes, parms =c(beta = beta, gamma = gamma)))  
  MaxLike <- results %>%
    filter(time %in% timeValues) %>% 
    mutate(sample_size = sample_size, InfectedProb = I/(S+I+R), 
           NumInf= rbinom(n(),10,InfectedProb),fracInf = NumInf/sample_size)
  
  return(MaxLike) 
  
}  

SimData <- SimDataFunc(params)


#loglhood fn 
logLhoodfunc <- function(parms){
  results <- as.data.frame(ode(y = initial_state, times = t, func = SIR_odes, parms = parms))
  fit_data <- results %>%
    filter(time %in% timeValues) %>%
    mutate(InfectedProb = I / (S + I + R))
  
  # Merge with observed SimData
  merged <- SimData %>%
    mutate(time = as.numeric(time)) %>%
    left_join(fit_data, by = "time") %>%
    mutate(loglike = dbinom(NumInf, sample_size, InfectedProb.y, log = TRUE))
  
  return(-sum(merged$loglike))
}
logLhoodfunc(params)

#creating another fxn for optim  

print(optim(par = params, fn = logLhoodfunc, method = "Nelder-Mead"))


plot(x =SimData$time, y= SimData$fracInf, type = 'p', col = 'red', ylim = c(0,1), main = 'sir', xlab = 'time', ylab = 'number of infected people')
lines(x = df$S , type = 'l', col= 'black') 
lines(x = df$I, type = 'l', col= 'green')
lines(x = df$R,  type = 'l', col = 'blue')  
legend("topright", title="key", legend=c("susceptible", "infected", "recovered"), 
       col = c("black","green", "blue"), cex = 0.5,
       lty=c(1,1,1)) 




