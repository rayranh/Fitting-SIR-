library(deSolve) 
library(dplyr)
library(ggplot2) 

#SSE sum((observation - mean))^2 

data <- read.csv("Fitting-SIR-/SIR_Output_With_Noise (1).csv")  
colnames(data) <- c("time", "S")
beta_parms <- seq(from = 0.01, to = 1, by= 0.01)

#creating an empty list for the answers 
result_df <- data.frame()
initial_values <- c( 
  S = 1, 
  I = 0.001, 
  R = 0 
)  
time_values <- seq(0,150,by = 5)


sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    S <- -beta*S*I 
    I <- beta*S*I-gamma*I
    R <- gamma*I 
    return(list(c(S, I, R)))
  })
}



#for loop to run different beta_values 
for (k in seq_along(beta_parms)){ #range of values for beta
  result_beta <- ode(y=initial_values,times=time_values,func=sir_equations,
                     parms=c(beta =beta_parms[k], gamma=0.11))  
  
  beta_df <- as.data.frame(result_beta)
  
  beta_df$beta <- beta_parms[k] 
  
  result_df <- rbind(result_df, beta_df) #Have to append results to store it all!  
}

obs_expanded <- tidyr::crossing(beta = unique(result_df$beta), data) #assign beta to data?  
obs_data <- data %>% rename(S_real = S)


resultWithObs <- result_df %>% 
  left_join(obs_expanded, by=c("beta", "time"))

#calculating SSE 
 SSE_beta <- resultWithObs %>% 
   mutate(SSE = (S.x- S.y)^2) %>% 
   group_by(beta) %>% 
   summarise(SumCol = sum(SSE, na.rm = TRUE))



