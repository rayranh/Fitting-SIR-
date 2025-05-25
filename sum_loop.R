library(deSolve) 
library(dplyr)
library(ggplot2) 

data <- read.csv("SIR_Output_With_Noise.csv") 
beta_parms <- seq(from = 0.01, to = 1, by= 0.01)

#creating an empty list for the answers 
result <- vector(length(beta_parms),mode="list")   
initial_values <- c( 
  S = 1, 
  I = 0.001, 
  R = 0 
)  
time_values <- seq(0,100)

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
  result[[k]] <- ode(y=initial_values,times=time_values,func=sir_equations,
                     parms=c(beta=beta_parms[k], gamma=0.11)) 
  result_df <- as.data.frame(result[[k]])
  
  sum_of_er <- rowSums((result_df[[k]]-data$S)^2)
}




#naming the results after the beta parameters 
names(result) <- beta_parms 

#binding rows, l apply applies fxn to list and returns a list 
dd <- dplyr::bind_rows(lapply(result,as.data.frame),.id="beta")

#matching the number in given data set 
sequence_time <- seq(from = 5, to= 150, by = 5)

#subsetting the model's dataset to include only the first 2 rows and columns 
model_sum <- dd[dd$time %in% sequence_time, c(1,3) ]
#renaming the S column 
names(model_sum)[names(model_sum) == 'S'] <- 'S1'

#combining the actual data and model output together 
combined_df <- cbind(data, model_sum)  

#sum of errors (data - model)
sum_of_error <- function(combined_df) { 
  result <- (combined_df$S-combined_df$S1)^2
  return(result)
} 
#sum of error fxn applied to combined dataframe 
error <- sum_of_error(combined_df)  

#binding data and error = dataframe with both S and Error values 
data_error<- cbind(combined_df, error)

#subsetting data to error and grouping by beta values and performing sum function 
answer <- aggregate(data_error[,c(5)], by=list(data_error$beta), FUN= "sum")

print(beta_answer<- min(answer[,"x"]))

print(x_val_associated <- answer[answer$x == beta_answer, "Group.1"])

with(answer, {
  plot(x=Group.1, y=x, col="black", type="b", ylim= c(0,15),  xlab="beta value",
       ylab="sum of error", main="Best Beta")          #Plot the data for S over time
  points(x=data$Time, y=data$S)
})

