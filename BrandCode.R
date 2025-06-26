
# rm(list = ls())
library(tidyr) 
library(deSolve) 
library(ggplot2)


####################
## Template Model ##
##    Function    ##
####################

baGAWK <- function (t, initial_state, parms) {
  val = with(as.list(c(initial_state, parms)), {
    # need a vector for derivative value object names
    ## this allows you to use lapply() and the get() function to return all derivatives as a list.
    ### The order of derivatives do have to match up the initial state vector, but this
    #### formulation eliminates that issue
    deriv_names <- c(paste0('d', names(initial_state)))
    
    #######################
    ## define ode system ##
    #######################
    
    # dA = a*A - b*A
    
    dB_cells <- -M*B_cells - beta*Cb*B_cells - beta_2*Ct*B_cells + (g1*(Cb+Ct)/(g2+(Cb+Ct))) 
    dCb <- M*B_cells + beta*Cb*B_cells + beta_2*Ct*B_cells - alpha*Cb
    dT_cells<- -nu_A*Cb*T_cells - nu_b*Ct*T_cells + (h1*(Cb+Ct)/(h2+(Cb+Ct)))
    dAt <- nu_A*Cb*T_cells + nu_b*Ct*T_cells - beta_2*Ct*At - beta*Cb*At
    dLt <- theta*(beta_2*Ct*At + beta*Cb*At) - mu*Lt
    dCt <- (1-theta)*(beta_2*Ct*At + beta*Cb*At) - alpha_2*Ct
    dZ <- mu*Lt
    df <- -nu_F*Lt*f
    dIf <- nu_F*Lt*f  
    
    ## calculate subclass differential equations
    ### First equation must be defined outside the loop because
    #### it depends on A and subclasses don't
    ##### generally, the first equation cant be iteratively defined
    dIf_1 = nu_F*Lt*f - ktr*N*If_1  
    If_tot = If_1 # add first subclass pop. value to total count
    if(N>1){
      for(i in 2:N){
        assign(x = paste0('dIf_',i), value = eval(parse(text = paste0( 'ktr*N*If_', i-1, # entering from previous subclass 
                                                                      ' - ktr*N*If_', i # leaving current subclass
        ))))
        If_tot = If_tot + get(as.character(paste0('If_', i))) ## add current subclass pop. value to total
      }
    }
    
    ## dynamic definition for C derivative
    # assign(x = 'dIf', value = eval(parse(text = paste0( 'ktr*N*If_', N)))) # entering from last subclass 

    
    # Delayed conversion from f to If
    df  <- -ktr * get(paste0('If_', N)) * f
    dIf <-  ktr * get(paste0('If_', N)) * f
    
    #######################   
    
    ## use lapply function to return all derivative equation values as a list
    lapply(X = deriv_names, FUN = get, envir=sys.frame(sys.parent(0)))
    
  })# end with()
  return(list(val))
}# end function

##tooth##################

## initialize parameters and initial states
parms <- c( 
  M = 0.1
  , beta =4.819e-4              #contact rate with B cells 
  , beta_2 =5e-4                #contact rate with T cells #0.5 
  , nu_A = 0.005               #Activation rate with T cells CD4+  
  , nu_b = 0.01                #Activation rate with B cells 
  , nu_F = 0.5                 #Infection rate of follicular cells 
  , mu = 0.1                 #Rate of Tumor Ckells 
  , alpha = 0.015              #death rate of B cells 
  , alpha_2 = 0.010            #death rate of T cells 
  , theta = 0.8                #population of activated T cells 
  , g1 = 0.05                  #incoming B cells 
  , g2 =0.001    
  , h1 = 0                     #incoming T cells 
  , h2 = 10 
  , N = 10 
  , ktr = 11 / 300
)

initial_state <- c( 
  B_cells = 50  
  , Cb = 0 
  , T_cells =50
  , At = 0
  , Lt = 0
  , Ct = 0
  , Z = 0
  , f = 30 
  , If =0
  , If_1 = 0 
) 

If_new = replicate(n = parms['N']-1, expr = 0)
if(parms['N']>1){ names(If_new) = paste0('If_', c(2:parms['N'])) }
initial_state = c(initial_state, If_new)

time <- as.numeric(seq(from = 0, to = 100, by = 0.01) )

#################
# Run the model #
#################

tic = Sys.time()
ode(
  func = baGAWK,
  y = initial_state,
  times = time,
  parms = parms,
  method = "rk4"
) %>% as.data.frame() -> out
toc = Sys.time()
print(toc-tic)
#################

## re-aggregate subclasses
out_merge <- data.frame(time = out$time, 
                        B_cells = out$B_cells, 
                        If = rowSums(out[,grep('If_',names(out))]), 
                        Cb = out$Cb,
                        T_cells = out$T_cells,
                        At = out$At,
                        Lt = out$Lt, 
                        Ct = out$Ct, 
                        Z = out$Z, 
                        f = out$f)

## convert data to long format for plotting
data <- out_merge %>% gather(variable, value, -time)


ggplot() +
  geom_line(data = data, aes(x = time, y = value, color = variable))


