library(ggplot2)

si <- function(beta, N, S0, I0, tf) { 
  time = 0 
  S = S0 
  I = I0 
  
  #empty vectors 
  ta = numeric(0)  
  Sa = numeric(0) 
  Ia = numeric(0) 
  
  #while time is less than final time, update ta with new time 
  while (time < tf) { 
    ta = c(ta, time) 
    Sa = c(Sa, S) 
    Ia = c(Ia, I) 
    
    w = beta * S * I
    if (w == 0) break
    
    #selecting random time, updating time 
    dt = rexp(1, rate = w) 
    time = time + dt 
    
    if (time > tf) break
    
    ru = runif(1)
    if (ru < 1) {  # there's only one event type: infection
      S = S - 1
      I = I + 1
    }
  }
  
  return(data.frame(time = ta, S = Sa, I = Ia))
}  

si_out = si(0.1/1000,1000,999,1,200)
si_out_long = melt(si_out, "time") 
ggplot(si_out_long, aes(x = time, y=value, colour = variable, group = variable)) + 
  geom_line(lwd=0.5) + 
  xlab("time") + ylab("Number")








