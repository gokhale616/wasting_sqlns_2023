source('./setup.R')

wasting_model <- function (t, x, params) {
  with(
    as.list(c(x,params)),
    {
      
      # Seasonality in wasting rates 
      beta = beta0*(1+beta1*cos(2*pi*t))
      
      # compartments 
      dB1 = nu*N - (delta + mu)*B1
      dB2 = delta*B1 + gamma*M - (beta + mu)*B2
      dM = beta*B2 + phi*S - (gamma + sigma + (1+alpha)*mu)*M
      dS = sigma*M - (phi + (1+iota)*mu)*S
      
      dM_inc = beta*B2 
      dS_inc = sigma*M   
      
      dM_die = (1+alpha)*M
      dS_die = (1+iota)*S
      
      dx <- c(dB1,dB2,dM, dS, dM_inc, dS_inc, dM_die, dS_die)
      list(dx) 
    }
  )
}


N_val = 1

# setting up the equations 
times <- seq(0,3000,by=1/12) #times at which to obtain solution
params <- c(N = N_val, mu=1/50, nu = 1/50, delta=12/6, beta0 = 52/4, 
            beta1 = 0.001, sigma = 52/3, gamma = 52/2, phi = 52/2, 
            alpha = 0.5, iota = 1
            ) #parameters


init <- N_val*c(B1=1e-2,B2=N_val*8e-3,M=1e-5, S=1e-6, 
                M_inc = 0, S_inc = 0, M_die=0, S_die=0)

result <- (
  lsoda(y = init, times = times, 
        func = wasting_model, 
        parms = params,
        rtol=1e-12,hmax=1/120) %>%    
    as_tibble()
)


data_to_plot <- (
  result %>%    
    filter(time >= 2997) %>% 
    mutate(
      time = time-2997,
      M_inc = diff(as.zoo(M_inc), lag =1, na.pad=TRUE),
      S_inc = diff(as.zoo(S_inc), lag =1, na.pad=TRUE), 
      M_die = diff(as.zoo(M_die), lag =1, na.pad=TRUE),
      S_die = diff(as.zoo(S_die), lag =1, na.pad=TRUE), 
      ) %>%  
    mutate_all(list(as.numeric)) %>% 
    #replace_na(replace = list(M_inc=0, S_inc = 0, M_die = 0, S_die = 0)) %>% 
    mutate_at(1:ncol(.), as.numeric))


data_to_plot <- (
  data_to_plot %>% 
    mutate(
      M_mort = map_dbl(1:nrow(data_to_plot), 
            function(counter) {
              if (counter < nrow(data_to_plot)) {
                result <- data_to_plot$M_die[(counter+1)]/data_to_plot$M[counter]     
                } else {
                  result <- NA  
                  }
            result
            }), 
      S_mort = map_dbl(1:nrow(data_to_plot), 
                  function(counter) {
                    if (counter < nrow(data_to_plot)) {
                      result <- data_to_plot$S_die[(counter+1)]/data_to_plot$S[counter]     
                    } else {
                      result <- NA  
                    }
                    result
                  })
      ) %>% 
      pivot_longer(!time, names_to = "state", values_to = "proportion") 
    
)  
          
        


data_to_plot %>% 
  ggplot(aes(x = time, y = proportion)) +
  geom_line(size = 0.8) +
  facet_wrap(.~state, scales = "free_y")+
  project_theme+
  cap_axes()


