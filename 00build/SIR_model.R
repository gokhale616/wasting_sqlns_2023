library(dplyr)
library(tidyr)
library(magrittr)
library(deSolve)
library(zoo)
library(ggplot2)

# differential equation system
seasonal.sir.model <- function (t, x, params) {
  with(
    as.list(c(x,params)),
    {
      
      # Seasonality in transmission 
      
      beta <- R0*gamma*(1+beta1*cos(2*pi*t))
      
      rho <- pgamma(t, rate = 0.25, shape = k)
      
      # compartments 
      dS <- mu*(1-S)-beta*S*I
      dI <- beta*S*I-(mu+gamma)*I
      dR <- gamma*I-mu*R
      
      dC <- gamma*I*rho
      
      dx <- c(dS,dI,dR, dC)
    list(dx) 
    }
  )
}


# setting up the equations 
times <- seq(0,100,by=1/52) #times at which to obtain solution
params <- c(mu=1/50,R0 = 9,beta1=0.1,gamma=365/13,k=1) #parameters
init <- c(S=0.06,I=0.001,R=0.939, C=0)

result <- (
  lsoda(y = init, times = times, 
        func = seasonal.sir.model, 
        parms = params,
        rtol=1e-12,hmax=1/120) %>%    
  as_tibble()
)


data_to_plot <- (
  result %>%    
    mutate(
      N = (S+I+R) %>% round(),
      C = diff(as.zoo(C), lag =1, na.pad=TRUE)) %>%  
    mutate_all(list(as.numeric)) %>% 
    replace_na(replace = list(C=0)) %>% 
    mutate_at(1:ncol(.), as.numeric) %>%   
    pivot_longer(!time, names_to = "state", values_to = "proportion")
)


data_to_plot %>% 
  ggplot(aes(x = time, y = proportion, colour = state)) +
  geom_line(size = 0.8) +
  facet_wrap(.~state, scales = "free_y")
  




# Now define the parameters of your gamma distribution
shape = 15
rate = 0.3
# Now calculate points on the cdf
cdf = pgamma(x, shape, rate)
# Shown plotted here
plot(x,cdf, type = "l")

gamma_data <- (
  expand.grid(
    x = seq(0, 100, by = 0.5),
    shape = seq(0, 40, by = 0.05),
    rate = 0.25
    ) %>% 
  tibble()
  )


gamma_data <- (
  gamma_data %>% 
    mutate(gamma_cdf = pgamma(x, shape, rate))
) 

python_colour_pallate <- c(
  '#1f77b4', '#ff7f0e', '#2ca02c', 
  '#d62728', '#9467bd', '#8c564b', '#e377c2',
  '#7f7f7f', '#bcbd22', '#17becf'
  )

# gamma_data %>% 
#   filter(shape %in% c(0.25, 0.5, 0.75, 1, 5, 10, 15, 20)) %>% 
#   ggplot(aes(x = x, y = gamma_cdf, colour = rate, group = rate))+
#   geom_line(size = 0.8)+
#   facet_wrap(.~shape, scales = "free_y")+
#   scale_color_gradient(low = '#1f77b4', high = '#7f7f7f')+
#   theme_bw()


gamma_data %>% 
  filter(rate %in% c(0.25, 0.5, 0.75, 1, 5, 10, 15, 20)) %>% 
  ggplot(aes(x = x, y = gamma_cdf, colour = shape, group = shape))+
  geom_line()+
  facet_wrap(.~rate, scales = "free_y")+
  scale_color_gradient(high = '#7f7f7f', low = '#ff7f0e')+
  theme_bw()




# emax function how flxible is it

saturating_function <- function(t, x, max, t_half) {
  
  (max*t^x)/(t_half^x+t^x)
  
}



saturating_fn_data <- 
  tibble(
    t = times
  ) %>%  
  mutate(
    effect = saturating_function(t = t, x = 20,  max = 1, t_half=50)
  )

saturating_fn_data %>% 
  ggplot(aes(x = t, y = effect))+
  geom_line()












  