library(stats4)
library(quantmod)
library(sde)  ## Only for Simulation

rm(list = ls(all = TRUE))
graphics.off()

estimateCIR_OLS <- function(y, dt = 1/256, correction = 0.004){
  
  value <- y + correction
  diff <- diff(value, 1)
  sq_r <- sqrt(value)[-length(value)]
  
  # calculate variables for CIR OLS estimation
  y <- diff/sq_r
  x1 <- dt/sq_r
  x2 <- sq_r*dt
  
  #estimate OLS without intercept
  OLS <- lm(y~x1+x2 - 1)

  sigma <- summary(OLS)$sigma                       # diffusion estimate
  a <- OLS$coefficients[2]*-1                       # convergence speed estimate
  b <- OLS$coefficients[1]/OLS$coefficients[2]*-1   # long time mean
  
  return(c(LTmean_b = b, convSpeed_a = a, sigma = sigma))
}

estimateCIR_ML <- function(y, start_a, start_b, start_sigma, dt = 1/256, seed = 42, correction = 0.004){
  
  set.seed(seed) # set inital seed
  y <- y + correction # correct ts 
  
  # define CIR function
  CIR <- function(theta1, theta2, theta3) {
    n=length(y)
    dt=1/256
    return(-sum(dcCIR(x=y[2:n], Dt=dt, x0=y[1:(n-1)], theta=c(theta1, theta2, theta3), log=T)))
  }
  
  #estimate ML based on start values (here OLS)
  fit <- mle(CIR, start=list(theta1=as.numeric(start_a * start_b), 
                             theta2=as.numeric(-start_a),
                             theta3=as.numeric(start_sigma)),
             method='L-BFGS-B',
             lower=c(0.0005,0.01,0.01),
             upper=c(1,Inf,1))
  
  result <- c(LTmean_b = coef(fit)[1],  convSpeed_a = coef(fit)[2], sigma = coef(fit)[3])
  return(result)
}


#################
### execution ###
#################

# read data points
y <- read.csv(file = "eonia_clean.csv")
#y <- read.csv(file = "3Meuribor_clean.csv")

# time horizon
start <- match("1999-01-04", y$Date)
end <- match("2012-12-31", y$Date)
y <- y[start:end, 2]

OLSestimates <- estimateCIR_OLS(y, correction = 0)
MLestimates <- estimateCIR_ML(y, 
                               start_a = OLSestimates[2], 
                               start_b = OLSestimates[1], 
                               start_sigma = OLSestimates[3], 
                               correction = 0)


##################
### simulation ###
##################

set.seed(42)
sde.sim(X0=y[1], theta=c(MLestimates[1], MLestimates[2], MLestimates[3]), model="CIR", N=256) -> X1
plot(X1, main="Cox-Ingersoll-Ross")
