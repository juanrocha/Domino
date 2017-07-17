### Cascading effects models
### Juan Carlos Rocha
### juan.rocha@princeton.edu
### February 13 2017


## This script shows a short demostration of modeling a bifurcation with ODE
## The idea is to model simple toy models of RS and then couple them
## The code should be minimalistic and emphasize on the workflow and replicability

library(deSolve)
library(ggplot2)
library(dplyr)
library(tidyr)


########
# model of logistic growth [following Sodeart 2012]
########

# set parameters
r <- 1
K <- 10
yini <- 12

# define the model
derivs <- function (t, y, params){
    list(r * y * (1-y/K))
}

# set run time
times <- seq(from = 0, to = 20, by = 0.2)

# integrate the model
out <- ode(y = yini, times = times, func = derivs, parms = NULL)
out2 <- ode(y = yini, times = times, func = derivs, parms = NULL) # run this when yini <- 12 instead of 2


########
# Lorenz
########

# set parameters
a <- -8/3
b <- -10
c <- 28

# Initial conditions
yini <- c(X = 1, Y = 1, Z = 1)

# Model NOTE: state variables needs to be on exactly the same order as yini.
Lorenz <- function(t, y, parms){
    with(as.list(y), {
        dX <- a * X + Y * Z
        dY <- b * (Y - Z)
        dZ <- -X * Y + c * Y - Z
        list(c(dX, dY, dZ)) })
}

# set time
times <- seq(from = 0, to = 100, by=0.01)
out <- ode(y = yini, times = times, func = Lorenz, parms = NULL)

plot(out, lwd = 2)
plot(out[,'X'], out[,'Y'], type = 'l', xlab = "X", ylab = "Y", main= 'butterfly')


####################
#


