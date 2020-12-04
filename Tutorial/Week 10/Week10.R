rm(list=ls()) # Clear the Environment / History


# Write a function that computes the ACF for an MA(2) process
# Input: theta = (theta.1, theta.2) 
# Output: ACF

MA2.ACF = function(theta, max.lag = 12){
  
  theta.1 = theta[1]
  theta.2 = theta[2]
  
  ACF = sapply(0:max.lag, function(k){
      if (k == 0) val = 1
      if (k == 1) val = (-theta.1 + theta.1 * theta.2) / (1 + theta.1^2 + theta.2^2)
      if (k == 2) val = -theta.2 / (1 + theta.1^2 + theta.2^2)
      if (k > 2)  val = 0
      return(val)
  })

  return(ACF)
}


# Write a function that computes the ACF for an AR(1) process
# Input: phi 
# Output: ACF

AR1.ACF = function(phi, max.lag = 12){
  k = 0:max.lag
  ACF = phi^k
  return(ACF)
}


# Write a function that computes the ACF for an AR(1) process
# Input: phi = (phi.1, phi.2)
# Output: ACF

AR2.ACF = function(phi, max.lag = 12){
  
  phi.1 = phi[1]
  phi.2 = phi[2]
  
  ACF = rep(NA, max.lag)
  
  for (k in 1:max.lag){
    if (k == 1) ACF[k] = phi.1 / (1-phi.2)
    if (k == 2) ACF[k] = (phi.2*(1-phi.2) + phi.1^2) / (1-phi.2)
    if (k > 2)  ACF[k] = phi.1 * ACF[k-1] + phi.2 * ACF[k-2]
  }
  
  ACF = c(1,ACF)
  
  return(ACF)
}

# -----------------------------------------------------------------------------------
# Based on the specifications of the parameters in the question, let's run
# our functions!
# -----------------------------------------------------------------------------------

# ---- part (a)
PartA.AR2 = AR2.ACF(phi = c(1.6, -0.8), max.lag = 12)

# ---- part (b)
PartB.MA2 = MA2.ACF(theta = c(-0.7, -0.99), max.lag = 12)

# ---- part (c)
PartC.AR2 = AR2.ACF(phi = c(0.5, 0.3), max.lag = 12)

# ---- part (d)
PartD.AR1 = AR1.ACF(phi = -0.8, max.lag = 12)



# -----------------------------------------------------------------------------------
# Now plot the ACFs and match them to the pictures from the assignment!
# -----------------------------------------------------------------------------------

# ---- part (a)

plot(x = 0:12,
     y = PartA.AR2,
     main = "ACF for an AR(2) process \n Part A",
     ylab = "ACF",
     xlab = "Lag",
     ylim = c(-1,1),
     xlim = c(1,12),
     type = "h")
points(x = 0:12,
       y = PartA.AR2,
       pch = 16)
abline(h = 0)

# ---------------------------------
#         Answer: (2)
# ---------------------------------
  
# ---- part (b)

plot(x = 0:12,
     y = PartB.MA2,
     main = "ACF for an MA(2) process \n Part B",
     ylab = "ACF",
     xlab = "Lag",
     ylim = c(-1,1),
     xlim = c(1,12),
     type = "h")
points(x = 0:12,
       y = PartB.MA2,
       pch = 16)
abline(h = 0)

# ---------------------------------
#         Answer: (3)
# ---------------------------------


# ---- part (c)

plot(x = 0:12,
     y = PartC.AR2,
     main = "ACF for an AR(2) process \n Part C",
     ylab = "ACF",
     xlab = "Lag",
     ylim = c(-1,1),
     xlim = c(1,12),
     type = "h")
points(x = 0:12,
       y = PartC.AR2,
       pch = 16)
abline(h = 0)

# ---------------------------------
#         Answer: (1)
# ---------------------------------

# ---- part (d)

plot(x = 0:12,
     y = PartD.AR1,
     main = "ACF for an AR(1) process \n Part D",
     ylab = "ACF",
     xlab = "Lag",
     ylim = c(-1,1),
     xlim = c(1,12),
     type = "h")
points(x = 0:12,
       y = PartD.AR1,
       pch = 16)
abline(h = 0)

# ---------------------------------
#         Answer: (4)
# ---------------------------------



