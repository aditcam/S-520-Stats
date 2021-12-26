# ******* HW 8 SOLUTION *******
# Submitted by Aditya Shekhar Camarushy 
# IU ID : adcama@iu.edu
# University ID : 2000772747

# Set the seed using university ID 
set.seed(2000772747)

# **************************************************************************************

# Q1A

# 1) Write a statistic function for the rate of an exponential distribution that can be used by the boot function.

stat_rate_fun = function(x,index=c(1:length(x))){ # x is a data vector
  # index is an integer vector of indices to access
  # default is c(1,2,...,length(x)) and for it x_loc=x
  x_loc = x[index] # access elements corresponding to index
  return(length(x_loc)/sum(x_loc)) # return statistic for local x 
  }
  
# 2) Generate 10000 samples of size 87 from a population following an exponential distribution with rate = 0.5 and compute the rate estimate for each sample.

# set parameters of the simulation 
N_samples = 10000 # number of samples 
n = 87 # sample size
# distribution parameters
rate = 0.5
sample_mat = matrix(data = rexp(n*N_samples,rate),nrow = n, ncol = N_samples)
sample_rate_est = apply(sample_mat,2,stat_rate_fun)

# 3) Generate a fixed sample of size 87 from the population.

fixed_sample = rexp(n,rate)

# 4) Generate 10000 bootstrap replicates from the fixed sample and compute the rate estimate for each bootstrap replicate.

#install.packages('boot')
#library(boot)

N_replicates = 10000
boot_out_rate_est = boot(fixed_sample,stat_rate_fun,R = N_replicates)

# 5) Plot the densities of the sample rate estimates (minus the population rate) and the bootstrap replicate rate estimates (minus the fixed sample rate estimate) on the same plot.

den_sample_rate_est = density(sample_rate_est-rate)
den_boot_out_rate_est = density(boot_out_rate_est$t - boot_out_rate_est$t0)

ylim = c(0,max(c(den_sample_rate_est$y,den_boot_out_rate_est$y)))
xlim = c(min(c(den_sample_rate_est$x,den_boot_out_rate_est$x)),
         max(c(den_sample_rate_est$x,den_boot_out_rate_est$x)))
plot(den_sample_rate_est,ylim=ylim,xlim=xlim,
     main="Density of Rate Estimates (Centered)",
     ylab="Density",xlab="x",type="l",lwd=2)
lines(den_boot_out_rate_est,col="red",lty=2,lwd=2)
legend("topright",c(expression('Sampling-'~theta[P]^'*'),expression('Bootstrap-'~hat(theta)[S])),
       col=c("black","red"),lty=c(1,2),bty="n",lwd=2)

# 6) Do the recentered densities correspond well to each other? Are there any major discrepencies that between the two densities?

# The recentered densities correspond well to each other based on the graph we have obtained and there are are no major discrepancies. This also means that the bootstrap estimates are a good representation of the distribution of the estimate from repeated population sampling.

# ------------------------------------------------------------------------------------------

# Q1B
set.seed(2000772747)
# 1) Write a statistic function for the rate of an Poisson distribution that can be used by the boot function.

stat_rate_fun = function(x,index=c(1:length(x))){ # x is a data vector
  # index is an integer vector of indices to access
  # default is c(1,2,...,length(x)) and for it x_loc=x
  x_loc = x[index] # access elements corresponding to index
  return(sum(x_loc)/length(x_loc)) # return statistic for local x 
}

# 2) Generate 10000 samples of size 104 from population following a Poisson distribution with rate λ = 10 and compute the rate estimate for each sample.

# set parameters of the simulation 
N_samples = 10000 # number of samples 
n = 104 # sample size
# distribution parameters
rate = 10
sample_mat = matrix(data = rpois(n*N_samples,rate),nrow = n, ncol = N_samples)
sample_rate_est = apply(sample_mat,2,stat_rate_fun)

# 3) Generate a fixed sample of size 104 from the population.

fixed_sample = rpois(n,rate)

# 4) Generate 10000 bootstrap replicates from the fixed sample and compute the rate estimate for each bootstrap replicate.

install.packages('boot')
library(boot)

N_replicates = 10000
boot_out_rate_est = boot(fixed_sample,stat_rate_fun,R = N_replicates)

# 5) Plot the densities of the sample rate estimates (minus the population rate) and the bootstrap replicate rate estimates (minus the fixed sample rate estimate) on the same plot.

den_sample_rate_est = density(sample_rate_est-rate)
den_boot_out_rate_est = density(boot_out_rate_est$t - boot_out_rate_est$t0)

ylim = c(0,max(c(den_sample_rate_est$y,den_boot_out_rate_est$y)))
xlim = c(min(c(den_sample_rate_est$x,den_boot_out_rate_est$x)),
         max(c(den_sample_rate_est$x,den_boot_out_rate_est$x)))
plot(den_sample_rate_est,ylim=ylim,xlim=xlim,
     main="Density of Rate Estimates (Centered)",
     ylab="Density",xlab="x",type="l",lwd=2)
lines(den_boot_out_rate_est,col="red",lty=2,lwd=2)
legend("topright",c(expression('Sampling-'~theta[P]^'*'),expression('Bootstrap-'~hat(theta)[S])),
       col=c("black","red"),lty=c(1,2),bty="n",lwd=2)

# 6) Do the recentered densities correspond well to each other? Are there any major discrepencies that between the two densities?
# The recentered densities correspond very well to each other based on the graph we have obtained and there are are no major discrepancies. This also means that the bootstrap estimates are a good representation of the distribution of the estimate from repeated population sampling.

# ----------------------------------------------------------------------------------------
# Q1C
set.seed(2000772747)
# 1) Write a statistic function for the probability of success of a negative binomial distribution that can be used by the boot function.

stat_rate_fun = function(x,index=c(1:length(x))){ # x is a data vector
  # index is an integer vector of indices to access
  # default is c(1,2,...,length(x)) and for it x_loc=x
  x_loc = x[index] # access elements corresponding to index
  x_mean = sum(x_loc)/length(x_loc)
  x_mean_sq = sum((x_loc)^2)/length(x_loc)
  return(x_mean/(x_mean_sq-(x_mean^2))) # return statistic for local x 
}

# 2) Generate 10000 samples of size 72 from population following a negative binomial distribution with size r = 8 and probability of success p = 0.4 and compute the rate estimate for each sample.

# set parameters of the simulation 
N_samples = 10000 # number of samples 
n = 72 # sample size
# distribution parameters
r = 8
p = 0.4
sample_mat = matrix(data = rnbinom(n*N_samples,r,p),nrow = n, ncol = N_samples)
sample_rate_est = apply(sample_mat,2,stat_rate_fun)

# 3) Generate a fixed sample of size 72 from the population.

fixed_sample = rnbinom(n,r,p)

# 4) Generate 10000 bootstrap replicates from the fixed sample and compute the rate estimate for each bootstrap replicate.

N_replicates = 10000
boot_out_rate_est = boot(fixed_sample,stat_rate_fun,R = N_replicates)

# 5) Plot the densities of the sample rate estimates (minus the population rate) and the bootstrap replicate rate estimates (minus the fixed sample rate estimate) on the same plot.

den_sample_rate_est = density(sample_rate_est-p)
den_boot_out_rate_est = density(boot_out_rate_est$t - boot_out_rate_est$t0)

ylim = c(0,max(c(den_sample_rate_est$y,den_boot_out_rate_est$y)))
xlim = c(min(c(den_sample_rate_est$x,den_boot_out_rate_est$x)),
         max(c(den_sample_rate_est$x,den_boot_out_rate_est$x)))
plot(den_sample_rate_est,ylim=ylim,xlim=xlim,
     main="Density of Probability of Success Estimates (Centered)",
     ylab="Density",xlab="x",type="l",lwd=2)
lines(den_boot_out_rate_est,col="red",lty=2,lwd=2)
legend("topright",c(expression('Sampling-'~theta[P]^'*'),expression('Bootstrap-'~hat(theta)[S])),
       col=c("black","red"),lty=c(1,2),bty="n",lwd=2)

# 6) Do the recentered densities correspond well to each other? Are there any major discrepencies that between the two densities?
# The recentered densities correspond very well to each other based on the graph we have obtained and there are are no major discrepancies. This also means that the bootstrap estimates are a good representation of the distribution of the estimate from repeated population sampling.

# -----------------------------------------------------------------------------

# Q1D
set.seed(2000772747)
# 1) Write a statistic function for the rate of a gamma distribution that can be used by the boot function.

stat_rate_fun = function(x,index=c(1:length(x))){ # x is a data vector
  # index is an integer vector of indices to access
  # default is c(1,2,...,length(x)) and for it x_loc=x
  x_loc = x[index] # access elements corresponding to index
  x_mean = sum(x_loc)/length(x_loc)
  x_mean_sq = sum((x_loc)^2)/length(x_loc)
  return(x_mean/(x_mean_sq-(x_mean^2))) # return statistic for local x 
}

# 2) Generate 10000 samples of size 65 from population following a gamma distribution with shape α = 7.5 and rate β = 2.5 and compute the rate estimate for each sample

# set parameters of the simulation 
N_samples = 10000 # number of samples 
n = 65 # sample size
# distribution parameters
alpha = 7.5
beta = 2.5
sample_mat = matrix(data = rgamma(n*N_samples,alpha,beta),nrow = n, ncol = N_samples)
sample_rate_est = apply(sample_mat,2,stat_rate_fun)

# 3) Generate a fixed sample of size 65 from the population.

fixed_sample = rgamma(n,alpha,beta)

# 4) Generate 10000 bootstrap replicates from the fixed sample and compute the rate estimate for each bootstrap replicate.

N_replicates = 10000
boot_out_rate_est = boot(fixed_sample,stat_rate_fun,R = N_replicates)

# 5) Plot the densities of the sample rate estimates (minus the population rate) and the bootstrap replicate rate estimates (minus the fixed sample rate estimate) on the same plot.

den_sample_rate_est = density(sample_rate_est-beta)
den_boot_out_rate_est = density(boot_out_rate_est$t - boot_out_rate_est$t0)

ylim = c(0,max(c(den_sample_rate_est$y,den_boot_out_rate_est$y)))
xlim = c(min(c(den_sample_rate_est$x,den_boot_out_rate_est$x)),
         max(c(den_sample_rate_est$x,den_boot_out_rate_est$x)))
plot(den_sample_rate_est,ylim=ylim,xlim=xlim,
     main="Density of Rate Estimates (Centered)",
     ylab="Density",xlab="x",type="l",lwd=2)
lines(den_boot_out_rate_est,col="red",lty=2,lwd=2)
legend("topright",c(expression('Sampling-'~theta[P]^'*'),expression('Bootstrap-'~hat(theta)[S])),
       col=c("black","red"),lty=c(1,2),bty="n",lwd=2)

# 6) Do the recentered densities correspond well to each other? Are there any major discrepencies that between the two densities?
# The recentered densities correspond well to each other based on the graph we have obtained and there are are no major discrepancies. This also means that the bootstrap estimates are a good representation of the distribution of the estimate from repeated population sampling.


# -----------------------------------------------------------------------------

# Q1E
set.seed(2000772747)
# 1) Write a statistic function for the location of a logistic distribution that can be used by the boot function.

stat_rate_fun = function(x,index=c(1:length(x))){ # x is a data vector
  # index is an integer vector of indices to access
  # default is c(1,2,...,length(x)) and for it x_loc=x
  x_loc = x[index] # access elements corresponding to index
  return(sum(x_loc)/length(x_loc)) # return statistic for local x
}

# 2) Generate 10000 samples of size 91 from population following a logistic distribution with location μ = −2.7 and scale s = 3 and compute the location estimate for each sample.

# set parameters of the simulation 
N_samples = 10000 # number of samples 
n = 91 # sample size
# distribution parameters
u = -2.7
s = -3
sample_mat = matrix(data = rlogis(n*N_samples,u,s),nrow = n, ncol = N_samples)
sample_rate_est = apply(sample_mat,2,stat_rate_fun)

# 3) Generate a fixed sample of size 91 from the population.

fixed_sample = rlogis(n,u,s)

# 4) Generate 10000 bootstrap replicates from the fixed sample and compute the location estimate for each bootstrap replicate.

N_replicates = 10000
boot_out_rate_est = boot(fixed_sample,stat_rate_fun,R = N_replicates)

# 5) Plot the densities of the sample rate estimates (minus the population rate) and the bootstrap replicate rate estimates (minus the fixed sample rate estimate) on the same plot.

den_sample_rate_est = density(sample_rate_est-u)
den_boot_out_rate_est = density(boot_out_rate_est$t - boot_out_rate_est$t0)

ylim = c(0,max(c(den_sample_rate_est$y,den_boot_out_rate_est$y)))
xlim = c(min(c(den_sample_rate_est$x,den_boot_out_rate_est$x)),
         max(c(den_sample_rate_est$x,den_boot_out_rate_est$x)))
plot(den_sample_rate_est,ylim=ylim,xlim=xlim,
     main="Density of Rate Estimates (Centered)",
     ylab="Density",xlab="x",type="l",lwd=2)
lines(den_boot_out_rate_est,col="red",lty=2,lwd=2)
legend("topright",c(expression('Sampling-'~theta[P]^'*'),expression('Bootstrap-'~hat(theta)[S])),
       col=c("black","red"),lty=c(1,2),bty="n",lwd=2)

# 6) Do the recentered densities correspond well to each other? Are there any major discrepencies that between the two densities?
# The recentered densities correspond well to each other based on the graph we have obtained and there are are no major discrepancies. This also means that the bootstrap estimates are a good representation of the distribution of the estimate from repeated population sampling.

# ************************************************************************************

#Q2

# Q2A
set.seed(2000772747)
# 1) Generate a sample of size 200 from a Pareto distribution with lower bound x0 = 3 and shape alpha= 1.5.

rpareto = function(n,x_0,shape){
  return(exp(1/shape*runif(n))*x_0)
}

x = rpareto(200,x_0=3,shape=1.5)

# 2) Write a function to compute the shape parameter estimator that can be used with boot.

shape_param_est = function(x,index=c(1:length(x))){
  x_loc = x[index]
  x_cap = min(x_loc)
  log_sum = sum(log(x_loc/x_cap))
  return (length(x_loc)/log_sum)
}

# 3) Get 10000 bootstrap replicate estimates of the shape parameter using the Pareto sample that you drew. Use these replicate estimates and the point estimate from the sample to get the bias corrected bootstrap estimates.

N_replicates = 10000
boot_out_shape_param = boot(x,shape_param_est,R=N_replicates)
bias_corrected_boot_shape_est = 2*boot_out_shape_param$t0 - boot_out_shape_param$t

plot(density(bias_corrected_boot_shape_est))

# 4) Plot the density of the bias corrected bootstrap estimates and use the summary function on this vector. From these outputs, describe what you think is a reasonable guess at the true population value and how much uncertainty you would want to express in that guess. Plot the empirical distribution function of the bias corrected bootstrap estimates. Does the distribution look smooth to you? Why or why not?

summary(bias_corrected_boot_shape_est)
plot(ecdf(bias_corrected_boot_shape_est))

# Min.   :2.457  
# 1st Qu.:3.098  
# Median :3.198  
# Mean   :3.192  
# 3rd Qu.:3.295  
# Max.   :3.641 

# A majority of the population lies between 2.8 and 3.6, The true population is around 3.19. The distribution looks smooth as the tails tend to 0 and 1. 

# ------------------------------------------------------------------------------------

# Q2B
set.seed(2000772747)
# 1) Generate a sample of size 200 from a Rayleigh distribution with scale s = 5

rrayleigh = function(n,scale){
  return(sqrt(rchisq(n,2))*scale)
}
x = rrayleigh(200,scale=5)

# 2)Write a function to compute the skewness estimator that can be used with boot

skewness_est = function(x,index=c(1:length(x))){
  x_loc = x[index]
  x_mean = sum(x_loc)/length(x_loc)
  return((n^0.5)*(sum((x_loc-x_mean)^3)/((sum((x_loc-x_mean)^2)^1.5))))
}

# 3) Get 10000 bootstrap replicate estimates of the skewness parameter using the Rayleigh sample that you drew. Use these replicate estimates and the point estimate from the sample to get the bias corrected bootstrap estimates. 

N_replicates = 10000
boot_out_shape_param = boot(x,skewness_est,R=N_replicates)
bias_corrected_boot_shape_est = 2*boot_out_shape_param$t0 - boot_out_shape_param$t

plot(density(bias_corrected_boot_shape_est))

# 4) Plot the density of the bias corrected bootstrap estimates and use the summary function on this vector. From these outputs, describe what you think is a reasonable guess at the true population value and how much uncertainty you would want to express in that guess. Does the actual true value of the skewness lie within what you would describe as the bulk of the density you have plotted? Plot the empirical distribution function of the bias corrected bootstrap estimates. Does the distribution look smooth to you? Why or why not?

summary(bias_corrected_boot_shape_est)
plot(ecdf(bias_corrected_boot_shape_est))

# Min.   :-0.1003  
# 1st Qu.: 0.1862  
# Median : 0.2529  
# Mean   : 0.2524  
# 3rd Qu.: 0.3201  
# Max.   : 0.5828

# A reasonable guess for the True population value will be 0.2529  
# The actual true value of the skewness lies within what I would describe as the bulk of the density I have plotted. The distribution looks smooth as the distribution tends to 0 or 1. 
# ------------------------------------------------------------------------------------

# Q2C
set.seed(2000772747)
#1. Generate a sample of size 200 from a Laplace distribution with center ?? = ???10 and scale s = 4 using the function
rlaplace = function(n,center,scale){
  return(rexp(n)*scale*(2*rbinom(n,1,0.5)-1)+center)
}
x = rlaplace(200,center=-10,scale=4)

# 2)Write a function to compute the scale estimator that can be used with boot.

scale_est = function(x,index=c(1:length(x))){
  x_loc = x[index]
  u_cap = median(x_loc)
  return (1/length(x_loc)*(sum(abs(x_loc - u_cap))))
}

# 3) Get 10000 bootstrap replicate estimates of the scale parameter using the Laplace sample that you drew. Use these replicate estimates and the point estimate from the sample to get the bias corrected bootstrap estimates.

N_replicates = 10000
boot_out_shape_param = boot(x,scale_est,R=N_replicates)
bias_corrected_boot_shape_est = 2*boot_out_shape_param$t0 - boot_out_shape_param$t

plot(density(bias_corrected_boot_shape_est))

# 4) Plot the density of the bias corrected bootstrap estimates and use the summary function on this vector. From these outputs, describe what you think is a reasonable guess at the true population value and how much uncertainty you would want to express in that guess. Plot the empirical distribution function of the bias corrected bootstrap estimates. Does the distribution look smooth to you? Why or why not?

summary(bias_corrected_boot_shape_est)
plot(ecdf(bias_corrected_boot_shape_est))
# 
# Min.   :2.968  
# 1st Qu.:4.155  
# Median :4.363  
# Mean   :4.357  
# 3rd Qu.:4.564  
# Max.   :5.426  

# A majority of the population lies between 3.5 and 5.0, The true population is around 4.35. The distribution looks smooth as the tails tend to 0 and 1. 

# ------------------------------------------------------------------------------------

# Q2D
set.seed(2000772747)
#1. Generate a sample of size 200 from a Yule distribution with shape ρ = 5 
ryule = function(n,shape){
  return(rgeom(n,exp(-rexp(n,shape)))+1)
}
x = ryule(200,shape=5)

# 2)Write a function to compute the shape estimator that can be used with boot.

shape_est = function(x,index=c(1:length(x))){
  x_loc = x[index]
  x_mean = sum(x_loc)/length(x_loc)
  return (x_mean/(x_mean-1))
}

# 3) Get 10000 bootstrap replicate estimates of the scale parameter using the Laplace sample that you drew. Use these replicate estimates and the point estimate from the sample to get the bias corrected bootstrap estimates.

N_replicates = 10000
boot_out_shape_param = boot(x,shape_est,R=N_replicates)
bias_corrected_boot_shape_est = 2*boot_out_shape_param$t0 - boot_out_shape_param$t

plot(density(bias_corrected_boot_shape_est))

# 4) Plot the density of the bias corrected bootstrap estimates and use the summary function on this vector. From these outputs, describe what you think is a reasonable guess at the true population value and how much uncertainty you would want to express in that guess. Plot the empirical distribution function of the bias corrected bootstrap estimates. Does the distribution look smooth to you? Why or why not?

summary(bias_corrected_boot_shape_est)
plot(ecdf(bias_corrected_boot_shape_est))

# # A majority of the population lies between 2.0 and 5.0, The true population is around 3.8. The distribution looks smooth as the tails tend to 0 and 1. 

# ------------------------------------------------------------------------------------

# Q2E
set.seed(2000772747)
#1. Generate a sample of size 200 from a neglog distribution with shape α = 2 
rneglogexp = function(n,shape){
  return(rbeta(n,shape,1))
}
x = rneglogexp(200,shape=2)

# 2)Write a function to compute the shape estimator that can be used with boot.

shape_est = function(x,index=c(1:length(x))){
  x_loc = x[index]
  x_mean = sum(x_loc)/length(x_loc)
  return (x_mean/(1-x_mean))
}

# 3) Get 10000 bootstrap replicate estimates of the shape parameter using the negative-log-exponential sample that you drew. Use these replicate estimates and the point estimate from the sample to get the bias corrected bootstrap estimates.

N_replicates = 10000
boot_out_shape_param = boot(x,shape_est,R=N_replicates)
bias_corrected_boot_shape_est = 2*boot_out_shape_param$t0 - boot_out_shape_param$t

plot(density(bias_corrected_boot_shape_est))

# 4) Plot the density of the bias corrected bootstrap estimates and use the summary function on this vector. From these outputs, describe what you think is a reasonable guess at the true population value and how much uncertainty you would want to express in that guess. Plot the empirical distribution function of the bias corrected bootstrap estimates. Does the distribution look smooth to you? Why or why not?

summary(bias_corrected_boot_shape_est)
plot(ecdf(bias_corrected_boot_shape_est))

# Min.   :1.321  
# 1st Qu.:1.972  
# Median :2.080  
# Mean   :2.074  
# 3rd Qu.:2.185  
# Max.   :2.536  

# # A majority of the population lies between 1.8 and 2.4, The true population is around 2.08. The distribution looks smooth as the tails tend to 0 and 1. 