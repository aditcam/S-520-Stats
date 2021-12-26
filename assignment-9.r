
# Load required Data

setwd("/Users/aditcam/Desktop/Aditya/Work/Subjects/Stat-S520/HW/Solutions")
load(file = "hw9.rdata")
set.seed(2000772747)

# Function that creates 50 sample means after taking means of random samples of size samp_size
sample_means = function(x,samp_size) {
  samp <- c()
  for ( i in 1:50) {
    rand_samp = sample(x,samp_size,replace = TRUE)
    samp <- c(samp,mean(rand_samp))
  }
  return(samp)
}

############ Question 1 ###############

samp_size = floor(0.75*length(x_q1))
means = sample_means(x_q1,samp_size)
s = 3
conf_int = 0.98
alpha = 1 - conf_int
sigma = s*pi/sqrt(3)

margin_error = qnorm(1-alpha)*sigma/sqrt(length(means))
left_int = c(-Inf,mean(means)+margin_error)
print(left_int)
#output [1]     -Inf 7.751644

left_int_rejected_mu_vals = c(mean(means)+margin_error,Inf)
print(left_int_rejected_mu_vals)
#output [1] 7.751644      Inf

############ Question 2 ###############

samp_size = floor(0.75*length(x_q2))
means = sample_means(x_q2,samp_size)
s = 10
conf_int = 0.99
alpha = 1 - conf_int
sigma = s*sqrt(0.138)
margin_error = qnorm(1-alpha/2)*sigma/sqrt(length(means))
left_int = c(-Inf,mean(means)+margin_error)
print(left_int)
#output [1]     -Inf 346.4984


left_int_rejected_mu_vals = c(mean(means)+margin_error,Inf)
print(left_int_rejected_mu_vals)
#output [1] 346.4984      Inf

right_int = c(mean(means)-margin_error,Inf)
print(right_int)
#output [1] 343.792     Inf

right_int_rejected_mu_vals = c(-Inf,mean(means)-margin_error)
print(right_int_rejected_mu_vals)
#output  -Inf 343.792

symmetric_int = c(mean(means)-margin_error,mean(means)+margin_error)
print(symmetric_int)
# output [1] 343.7920 346.4984

symmetric_int_rejected_mu_values_left = c(-Inf,mean(means)-margin_error)
print(symmetric_int_rejected_mu_values_left)
# output [1]  -Inf 343.792

symmetric_int_rejected_mu_values_right = c(mean(means)+margin_error,Inf)
print(symmetric_int_rejected_mu_values_right)
# output [1] 346.4984      Inf

############ Question 3 ###############

samp_size = floor(0.75*length(x_q3))
means = sample_means(x_q3,samp_size)
s = 2
conf_int = 0.99
alpha = 1 - conf_int
sigma = s*sqrt(.54)
margin_error = qnorm(1-alpha)*sigma/sqrt(length(means))
right_int = c(mean(means)-margin_error,Inf)
print(right_int)
#output [1] -20.0766      Inf

right_int_rejected_mu_vals = c(-Inf,mean(means)-margin_error)
print(right_int_rejected_mu_vals)
#output [1]  -Inf -20.0754

############ Question 4 ###############

s = 3
conf_int = 0.995
alpha = 1 - conf_int
sigma = s*sqrt(2)
delta = 0.2

n_0 = ceiling((2*sigma*qnorm(1-alpha/2)/delta)^2)
print(n_0)
#output [1] 14183

############ Question 5 ###############

alpha = 0.02
n =35
h_0 = 12
s = 3
v = 5
sigma = sqrt((v/(v-2))*s^2)

rejected_int_right = c(h_0+qnorm(1-alpha)*sigma/sqrt(n), Inf)
print(rejected_int_right)
# output [1] 13.34449      Inf

############ Question 6 ###############

samp_size = floor(0.75*length(x_q6))
means = sample_means(x_q6,samp_size)
r = 10
conf_int = 0.99
alpha = 1 - conf_int
mu = mean(means) # plug in estimator for mean
sigma_hat = sqrt(mu*(1+mu/r))
margin_error = qnorm(1-alpha)*sigma_hat/sqrt(length(means))

right_int = c(mu-margin_error,Inf)
print(right_int)
#output [1] 12.65644      Inf

right_int_rejected_mu_values = c(0,mu-margin_error)
print(right_int_rejected_mu_values)
#output [1]  0.00000 12.65644

############ Question 7 ###############

samp_size = floor(0.75*length(x_q7))
means = sample_means(x_q7,samp_size)
conf_int = 0.94
alpha = 1 - conf_int
mu = mean(means)
sigma_hat = mu*sqrt((4-pi)/pi)
margin_error = qnorm(1-alpha)*sigma_hat/sqrt(length(means))

left_int = c(0,mu+margin_error)
print(left_int)
#output [1] 0.000000 7.701419

left_int_rejected_mu_values = c(mu+margin_error,Inf)
print(left_int_rejected_mu_values)
#output [1] 7.701419      Inf

############ Question 8 ###############

samp_size = floor(0.75*length(x_q8))
means = sample_means(x_q8,samp_size)
conf_int = 0.97
alpha = 1 - conf_int
lambda = 2.5
mu = mean(means)
sigma_hat = sqrt((mu^3)/lambda)
margin_error = qnorm(1-alpha/2)*sigma_hat/sqrt(length(means))

left_int = c(0,mu+margin_error)
print(left_int)
#output [1] 0.000000 1.280755

left_int_rejected_mu_values = c(mu+margin_error,Inf)
print(left_int_rejected_mu_values)
#output [1] 1.280755      Inf

right_int = c(mu-margin_error,Inf)
print(right_int)
#output [1] 0.8529774       Inf

right_int_rejected_mu_values = c(0,mu-margin_error)
print(right_int_rejected_mu_values)
# output [1] 0.0000000 0.8529774

symmetric_int = c(mu-margin_error,mu+margin_error)
print(symmetric_int)
# output [1] 0.8529774 1.2807546

symmetric_int_rejected_mu_values_left = c(0,mu-margin_error)
print(symmetric_int_rejected_mu_values_left)
# output [1] 0.0000000 0.8529774

symmetric_int_rejected_mu_values_right = c(mu+margin_error,Inf)
print(symmetric_int_rejected_mu_values_right)
# output [1] 1.280755      Inf

############ Question 9 ###############

conf_int = 0.975
alpha = 1 - conf_int
alpha_shape = 7
delta = 0.1
mu = 5
sigma_hat = sqrt(alpha_shape/(alpha_shape-2))*mu
n_0 = ceiling((2*sigma_hat*qnorm(1-alpha)/delta)^2)
print(n_0)
# output [1] 53781

############ Question 10 ###############

alpha= 0.92
mu = 7.5
sigma_hat = sqrt(mu)
n = 102 

symmetric_int_rejected_mu_values_left = c(-Inf,mu-qnorm(1-alpha/2)*sigma/sqrt(n))
print(symmetric_int_rejected_mu_values_left)

symmetric_int_rejected_mu_values_right = c(mu+qnorm(1-alpha/2)*sigma/sqrt(n),Inf)
print(symmetric_int_rejected_mu_values_right)

############ Question 11 ###############

n = 48
mean_samp = 4.68
sigma_samp = sqrt(2.35)
conf_int = 0.96
alpha = 1 - conf_int

right_int = c(mean_samp-qnorm(1-alpha)*sigma_samp/sqrt(n),Inf)
print(right_int)
# output [1] 4.292634      Inf

############ Question 12 ###############

library(boot)
data("gravity")
x_q12 = gravity$g

samp_size = floor(0.75*length(x_q12))
means = sample_means(x_q12,samp_size)

conf_int = 0.99
alpha = 1 - conf_int
sigma_hat = sd(means)
mu_hat = mean(means)
margin_error = qnorm(1-alpha/2)*sigma_hat/sqrt(length(means))

left_int = c(0,mu_hat+margin_error)
print(left_int)
#output [1]  0.00000 79.56497

left_int_rejected_mu_values = c(mu_hat+margin_error,Inf)
print(left_int_rejected_mu_values)
#output [1] 79.56497      Inf

right_int = c(mu_hat-margin_error,Inf)
print(right_int)
#output [1] 78.38903      Inf

right_int_rejected_mu_values = c(0,mu_hat-margin_error)
print(right_int_rejected_mu_values)
# output [1] 0.00000 78.38903

symmetric_int = c(mu_hat-margin_error,mu_hat+margin_error)
print(symmetric_int)
# output [1] 78.38903 79.56497

symmetric_int_rejected_mu_values_left = c(0,mu_hat-margin_error)
print(symmetric_int_rejected_mu_values_left)
# output [1] 0.00000 78.38903

symmetric_int_rejected_mu_values_right = c(mu_hat+margin_error,Inf)
print(symmetric_int_rejected_mu_values_right)
# output [1] 79.56497      Inf

############ Question 13 ###############

data("neuro")
x_q13 = neuro[,3]
conf_int = 0.96
alpha = 1 - conf_int
sigma_hat = sd(means)
mu_hat = mean(means)
margin_error = qnorm(1-alpha)*sigma_hat/sqrt(length(means))

left_int = c(0,mu_hat+margin_error)
print(left_int)
# output [1] 0.00000 79.37662

left_int_rejected_mu_vals = c(mu_hat+margin_error,Inf)
print(left_int_rejected_mu_vals)
# output [1] 79.37662      Inf

############ Question 14 ###############

conf_int = 0.95
alpha = 1 - conf_int
sigma_hat = sqrt(6.8)
delta = 0.2
n_0 = ceiling((2*sigma_hat*qnorm(1-alpha)/delta)^2)
print(n_0)
# output [1] 1840

############ Question 15 ###############

alpha= 0.02
h_0 = 4.5
n = 72
sigma_hat = sqrt(3.7)
rejected_region_left = c(-Inf, h_0-qnorm(1-alpha)*(sigma_hat/sqrt(n)))
print(rejected_region_left)
# output [1]     -Inf 4.034433