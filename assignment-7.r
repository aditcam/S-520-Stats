# ******* HW 7 SOLUTION *******
# Submitted by Aditya Shekhar Camarushy 
# IU ID : adcama@iu.edu
# University ID : 2000772747

# Load required Data

setwd("/Users/aditcam/Desktop/Aditya/Work/Subjects/Stat-S520/HW/q7")
load(file = "hw7.rdata")
set.seed(2000772747)

# ____________________________________________________________________________

# Question 1.a

length(negative_binomial_pop) # Size of the population is 654321
range(negative_binomial_pop) # 3 128

# Standard Deviation for population 

sqrt(mean((negative_binomial_pop-mean(negative_binomial_pop))^2)) # 12.46461

# Mean Absolute Deviation 

  # Using median as the measure of central tendency in M.A.D calculation
mean(abs(negative_binomial_pop-median(negative_binomial_pop))) # 9.873192

  # Using mean as the measure of central tendency
#mean(abs(negative_binomial_pop-mean(negative_binomial_pop)))

# Inter Quartile range

quantile(negative_binomial_pop,0.75) - quantile(negative_binomial_pop,0.25) # 16

# For the random sample
# Taking a random sample of size 243

neg_bin_samp = sample(negative_binomial_pop,243) 

# Standard Deviation for population 

sqrt(mean((neg_bin_samp-mean(neg_bin_samp))^2)) # 12.21159

# Mean Absolute Deviation

  # Using median as the measure of central tendency
mean(abs(neg_bin_samp-median(neg_bin_samp))) # 9.641975

  # Using mean as the measure of central tendency
mean(abs(neg_bin_samp-mean(neg_bin_samp))) # 9.678301

# Inter Quartile range

quantile(neg_bin_samp,0.75) - quantile(neg_bin_samp,0.25) # 16

# ****** ANSWER *******
# Considering the fact that, out of a population size of 654321 we sample out a population of 243 which is just about 0.037% of the total population it is pleasantly surprising that our parameters either exactly match (as in case of IQR) or differ by about ± 0.2 (in case of the other parameters), which could be acceptable given that the data has a range of 125 (3 to 128) and 0.2 is relatively negligible. 
# We can conclude that the sampling is representative of the total population to a high degree. 

# ____________________________________________________________________________

# Question 1.b

# Quartile based skewness (Yule)

upper_length = quantile(laplace_pop,0.75) - quantile(laplace_pop,0.5)
lower_length = quantile(laplace_pop,0.5) - quantile(laplace_pop,0.25)
iqr = quantile(laplace_pop,0.75) - quantile(laplace_pop,0.25)
(upper_length - lower_length) / iqr # 1.817454e-05 

# Absolute deviation based skewness 
upper = mean((laplace_pop-median(laplace_pop))*(laplace_pop>median(laplace_pop)))
lower = mean((median(laplace_pop)-laplace_pop)*(laplace_pop<median(laplace_pop)))
mad = mean(abs(laplace_pop-median(laplace_pop)))
(upper - lower) / mad # 0.001292992

# expectations based skewness (Pearson)
mean((laplace_pop-mean(laplace_pop))^3) / (mean((laplace_pop-mean(laplace_pop))^2))^(3/2) # 0.01470812

# For the random sample
# Taking a random sample of size 85

laplace_samp = sample(laplace_pop,85)

# Quartile based skewness (Yule)

upper_length = quantile(laplace_samp,0.75) - quantile(laplace_samp,0.5)
lower_length = quantile(laplace_samp,0.5) - quantile(laplace_samp,0.25)
iqr = quantile(laplace_samp,0.75) - quantile(laplace_samp,0.25)
(upper_length - lower_length) / iqr # -0.3433153 

# Absolute deviation based skewness 
upper = mean((laplace_samp-median(laplace_samp))*(laplace_samp>median(laplace_samp)))
lower = mean((median(laplace_samp)-laplace_samp)*(laplace_samp<median(laplace_samp)))
mad = mean(abs(laplace_samp-median(laplace_samp)))
(upper - lower) / mad # -0.197469

# expectations based skewness (Pearson)
mean((laplace_samp-mean(laplace_samp))^3) / (mean((laplace_samp-mean(laplace_samp))^2))^(3/2) # 0.3330448

# ****** ANSWER *******

# Based on the finite population measures we could say that the super population in approximately skew zero as all skew values computed yield values of the order 10^(-2) or smaller and seem to be more or less in agreement.

# We observe a different output from the sample estimates. Based on the sample estimates we cannot conclude that the super population is skewness zero. This is because if we look at the 3 values computed by the different skewness metrics for the sample estimates, we observe widely varying skewness values, some of which are positive and some negative which means that we can't be certain about the super population by just looking at the skewness values. 

# ____________________________________________________________________________

# Question 1.c

length(singh_pop) # Size of the population is 268718
arr = range(singh_pop) # 2.343826e-06 5.534764e+00 --> 5.534762
arr[2]-arr[1]

# Mean 
mean(singh_pop) # 0.9325387

# Median 
median(singh_pop) # 0.7417126

# Average of 1st and 3rd quartile i.e mean of 0.25 and 0.75 quartiles

mean(c(quantile(singh_pop,0.75),quantile(singh_pop,0.25))) # 0.8223922

# For the random sample
# Taking a random sample of size 64

singh_samp = sample(singh_pop,64)

# Mean 
mean(singh_samp) # 0.9787802

# Median 
median(singh_samp) # 0.8103692

# Average of 1st and 3rd quartile i.e mean of 0.25 and 0.75 quartiles

mean(c(quantile(singh_samp,0.75),quantile(singh_samp,0.25))) # 0.8710765

# ****** ANSWER *******

#  In my opinion, the plug-in estimators do not correspond very well even though the computed parameter values may seem very close (numerically). This is because, in the given finite population the values range from 2.343826e-06 to 5.534764 with a range of 5.534762 .i.e. the values are numerically quite small, sometimes in the order of 10^(-6). We can observe a difference of the order 10^(-2) in the parameter values for the finite population and the sample estimate, which can by very inaccurate in the given context. 
 
# On observing the measures of center, based on the median, mean and mean of Q1,Q3 it seems as though a majority of the values towards the center of the population are closer to 0.75 - 0.88.

# ____________________________________________________________________________

# Question 1.d 
 
length(treering_pop) # Size of the population is 7979
arr = range(treering_pop) # 0.002 1.908 --> 1.906
arr[2]-arr[1]

# generating deciles for the Treering poulation 
quantile(treering_pop,seq(.1, .9, by = .1)) 

#    10%    20%    30%    40%    50%    60%    70%    80%    90% 
# 0.5890 0.7746 0.8850 0.9660 1.0340 1.0970 1.1606 1.2360 1.3422 

# Taking a sample of size 202
treering_samp = sample(treering,202)

# generating deciles for the Treering sample of size 202
quantile(treering_samp,seq(.1, .9, by = .1)) 

# 10%    20%    30%    40%    50%    60%    70%    80%    90% 
# 0.5748 0.7598 0.8820 0.9770 1.0420 1.1190 1.1790 1.2348 1.3388 

# ****** ANSWER *******

# Given that we have sampled 202 out of 7979 elements of the finite population, some of the decile values are quite different while some are similar. We can say that the correspondence is average. 
 
# The estimators indicate that a lot of the smaller values i.e. <0.5 are concentrated before the first decile and are relatively less in number, similarly is the case for numbers >1.338 which are less in number. We can say that a majority of the values are between 0.5 and 1.3388.  

# For the 3rd and 8th decile I would feel most comfortable making statements about the population using the estimates.

# ____________________________________________________________________________

# Question 1.e
 
length(speed_pop) # Size of the population is 8437
arr = range(speed_pop) # 19 67 --> 48
arr[2]-arr[1]

log_x = log(speed_pop)

exp_x = exp(speed_pop)

sqrt_x = sqrt(speed_pop)

# Calculation of expected value of expected value of log,exc and sqrt functions over total population 

# Expectation value of population
mean(log_x) # 3.6182
mean(exp_x) # 2.257899e+25
mean(sqrt_x) # 6.127569

# Taking a sample of size 49
speed_samp = sample(speed_pop,49)

log_x_samp = log(speed_samp)

exp_x_samp = exp(speed_samp)

sqrt_x_samp = sqrt(speed_samp)

# Expectation value for sample
mean(log_x_samp) # 3.630124
mean(exp_x_samp) # 6.610774e+21
mean(sqrt_x_samp) # 6.165467

# ****** ANSWER *******

# For log and exp functions, the expectation values seem to correspond well between the finite sample and the population, but in case of exp function they differ very widely. I would trust the estimate of sqrt/log function more than exp function simply because the number's value don't change as much when applying these function i.e. the increase/decrease in values remain more or less consistent and the order remains similar for the most part whereas in case of exp functions values can vary a lot and increase/decreases are quite incosistent. 
