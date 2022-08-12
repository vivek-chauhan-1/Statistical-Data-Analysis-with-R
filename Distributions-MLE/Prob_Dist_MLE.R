#' ---
#' title: "ASSIGNMENT 1"
#' author: "Vivek Chauhan (20955627)"
#' date: "Feb 3rd, 2022"
#' ---


suppressWarnings({
##Assignment 1

#Question 1
#Probability calculation:COVID
# a. P(getting COVID) = P(C) = (People who got COVID) / (World Population)
# 332,210,522/7,794,795,127 = 0.0426
# b. P(dying from COVID) = P(died from COVID) = (people died from COVID) / (Total population)
# 5,494,965/7,794,795,127 = 0.00070
# c. P(dying conditional one has covid) = P(dying_from_COVID) / P(getting COVID)
# 0.00070/0.042 = 0.016  
#  
# d. Fatality rate = the number of deaths from the COVID divided by the number of confirmed cases.
# ifr [Infection fatality Rate  (for male 21 years of age)]  = 0.0001
# P(dying conditional one has covid and is male of 21 years age) = 0.0001
#   P(A|B,C)=P(A∩B∩C)P(B∩C)
# P(dying|covid,man 21 yrs of age)=P(dying ∩ covid ∩man 21 yrs of  age)/P(covid ∩ man 21 yrs of age)
   
#t_c = people who got covid 
t_c <- 332210522

#typeof(t_c)
#w_p = total world population
w_p <-  7794795127
#t_d = people who died from covid
t_d <- 5494965

# a) p_c = probability of getting covid 
p_c <- t_c / w_p
print(paste("probabability of getting covid : ",p_c))

# b) p_dc = probability of dying from covid
p_dc <- t_d / w_p
print(paste("probabability of dying from covid : ",p_dc))

# c) p_d_c = probability of dying conditional that one has covid
p_d_c <- p_dc / p_c
print(paste("probabability of dying conditional that one has covid : ",p_d_c))

# d) p_d_c_m21 = probabibility of dying conditional that one has covid and is male of 21yrs age
ifr <- 0.01* 10^(-2)

#p_d_c_m21 <- ifr
print(paste("probabability of dying conditional that one has covid and is male of 21yrs: ",ifr))

#############################################################################

##Question 2
#Simple urn simulation with R

#2 black, 3 white and 3 Red balls
simple_urn <- c(rep(1,2), rep(2, 3), rep(3,3))
#n_trails <- 10
n_trails <- 10^4

#matrices without sampling for with and without replacement
w_replacemnent <- matrix(NA, nrow = n_trails, ncol = length(simple_urn)) 
wo_replacemnent <- matrix(NA, nrow = n_trails, ncol = length(simple_urn)) 

#head(w_replacemnent)
#head(wo_replacemnent)

#sampling to replace NA with values in matrix
for (i in 1:n_trails){
  w_replacemnent[i,] <- sample(simple_urn, length(simple_urn), replace =TRUE)
  wo_replacemnent[i,] <- sample(simple_urn, length(simple_urn), replace =FALSE)
}

# we are concerned about the first and last element of 
#one row at a time of sampled matrix
black_white <- function(x){
  succces <-c()
  # creating a NULL vector
  target_seq <- 1:3
 
  #sequence is arranged such that 1,2,3 and represents color as listed below:
  #Black:1
  #White:2
  #Red:3 
  # if first ball is black and last ball is white , then it is a success
  # we are not considering the sequence between first and last ball draw
  if(x[1]==target_seq[1] && x[8]==target_seq[2]){
    success <- 1
  }
  else{
    success <- 0
  }
  return(success)
}

#black_white(c(2,1,2,1,4,1,2,2))
# applying custom function on sampled matrix in a row wise fashion
w_replacement_result <- apply(w_replacemnent, 1, function(x) black_white(x))
wo_replacement_result <- apply(wo_replacemnent, 1, function(x) black_white(x))
probability_w_replacement <- (length(w_replacement_result) - sum(w_replacement_result == 0))/
  length(w_replacement_result)
probability_wo_replacement <- (length(wo_replacement_result) - sum(wo_replacement_result == 0))/
  length(wo_replacement_result)

print(paste("Probability of getting first black and last white with replacement is",probability_w_replacement))
print(paste("Probability of getting first black and last white without replacement is",probability_wo_replacement))

#####################################################################

#Question 3
#Generalized urn simulation with R

polya_run <- function(int_n, lambda, n_trial){
 
  #creating NA matrix
 polya_result <- matrix(NA, nrow = n_trial, ncol = length(int_n))
 #replacing values one row at a time
 polya_result[1,] <- int_n
 
 for(i in 2:n_trial){
 
  #cummulative propbabilities 
 interval <- cumsum(polya_result[i-1,])/sum(polya_result[i-1,])
 
 #picking 1 random value to comapare against
 draw <- runif(1)
 
 #assigning based on comaparison with draw value
 bin_asisgn <- c(interval > draw)*1
 
 bin_select <- c()
 #creating NULL vector
 for(j in 1:length(bin_asisgn)){
   
 if(bin_asisgn[j] == 1){
  bin_select <- j
  #quit the loop
  break
 } 
 }
 
 polya_result[i,] <- polya_result[i-1,]
 if(polya_result[i, bin_select] + lambda <= 0){
   #when there is only one ball of particular color left to draw , 
   #we can at max draw one ball and that will make balls of that color 0 in urn
   polya_result[i, bin_select] = 0
 }
 else{
 polya_result[i, bin_select] <- polya_result[i, bin_select] + lambda
 }
 }
 return(polya_result)
}


#set.seed(10)
polya_result_1 <- polya_run(int_n = c(20,10,7,7), lambda = -2, n_trial = 20)
#head(polya_result_1)

# To compare with 3 different seed values
set.seed(20)
polya_result_2 <- polya_run(int_n = c(20,10,7,7), lambda = -2, n_trial = 20)
#head(polya_result_2)
#set.seed(30)
polya_result_3 <- polya_run(int_n = c(20,10,7,7), lambda = -2, n_trial = 20)
#head(polya_result_3)
#set.seed(40)
polya_result_4 <- polya_run(int_n = c(20,10,7,7), lambda = -2, n_trial = 20)
#head(polya_result_4)

polya_result_list <- list(polya_result_1, polya_result_2, polya_result_3, 
polya_result_4)

#to plot in a 2,2 grid
#visulizing balls being drawn 
par(mfrow = c(2, 2)) 
color_list <- c("black","blue","red","green")

for(k in 1:length(polya_result_list)){
  
 y_min <- min(polya_result_list[[k]])
 y_max <- max(polya_result_list[[k]])
 plot(k, type = "n", ylim = c(y_min, y_max),  xlim = c(1, 20), xlab ="Time", ylab = "# of balls")   
 
 for(i in 1:4){

 lines(1:20, polya_result_list[[k]][,i],col = color_list[i], lwd=1.5)
 
 }
}


######################################################################

#Question 4
#MLE for normal distribution#

#setting seed as given in assignment
set.seed(200)
#mean=0
#sd=2
mynorm <- rnorm(50, mean = 0 , sd = 2)
#mynorm

# Negative log likelihood function maximizes the value for Normal distribution, hence negative is used
# Log likelihood is used as it educes the expensive multiplactions
log_likelihood <- function(params){
  
  log_ll <- -sum(dnorm(mynorm, params[1],params[2], log=TRUE))
  return(log_ll)
}
#starting with random paramters
#params is a vector with first parameter as mean and second parameter as sd
params <- c(1000,1000)

# using nlm function to optimize
mle_norm <- nlm(log_likelihood, params)
#mle_norm
#we are concerned with only mean and sd values only  
mean <- mle_norm$estimate[1]
print(paste("Estimated mean using mle for 50 random normal numbers is : ",mean))
sd<-mle_norm$estimate[2]
print(paste("Estimated sd using mle for 50 random normal numbers is : ",sd))

#mean and variance calculated using nlm function are close to initial parameters passed to distibution 

#library(MASS)
#fitdistr(x, "log-normal")

# to compare with 500 random generated values
mynorm_5000 <- rnorm(5*10^3, mean = 0 , sd = 2)
#mynorm_5000
log_likelihood <- function(params){
  
  log_ll <- -sum(dnorm(mynorm_5000, params[1],params[2], log=TRUE))
  return(log_ll)
}

params <- c(1000,1000)
mle_norm_5000 <- nlm(log_likelihood, params)
#mle_norm_5000
mean_5000 <- mle_norm_5000$estimate[1]
print(paste("Estimated mean using mle for 5000 random normal numbers is : ",mean_5000))
sd_5000<-mle_norm_5000$estimate[2]
print(paste("Estimated sd using mle for 5000 random normal numbers is : ",sd_5000))

## The vale of mean and variance with 5000 random values is much closer to original parameters

######################################################################

#Bonus Question 1
#P(probability of Sally Clark being innocent) = 
#H: Hypothesis : both children died of SIDS : Sally is innocent
#A: Alternate Hypothesis : children died of any other reasons, i.e. : mother killed them : Sally is guilty
#D: Data : both children died
#We need to find: probability of both children dying of SIDS given that both children died
#P(H|D) = ( P(D|H) * P(H) )/ ( P(D|H) *P(H) + P(D|A) *P(A) ) 
#P(H) = 1/ 73* 10^6 [1 in 73 million]
#P(A) = 1- P(H) 
#P(D|A) = P(D intersection A) / P (A) = 1/ 200 *10^6 [ 1 in 200 million]
#P(D|H) = 1 [becuae both child died]

#p_h = probability of both children dying of SIDS
#p_a = probability of both children not dying from SIDS
#p_d_h = probability of both children dying conditional both children dies of SIDS
#p_d_a = probability of both children dying conditional they did not died of SIDS
#p_h_d = probability of sally being innocent

p_h <- 1/ (73 *10^6)
p_a <- 1-p_h
p_d_h <- 1
p_d_a <- 1/ (200* 10^6)
p_h_d <- p_h /(p_h + (p_d_a*(1-p_h)))
print(paste("probability that sally is innocent is : ",p_h_d))


###########################################################################

##Bonus Question 2
#Cross-validation for Normal and Gamma distribution

library(fitdistrplus)
library(MASS)

set.seed(200)

##Normal distribution
rn_data <- rnorm(10^2, mean=10, sd=2) 
##Gamma distribution with r=2 and lambda=0.5
#rn_dgamma <- rgamma(n=10^2,shape = 2,scale = 0.5)
rn_dgamma <- rgamma(n=10^2,shape = 2,rate = 0.5)

#creating a function for cross validation using n_folds for given data and distribution
CV_fun <- function(n_fold, n_rep, uni_data, distribution){
  
  #NULL vectors for test data 
  test_list <- list()
  test_list_all <- list()
  for(k in 1:n_rep){ 
    
    #sampling to shuffle the data and remove any data that might be grouped together by index
    uni_data_s <- sample(uni_data)
    # Split in n_fold subsets
    folds <- cut(seq(1, length(uni_data_s)), breaks = n_fold, labels=FALSE)
    
    for(i in 1:n_fold){ 
      #since it is a K fold cross validation, we will iterte over every fold
      testID <- which(folds==i,arr.ind=TRUE) 
      #we take out data in test and train based on indices that we assign after sampling
      testD <- uni_data_s[testID]
      #test data contains data that contains the ith fold
      trainD <- uni_data_s[-testID]
      #all data that is not contained in test goes in train 
      
      #based on the distributiojn receive for CV_fun from user
      if (distribution == "Normal") { 
        trained <- fitdist(trainD, "norm", method="mle")
        #usnig MLE method to fit train data normally 
        test_result <- sum(dnorm(testD, trained$estimate[1],trained$estimate[2], log = T)) 
        #taking sum of the logs of normal density using mean and variance values of trained dataset to test
      }else if (distribution == "Gamma"){
        trained <- fitdist(trainD, "gamma", method="mle")
        test_result <- sum(dgamma(testD, shape = trained$estimate[1], rate = trained$estimate[2], log = T)) 
      }else{
        #in case distributionother than normal and gamma are passed from function
        print(paste("Unrecognized distribution requested", distribution, sep=" "))
        #we exit the function
        quit(status=1)
      }
      test_list[[i]] <- test_result
    }
    test_list_all[[k]] <- sum(unlist(test_list)) # the sum of all n-fold likelihood
  }
  return(test_list_all)
}

#using random normal data for Normal and Gamma distributions
CV_norm <- CV_fun(n_fold = 10, n_rep = 10, uni_data = rn_data, distribution ="Normal")
CV_gamma <- CV_fun(n_fold = 10, n_rep = 10, uni_data = rn_data, distribution ="Gamma")

norm_u <- unlist(CV_norm)
gamma_u <- unlist(CV_gamma)
print(paste("Mean of Normal distribution for normally distributed data: ",mean(norm_u)))
print(paste("Mean of Gamma distribution for normally distributed data: ",mean(gamma_u)))
print(paste("Predictive power of Gamma distribution performs slightly better for normally distributed data."))

#using random gamma data for Normal and Gamma distributions
CV_norm <- CV_fun(n_fold = 10, n_rep = 10, uni_data = rn_dgamma, distribution ="Normal")
CV_gamma <- CV_fun(n_fold = 10, n_rep = 10, uni_data = rn_dgamma, distribution ="Gamma")
norm_u <- unlist(CV_norm)
gamma_u <- unlist(CV_gamma)
print(paste("Mean of Normal distribution for gamma distributed data: ",mean(norm_u)))
print(paste("Mean of Gamma distribution for gamma distributed data: ",mean(gamma_u)))
print(paste("Predictive power of Gamma distribution performs better for gamma distributed data."))


})