{r}
#This script is used to do an histogram of 3000 iterations from the values of the parameters from a gamma distribution.
#We used the maximum likelihood method to estimate the value of the parameters.
#The parameters are considered as random variables.

#The partial derivative from the log likelihood with respect to  alpha is declared:
loglikelihoodpart<-function(a, m1, vectorg) {
  log(a)-log(m1)+sum(log(vectorg))/30-digamma(a)
}

#For each of the 3,000 simulations, there is a vector of 30 samples which each sample follows a Gamma(2.5, 3) distribution.
#The objective of this excercise is to show an histogram of the distribution of alpha and beta.
#alpha and beta are estimated by the maximum likelihood method.
#For the gamma vector, we will take the alpha parameter as 2.5 and beta parameter as 3.
#This inference is because of its closeness to the estimation made by the method of moments.

alphas_vector<-c()
betas_vector<-c()
k<-1
while (k<=3000) {
  vector_gamma <- rgamma(n = 30, shape = 2.5, rate = 3)
  m1<-mean(vector_gamma)
  
  #We calculate the value of alphaMLM
  alphai <- uniroot(loglikelihoodpart, c(0.1,10), m1=m1, vectorg=vector_gamma)
  alphai<-alphai$root
  alphas_vector[k]<-alphai
  
  #The value of alphaMLM is used to calculate the value of betaMLM
  betai<-m1/alphai
  betas_vector[k]<-betai
  
  
  k<-k+1
}

hist(alphas_vector,
     col = "steelblue",
     main = "Alpha parameter distribution",
     xlab = "",
     ylab = "Frecuency")
hist(betas_vector,
     col = "red",
     main = "Beta parameter distribution",
     xlab = "",
     ylab = "Frecuency")


