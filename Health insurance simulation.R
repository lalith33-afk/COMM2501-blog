# Lalith Lakkaraju Auto Insurance Premiums Modelling

# installing packages
install.packages("gamlss")
install.packages("gamlss.dist")
install.packages("gamlss.add")
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

install.packages("tidyverse")
library(tidyverse)


#Importing sample data
library(readr)
insurance <- read_csv("insurance.csv")
View(insurance)

# Creating a blank database to store my data
my_dataframe <- data.frame()

# Setting the number of policy holders
num_policyholders <- 10000

# Reproducibility of results
set.seed(1)

# Fitting and simulation of Age data
Age_vector <- c(insurance$age)
Age_fit <- fitDist(Age_vector, k = 2, type = "realplus", trace = FALSE,
                   try.gamlss = TRUE)
Age_mu_est <- Age_fit$mu.coefficients
Age_Sigma_est <- Age_fit$sigma.coefficients
Age_Nu_est <- Age_fit$nu.coefficients
Age_Tau_est <- Age_fit$tau.coefficients

Age_simulation <- rBCPE(n=num_policyholders,
                    mu = Age_mu_est,
                    sigma = -Age_Sigma_est,
                    nu = Age_Nu_est,
                    tau = Age_Tau_est)%>% as.vector()
Filter_age_data <- Age_simulation[Age_simulation>= 17 & Age_simulation<=100]
Age_final_data <- sample(Filter_age_data,size = num_policyholders,
                         replace = TRUE)%>% floor() %>% as.vector()
# Gender fit and simulation
Gender_data <- data.frame(insurance$sex)
probs_success <- sum(Gender_data == "male")/1338
Gender_simulation <- rbinom(num_policyholders,1,prob = probs_success)%>%
  as.vector()
Final_gender_data <- ifelse(Gender_simulation,"male","female")%>% as.vector()

# BMI fit and simulation
BMI_vector <- c(insurance$bmi)
BMI_fit <- fitDist(BMI_vector, k = 2, type = "realplus", trace = FALSE,
                   try.gamlss = TRUE)
BMI_mu_est <- BMI_fit$mu.coefficients
BMI_sigma_est <- BMI_fit$sigma.coefficients
BMI_simulation <- rgamma(n=num_policyholders,
                         shape = BMI_mu_est,
                         scale = -BMI_sigma_est)%>% as.vector()
# Children Fit
Children_vector <- c(insurance$children)
Children_fit <- fitDist(Children_vector,k = 2, type = "realplus", trace = FALSE,
                        try.gamlss = TRUE)
muest <- as.numeric(Children_fit$mu.coefficients)
sigmaest <- as.numeric(Children_fit$sigma.coefficients)
Simulation_children <- rgamma(num_policyholders,
                                 shape=muest,scale=-sigmaest)%>% as.vector()

# Simulation of charges
Charges_vector <- insurance$charges
Charges_fit <- fitDist(Charges_vector,k = 2, type = "realplus", trace = FALSE,
                       try.gamlss = TRUE)
mu1 <- 7.513
Sigma1 <- 7.22
NU1 <- 2.653
Tau1 <- -1.733
Final_charges <- rBCPE(n=num_policyholders,
      mu = mu1,
      sigma = Sigma1,
      nu = NU1 ,
      tau = -Tau1)%>% as.vector()
# Adding all simulation to a single data frame
Simulated_tibble <- data.frame(
  Age = Age_final_data,
  Gender = Final_gender_data,
  BMI = BMI_simulation,
  Children = Simulation_children,
  Charges = Final_charges
)





