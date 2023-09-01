drivers_age <- floor(rtruncnorm(n=num_policyholders,a = 17,b=100,
                                mean = 45,sd = 10))%>% 
  as.vector() %>%
  sort(decreasing=FALSE)

# Gender simulation
genders <- c()
simulation_results <- as.vector(rbinom(n=num_policyholders,1,0.48))
for (i in 1:num_policyholders){
  if (simulation_results[i] == 1){
    genders[i] = "male"}
  else {genders[i] = "female"}
}

# Accidents History
Accidents_history <- as.vector(rpois(n=num_policyholders,
                                     lambda = -log(0.36)))

#Vehicle Age
Vehicle_age <- rnorm(n = num_policyholders,mean = 10.6, sd = 2)%>%
  as.vector() %>% floor()
Vehicle_age <- pmax(Vehicle_age,0)

# Driving experience
Driving_experience <- 