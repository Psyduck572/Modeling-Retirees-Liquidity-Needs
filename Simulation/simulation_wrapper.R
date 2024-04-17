# n: number of simulated individuals
simulation_wrapper <- function(n){
  # health scenarios
  health_functions <- c("createNoShocks", "createHeartAttack", "createCancer")
  num_scenarios <- length(health_scenarios)
  s <- sample(1:num_scenarios, n, replace = TRUE, prob = c(.7, .2, .1))
  # probabilities for start ages
  age_probs <- read.csv("/Users/jessicarizzo/Desktop/Project/Data Clean/age_probs.csv")[,3]
  yrs_probs <- read.csv("/Users/jessicarizzo/Desktop/Project/Data Clean/yrs_probs.csv")[,3]
  # call simulation model on each scenario
  output_data <- as.data.frame(matrix(nrow = 1, ncol = 3))
  colnames(output_data) <- c("ID", "AGE", "SPENDING")
  for(i in 1:n){
    start_age <- sample(65:103, size = 1, replace = TRUE, prob = age_probs)
    yrs_obs <- sample(5:12, size = 1, replace = TRUE, prob = yrs_probs)
    health_profile <- eval(call(health_functions[s[i]], yrs_obs))
    this_sim <- simulation1(health_profile, start_age, 1)
    this_sim$ID <- paste("T", i, sep = "")
    output_data <- rbind(output_data, this_sim)
  }
  output_data <- output_data[-1,]
  return(output_data)
  
}

test <- simulation_wrapper(10)

for_plot <- simulation_wrapper(50)
u <- unique(for_plot$ID)
plot(x = 1, type = "n",
     xlim = c(65, 103), 
     ylim = c(0, 7), 
     xlab = "Age", ylab = "Adjusted Spending", main = "50 Simulated Spend Paths")
cols <- sample(colors(), 10)
for(i in 1:10){
  this_id <- subset(for_plot, for_plot$ID == u[i])
  points(this_id$AGE, this_id$SPENDING, type = "l", col = cols[i])
}
