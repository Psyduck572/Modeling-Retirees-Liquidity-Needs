# n: number of simulated individuals
simulation_wrapper <- function(n){
  # probabilities for start ages and years observed
  age_probs <- read.csv("/Users/jessicarizzo/Desktop/Project/Data Clean/age_probs.csv")[,3]
  yrs_probs <- read.csv("/Users/jessicarizzo/Desktop/Project/Data Clean/yrs_probs.csv")[,3]
  
  # call simulation model on each scenario
  output_data <- as.data.frame(matrix(nrow = 1, ncol = 3))
  colnames(output_data) <- c("ID", "AGE", "SPENDING")
  for(i in 1:n){
    # Pick starting age and years observed
    start_age <- sample(65:94, size = 1, replace = TRUE, prob = age_probs)
    yrs_obs <- sample(5:12, size = 1, replace = TRUE, prob = yrs_probs)
    health_profile <- create_health_profile(yrs_obs, start_age)
    this_sim <- simulation1(health_profile)
    this_sim$ID <- paste("T", i, sep = "")
    output_data <- rbind(output_data, this_sim)
  }
  output_data <- output_data[-1,]
  return(output_data)
  
}

# Check that it works
test <- simulation_wrapper(10)

# Plot n simulated spending paths
n <- 1000
for_plot <- simulation_wrapper(n)
u <- unique(for_plot$ID)
plot(x = 1, type = "n",
     xlim = c(65, 90), 
     ylim = c(0, 10), 
     xlab = "Age", ylab = "Adjusted Spending", main = paste(n, "Simulated Spend Paths"))
cols <- sample(colors(), n, replace = TRUE)
for(i in 1:n){
  this_id <- subset(for_plot, for_plot$ID == u[i])
  points(this_id$AGE, this_id$SPENDING, type = "l", col = cols[i])
}

# Sample of 1000 real spend paths vs. simulated
sim <- simulation_wrapper(100)
u <- unique(ss_train$HHIDPN)
l <- length(u)
ids <- sample(1:l, size = 100, replace = FALSE)
test_ids <- u[ids]
plot(x = 1, type = "n",
     xlim = c(65, 90), 
     ylim = c(0, 10), 
     xlab = "Age", ylab = "Adjusted Spending", main = "Simulated vs. Training Data")
s <- unique(sim$ID)
for(i in 1:100){
  this_id_train <- subset(ss_train, ss_train$HHIDPN==test_ids[i])
  this_id_sim <- subset(sim, sim$ID == s[i])
  points(this_id_sim$AGE, this_id_sim$SPENDING, type = "l", col = "red")
  points(this_id_train$AGE, exp(this_id_train$SPEND_SS), type = "l", col = "black")
}
