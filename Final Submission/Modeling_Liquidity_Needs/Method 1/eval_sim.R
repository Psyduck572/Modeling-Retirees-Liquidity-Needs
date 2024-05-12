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

sim <- simulation_wrapper(10000)

# Mean
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- mean(age_sub$SPENDING)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "Mean")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- mean(age_sub$SPEND_SS)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# Median
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- median(age_sub$SPENDING)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "Median")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- median(age_sub$SPEND_SS)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# 5th Percentile
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- quantile(age_sub$SPENDING, .05, na.rm = TRUE)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "5th Percentile")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- quantile(age_sub$SPEND_SS, .05, na.rm = TRUE)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# 10th Percentile
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- quantile(age_sub$SPENDING, .1, na.rm = TRUE)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "10th Percentile")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- quantile(age_sub$SPEND_SS, .1, na.rm = TRUE)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# 25th Percentile
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- quantile(age_sub$SPENDING, .25, na.rm = TRUE)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "25th Percentile")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- quantile(age_sub$SPEND_SS, .25, na.rm = TRUE)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# 75th Percentile
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- quantile(age_sub$SPENDING, .75, na.rm = TRUE)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "75th Percentile")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- quantile(age_sub$SPEND_SS, .75, na.rm = TRUE)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# 90th Percentile
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- quantile(age_sub$SPENDING, .9, na.rm = TRUE)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "90th Percentile")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- quantile(age_sub$SPEND_SS, .9, na.rm = TRUE)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# 95th Percentile
ages <- unique(sim$AGE)
sim_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(sim, sim$AGE == age)
  sim_metric[k] <- quantile(age_sub$SPENDING, .95, na.rm = TRUE)
  k <- k + 1
}
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 2), 
     xlab = "Age", ylab = "Adjusted Spending", main = "95th Percentile")
points(ages, exp(sim_metric), type = 'l', col = "red")

ages <- 65:85
train_metric <- rep(NA, length(ages))
k <- 1
for(age in ages){
  age_sub <- subset(ss_train, ss_train$AGE == age)
  train_metric[k] <- quantile(age_sub$SPEND_SS, .95, na.rm = TRUE)
  k <- k + 1
}
points(ages, exp(train_metric), type = 'l', col = "blue")

# Standard Deviation
ids <- unique(sim$ID)
sim_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(sim, sim$ID == id)
  sim_metric[k] <- sd(id_sub$SPENDING)
  k <- k + 1
}

ids <- unique(ss_train$HHIDPN)
train_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(ss_train, ss_train$HHIDPN == id)
  train_metric[k] <- sd(id_sub$SPEND_SS)
  k <- k + 1
}

boxplot(sim_metric)
boxplot(train_metric)

# Max Percent Increase
ids <- unique(sim$ID)
sim_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(sim, sim$ID == id)
  idl <- length(id_sub$ID)
  percent_increase <- rep(NA, idl)
  for(obs in 2:idl){
    if(obs ==1){
      percent_increase[1] <- 0
    }else{
      percent_increase[obs] <- (id_sub$SPENDING[obs] + id_sub$SPENDING[obs-1])/(id_sub$SPENDING[obs-1])
    }
  }
  sim_metric[k] <- max(percent_increase)
  k <- k + 1
}

ids <- unique(ss_train$HHIDPN)
train_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(ss_train, ss_train$HHIDPN == id)
  idl <- length(id_sub$HHIDPN)
  percent_increase <- rep(NA, idl)
  for(obs in 2:idl){
    if(obs ==1){
      percent_increase[1] <- 0
    }else{
      percent_increase[obs] <- (id_sub$SPEND_SS[obs] + id_sub$SPEND_SS[obs-1])/(id_sub$SPEND_SS[obs-1])
    }
  }
  train_metric[k] <- max(percent_increase)
  k <- k + 1
}

boxplot(log(sim_metric))
boxplot(log(train_metric))

# Avg Percent Increase
ids <- unique(sim$ID)
sim_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(sim, sim$ID == id)
  idl <- length(id_sub$ID)
  percent_increase <- rep(NA, idl)
  for(obs in 2:idl){
    if(obs ==1){
      percent_increase[1] <- 0
    }else{
      percent_increase[obs] <- (id_sub$SPENDING[obs] + id_sub$SPENDING[obs-1])/(id_sub$SPENDING[obs-1])
    }
  }
  sim_metric[k] <- mean(percent_increase)
  k <- k + 1
}

ids <- unique(ss_train$HHIDPN)
train_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(ss_train, ss_train$HHIDPN == id)
  idl <- length(id_sub$HHIDPN)
  percent_increase <- rep(NA, idl)
  for(obs in 2:idl){
    if(obs ==1){
      percent_increase[1] <- 0
    }else{
      percent_increase[obs] <- (id_sub$SPEND_SS[obs] + id_sub$SPEND_SS[obs-1])/(id_sub$SPEND_SS[obs-1])
    }
  }
  train_metric[k] <- mean(percent_increase)
  k <- k + 1
}

boxplot(log(sim_metric))
boxplot(log(train_metric))

# Average Second Differences
ids <- unique(sim$ID)
sim_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(sim, sim$ID == id)
  idl <- length(id_sub$ID)
  sum <- 0
  for(obs in 3:idl){
    sum <- sum + (id_sub$SPENDING[obs] - 2*id_sub$SPENDING[obs-1] + id_sub$SPENDING[obs-2])^2/4
  }
  sim_metric[k] <-sum
  k <- k + 1
}
mean(sim_metric)

ids <- unique(ss_train$HHIDPN)
train_metric <- rep(NA, length(ids))
k <- 1
for(id in ids){
  id_sub <- subset(ss_train, ss_train$HHIDPN == id)
  idl <- length(id_sub$HHIDPN)
  sum <- 0
  for(obs in 3:idl){
    sum <- sum + (id_sub$SPEND_SS[obs] - 2*id_sub$SPEND_SS[obs-1] + id_sub$SPEND_SS[obs-2])^2/4
  }
  train_metric[k] <-sum
  k <- k + 1
}
mean(train_metric, na.rm = TRUE)
