u <- unique(ss_train$HHIDPN)
l <- length(u)
s <- sample(1:l, size = 10, replace = FALSE)
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 6), 
     xlab = "Age", ylab = "Adjusted Spending", main = paste(10, "Sampled Spend Paths"))
colors = c("firebrick1", "dodgerblue", "darkorange", "darkorchid1", "gold", "forestgreen", "deeppink", "black", "gray54", "chocolate4")
for(i in 1:10){
  sample_id <- u[s[i]]
  this_id <- subset(ss_train, ss_train$HHIDPN==sample_id)
  points(this_id$AGE, exp(this_id$SPEND_SS), col = colors[i], type = 'l', lwd = 2)
}

s <- sample(1:l, size = 1, replace = FALSE)
plot(x = 1, type = "n",
     xlim = c(65, 105), 
     ylim = c(0, 6), 
     xlab = "Age", ylab = "Adjusted Spending", main = paste(1, "Sampled Spend Paths"))
sample_id <- u[s]
this_id <- subset(ss_train, ss_train$HHIDPN==sample_id)
points(this_id$AGE, exp(this_id$SPEND_SS), col = "black", type = 'l', lwd = 4)

##############################        CREATE HEALTH PROBABILITIES         ##########################

u <- unique(ss_train$HHIDPN)
l <- length(u)
health_probs <- as.data.frame(matrix(rep(0, 39*5), 39, 5))
colnames(health_probs) <- c("Age", "S1", "S2", "S3", "S4")
for(i in 1:l){
  this_id <- subset(ss_train, ss_train$HHIDPN==u[i])
  tl <- length(this_id$HHIDPN)
  for(j in 1:tl){
    idx <- this_id$AGE[j] - 64
    health_probs$Age[idx] <- health_probs$Age[idx] + 1
    if(this_id$STROKE[j]==1 | this_id$HEART_ATTACK[j]==1 | this_id$PSYCH[j]==1){
      health_probs$S3[idx] <- health_probs$S3[idx] + 1
    }else if(this_id$HOSPITAL[j] > 0){
      health_probs$S2[idx] <- health_probs$S2[idx] + 1
    }else if(this_id$NURSING_HOME[j] > 0){
      health_probs$S4[idx] <- health_probs$S4[idx] + 1
    }else{
      health_probs$S1[idx] <- health_probs$S1[idx] + 1
    }
  }
}

health_probs$S1 <- round(health_probs$S1/health_probs$Age, 3)
health_probs$S2 <- round(health_probs$S2/health_probs$Age, 3)
health_probs$S3 <- round(health_probs$S3/health_probs$Age, 3)
health_probs$S4 <- round(health_probs$S4/health_probs$Age, 3)
health_probs$Age <- seq(65, 103, 1)

########################    HOW DO OTHER VARS CHANGE IN EACH SCENARIO    #############################

# S3

s3 <- subset(ss_train, ss_train$STROKE==1 | ss_train$HEART_ATTACK==1 | ss_train$PSYCH==1)

heart_attack <- subset(s3, s3$HEART_ATTACK==1)
heart_attack <- subset(heart_attack, !is.na(heart_attack$HHIDPN))
stroke <- subset(s3, s3$STROKE==1)
psych <- subset(s3, s3$PSYCH==1)

event_probs <- c(round(length(heart_attack$HHIDPN)/length(s3$HHIDPN), 2), 
                 round(length(stroke$HHIDPN)/length(s3$HHIDPN), 2), 
                 round(length(psych$HHIDPN)/length(s3$HHIDPN), 2))

#round(table(heart_attack$SPECIAL_FAC)/length(heart_attack$HHIDPN), 2)
#round(table(heart_attack$HOME_CARE)/length(heart_attack$HHIDPN), 2)
#round(table(heart_attack$HBP)/length(heart_attack$HHIDPN), 2)
#round(table(heart_attack$HEALTH_CHANGE)/length(heart_attack$HHIDPN), 2)
#round(table(heart_attack$DOCTOR)/length(heart_attack$HHIDPN), 3)
#round(table(heart_attack$HOSPITAL)/length(heart_attack$HHIDPN), 3)

#round(table(stroke$SPECIAL_FAC)/length(stroke$HHIDPN), 2)
#round(table(stroke$HOME_CARE)/length(stroke$HHIDPN), 2)
#round(table(stroke$HBP)/length(stroke$HHIDPN), 2)
#round(table(stroke$HEALTH_CHANGE)/length(stroke$HHIDPN), 2)
#round(table(stroke$DOCTOR)/length(stroke$HHIDPN), 3)
#round(table(stroke$HOSPITAL)/length(stroke$HHIDPN), 3)

#round(table(psych$SPECIAL_FAC)/length(psych$HHIDPN), 2)
#round(table(psych$HOME_CARE)/length(psych$HHIDPN), 2)
#round(table(psych$HBP)/length(psych$HHIDPN), 2)
#round(table(psych$HEALTH_CHANGE)/length(psych$HHIDPN), 2)
#round(table(psych$DOCTOR)/length(psych$HHIDPN), 3)
#round(table(psych$HOSPITAL)/length(psych$HHIDPN), 3)

# S1
s1 <- subset(ss_train, ss_train$HEART_ATTACK==0 & ss_train$STROKE==0 & ss_train$PSYCH==0 & ss_train$NURSING_HOME==0 & ss_train$HOSPITAL==0)

#round(table(s1$HBP)/length(s1$HHIDPN), 2)
#round(table(s1$ARTHRITIS)/length(s1$HHIDPN), 2)
#round(table(s1$DRUGS)/length(s1$DRUGS), 2)
#round(table(s1$HOME_CARE)/length(s1$HHIDPN), 2)
#round(table(s1$SPECIAL_FAC)/length(s1$HHIDPN), 2)
#round(table(s1$HEALTH_CHANGE)/length(s1$HHIDPN), 3)
#round(table(s1$DOCTOR)/length(s1$HHIDPN), 3)

# S2
s2 <- subset(ss_train, ss_train$HEART_ATTACK==0 & ss_train$STROKE==0 & ss_train$PSYCH==0 & ss_train$NURSING_HOME==0 & ss_train$HOSPITAL > 0)

#round(table(s2$HBP)/length(s2$HHIDPN), 2)
#round(table(s2$ARTHRITIS)/length(s2$HHIDPN), 2)
#round(table(s2$DRUGS)/length(s2$DRUGS), 2)
#round(table(s2$HOME_CARE)/length(s2$HHIDPN), 2)
#round(table(s2$SPECIAL_FAC)/length(s2$HHIDPN), 2)
#round(table(s2$HEALTH_CHANGE)/length(s2$HHIDPN), 3)
#round(table(s2$DOCTOR)/length(s2$HHIDPN), 3)
#round(table(s2$HOSPITAL)/length(s2$HHIDPN), 3)

# S4
s4 <- subset(ss_train, ss_train$NURSING_HOME > 0)
#round(table(s4$HBP)/length(s4$HHIDPN), 2)
#round(table(s4$ARTHRITIS)/length(s4$HHIDPN), 2)
#round(table(s4$DRUGS)/length(s4$DRUGS), 2)
#round(table(s4$HOME_CARE)/length(s4$HHIDPN), 2)
#round(table(s4$SPECIAL_FAC)/length(s4$HHIDPN), 2)
#round(table(s4$HEALTH_CHANGE)/length(s4$HHIDPN), 3)
#round(table(s4$DOCTOR)/length(s4$HHIDPN), 3)
#round(table(s4$HOSPITAL)/length(s4$HHIDPN), 3)
#round(table(s4$NURSING_HOME)/length(s4$HHIDPN), 3)

##########################         FUNCTION TO CREATE HEALTH PROFILE         ##########################  

create_health_profile <- function(yrs_obs, start_age){
  HealthVars <- c("HEALTH_CHANGE", "HBP", "HEART_ATTACK", "STROKE", "DIABETES", "CANCER", "PSYCH", "OUT_PT", 
                  "HOME_CARE", "ARTHRITIS", "SPECIAL_FAC", "DRUGS", "HOSPITAL", "DOCTOR", 
                  "NURSING_HOME")
  simulators <- c("S1", "S2", "S3", "S4")
  num_vars <- length(HealthVars)
  age <- start_age
  health_profile <- as.data.frame(matrix(rep(NA, yrs_obs*num_vars), yrs_obs, num_vars))
  colnames(health_profile) <- HealthVars
  for(i in 1:yrs_obs){
    # Retrieve probabilities 
    probs <- subset(health_probs, health_probs$Age==age)
    if(age > 103){
      probs <- subset(health_probs, health_probs$Age==103)
    }
    # Choose situation
    # 1 - No health events, no services used
    # 2 - Hospital visit (not from serious condition)
    # 3 - Serious condition (heart attack, stroke, pysch)
    # 4 - Nursing Home
    s <- sample(1:4, size = 1, replace = TRUE, prob = probs[2:5])
    # Simulate year based on situation
    health_profile[i,] <- eval(call(simulators[s]))
    age <- age + 2
  }
  health_profile$AGE <- seq(65, by=2, length.out=yrs_obs)
  return(health_profile)
}

# Plot
plot(x = 1, type = "n",
     xlim = c(65, 103), 
     ylim = c(0, 1), 
     xlab = "Age", ylab = "Adjusted Spending", main = "Scenarios vs. Age")
points(health_probs$Age, health_probs$S1, type = "l", col = "blue")
points(health_probs$Age, health_probs$S2, type = "l", col = "green")
points(health_probs$Age, health_probs$S3, type = "l", col = "orange")
points(health_probs$Age, health_probs$S4, type = "l", col = "red")

###################################         SIMULATOR FOR S3         ################################## 

S3 <- function(){
  # Set up return dataset
  HealthVars <- c("HEALTH_CHANGE", "HBP", "HEART_ATTACK", "STROKE", "DIABETES", "CANCER", "PSYCH", 
                  "OUT_PT", "HOME_CARE", "ARTHRITIS", "SPECIAL_FAC", "DRUGS", "HOSPITAL",
                  "DOCTOR", "NURSING_HOME")
  num_vars <- length(HealthVars)
  health_profile <- as.data.frame(matrix(rep(NA, 1*num_vars), 1, num_vars))
  colnames(health_profile) <- HealthVars
  # Set non significant variables to zero
  health_profile$DIABETES <- 0
  health_profile$CANCER <- 0
  health_profile$OUT_PT <- 0
  # Set variables were the probability does not change based on health issue
  health_profile$DRUGS <- 1
  health_profile$NURSING_HOME <- 0
  health_profile$ARTHRITIS <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.96, .04))
  # Choose event
  # 1 - Heart Attack
  # 2 - Stroke
  # 3 - Psych 
  e <- sample(1:3, size = 1, replace = TRUE, prob = event_probs)
  if(e==1){
    health_profile$HEART_ATTACK <- 1
    health_profile$STROKE <- 0
    health_profile$PSYCH <- 0
    # Choose other variables
    health_profile$SPECIAL_FAC <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(.75, .25))
    health_profile$HOME_CARE <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(.78, .22))
    health_profile$HBP <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.94, .06))
    health_profile$HEALTH_CHANGE <- sample(-2:3, size = 1, replace = TRUE, prob = c(.02, .1, .44, .33, .1, .02))
    health_profile$DOCTOR <- sample(c(0:12), size = 1, replace = TRUE, prob = c(.08, .04, .09, .13, .1, .08, .1, .05, .05, .05, .05, .05, .03))
    health_profile$HOSPITAL <- sample(c(0:8), size = 1, replace = TRUE, prob = c(.25, .18, .15, .12, .1, .08, .05, .05, .02))
  }else if(e==2){
    health_profile$HEART_ATTACK <- 0
    health_profile$STROKE <- 1
    health_profile$PSYCH <- 0
    # Choose other variables
    health_profile$SPECIAL_FAC <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(.7, .3))
    health_profile$HOME_CARE <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(.68, .32))
    health_profile$HBP <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.93, .07))
    health_profile$HEALTH_CHANGE <- sample(-2:4, size = 1, replace = TRUE, prob = c(.03, .12, .39, .31, .11, .03, .01))
    health_profile$DOCTOR <- sample(c(0:12), size = 1, replace = TRUE, prob = c(.08, .04, .09, .13, .1, .08, .1, .05, .05, .05, .05, .05, .03))
    health_profile$HOSPITAL <- sample(c(0:8), size = 1, replace = TRUE, prob = c(.25, .18, .15, .12, .1, .08, .05, .05, .02))
  }else{
    health_profile$HEART_ATTACK <- 0
    health_profile$STROKE <- 0
    health_profile$PSYCH <- 1
    # Choose other variables
    health_profile$SPECIAL_FAC <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(.74, .26))
    health_profile$HOME_CARE <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(.77, .23))
    health_profile$HBP <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.95, .05))
    health_profile$HEALTH_CHANGE <- sample(-2:3, size = 1, replace = TRUE, prob = c(.02, .16, .43, .27, .07, .03))
    health_profile$DOCTOR <- sample(c(0:12), size = 1, replace = TRUE, prob = c(.08, .04, .09, .13, .1, .08, .1, .05, .05, .05, .05, .05, .03))
    health_profile$HOSPITAL <- sample(c(0:8), size = 1, replace = TRUE, prob = c(.25, .18, .15, .12, .1, .08, .05, .05, .02))}
  return(health_profile)                            
  
}

###################################         SIMULATOR FOR S1         ################################## 

S1 <- function(){
  # Set up return dataset
  HealthVars <- c("HEALTH_CHANGE", "HBP", "HEART_ATTACK", "STROKE", "DIABETES", "CANCER", "PSYCH", 
                  "OUT_PT", "HOME_CARE", "ARTHRITIS", "SPECIAL_FAC", "DRUGS", "HOSPITAL",
                  "DOCTOR", "NURSING_HOME")
  num_vars <- length(HealthVars)
  health_profile <- as.data.frame(matrix(rep(NA, 1*num_vars), 1, num_vars))
  colnames(health_profile) <- HealthVars
  # Set non significant variables to zero
  health_profile$DIABETES <- 0
  health_profile$CANCER <- 0
  health_profile$OUT_PT <- 0
  # Set major health and events and services to zero
  health_profile$HEART_ATTACK <- 0
  health_profile$STROKE <- 0
  health_profile$PSYCH <- 0
  health_profile$NURSING_HOME <- 0
  health_profile$HOSPITAL <- 0
  # Set other variables
  health_profile$ARTHRITIS <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.96, .04))
  health_profile$HBP <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.96, .04))
  health_profile$DRUGS <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.1, .9))
  health_profile$HOME_CARE <- 0
  health_profile$SPECIAL_FAC <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.9, .1))
  health_profile$HEALTH_CHANGE <- sample(c(-3:4), size = 1, replace = TRUE, prob = c(.002, .021, .176, .564, .205, .03, .003, .001))
  health_profile$DOCTOR <- sample(c(0:12), size = 1, replace = TRUE, prob = c(.08, .04, .09, .13, .1, .08, .1, .05, .05, .05, .05, .05, .03))
  return(health_profile)
}


###################################         SIMULATOR FOR S2         ################################## 

S2 <- function(){
  # Set up return dataset
  HealthVars <- c("HEALTH_CHANGE", "HBP", "HEART_ATTACK", "STROKE", "DIABETES", "CANCER", "PSYCH", 
                  "OUT_PT", "HOME_CARE", "ARTHRITIS", "SPECIAL_FAC", "DRUGS", "HOSPITAL",
                  "DOCTOR", "NURSING_HOME")
  num_vars <- length(HealthVars)
  health_profile <- as.data.frame(matrix(rep(NA, 1*num_vars), 1, num_vars))
  colnames(health_profile) <- HealthVars
  # Set non significant variables to zero
  health_profile$DIABETES <- 0
  health_profile$CANCER <- 0
  health_profile$OUT_PT <- 0
  # Set major health and events and nursing home to zero
  health_profile$HEART_ATTACK <- 0
  health_profile$STROKE <- 0
  health_profile$PSYCH <- 0
  health_profile$NURSING_HOME <- 0
  # Set other variables
  health_profile$HBP <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.96, .04))
  health_profile$ARTHRITIS <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.96, .04))
  health_profile$DRUGS <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.05, .95))
  health_profile$HOME_CARE <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.8, .2))
  health_profile$SPECIAL_FAC <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.75, .25))
  health_profile$HEALTH_CHANGE <- sample(c(-3:4), size = 1, replace = TRUE, prob = c(.002, .021, .176, .564, .205, .03, .003, .001))
  health_profile$DOCTOR <- sample(c(0:12), size = 1, replace = TRUE, prob = c(.08, .04, .09, .13, .1, .08, .1, .05, .05, .05, .05, .05, .03))
  health_profile$HOSPITAL <- sample(c(0:8), size = 1, replace = TRUE, prob = c(.25, .18, .15, .12, .1, .08, .05, .05, .02))
  return(health_profile)
}

###################################         SIMULATOR FOR S4         ################################## 

S4 <- function(){
  # Set up return dataset
  HealthVars <- c("HEALTH_CHANGE", "HBP", "HEART_ATTACK", "STROKE", "DIABETES", "CANCER", "PSYCH", 
                  "OUT_PT", "HOME_CARE", "ARTHRITIS", "SPECIAL_FAC", "DRUGS", "HOSPITAL",
                  "DOCTOR", "NURSING_HOME")
  num_vars <- length(HealthVars)
  health_profile <- as.data.frame(matrix(rep(NA, 1*num_vars), 1, num_vars))
  colnames(health_profile) <- HealthVars
  # Set non significant variables to zero
  health_profile$DIABETES <- 0
  health_profile$CANCER <- 0
  health_profile$OUT_PT <- 0
  # Set major health and events and nursing home to zero
  health_profile$HEART_ATTACK <- 0
  health_profile$STROKE <- 0
  health_profile$PSYCH <- 0
  health_profile$NURSING_HOME <- 1
  # Set other variables
  health_profile$HBP <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.96, .04))
  health_profile$ARTHRITIS <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.96, .04))
  health_profile$DRUGS <- 1
  health_profile$HOME_CARE <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.5, .5))
  health_profile$SPECIAL_FAC <- sample(c(0,1), size = 1, replace = TRUE, prob = c(.55, .45))
  health_profile$HEALTH_CHANGE <- sample(c(-1:4), size = 1, replace = TRUE, prob = c(.1, .45, .25, .1, .08, .02))
  health_profile$DOCTOR <- sample(c(0:12), size = 1, replace = TRUE, prob = c(.08, .04, .09, .13, .1, .08, .1, .05, .05, .05, .05, .05, .03))
  health_profile$HOSPITAL <- sample(c(0:8), size = 1, replace = TRUE, prob = c(.25, .18, .15, .12, .1, .08, .05, .05, .02))
  health_profile$NURSING_HOME <- sample(c(0:730), size = 1, replace = TRUE)
  
  return(health_profile)
}

ages <- c()
u <- unique(ss_train$HHIDPN)
l <- length(u)
for(i in 1:l){
  this_id <- subset(ss_train, ss_train$HHIDPN==u[i])
  age <- this_id$AGE[1]
  ages <- c(ages, age)
}
write.csv(round(table(ages)/length(ages), 4), paste(pwd, "/Method 1/age_probs.csv", sep = ''))
write.csv(round(table(table(ss_train$HHIDPN))/5601, 4), paste(pwd, "/Method 1/yrs_probs.csv", sep = ''))


# Make factor variables
ss_train <- mutate(ss_train, "HBP" = as.factor(HBP)) %>%
  mutate("DIABETES" = as.factor(DIABETES)) %>%
  mutate("CANCER" = as.factor(CANCER)) %>%
  mutate("LUNGS" = as.factor(LUNGS)) %>%
  mutate("HEART_ATTACK" = as.factor(HEART_ATTACK)) %>%
  mutate("STROKE" = as.factor(STROKE)) %>%
  mutate("PSYCH" = as.factor(PSYCH)) %>%
  mutate("ARTHRITIS" = as.factor(ARTHRITIS)) %>%
  mutate("OUT_PT" = as.factor(OUT_PT)) %>%
  mutate("DRUGS" = as.factor(DRUGS)) %>%
  mutate("HOME_CARE" = as.factor(HOME_CARE)) %>%
  mutate("SPECIAL_FAC" = as.factor(SPECIAL_FAC))

ss_test <- mutate(ss_test, "HBP" = as.factor(HBP)) %>%
  mutate("DIABETES" = as.factor(DIABETES)) %>%
  mutate("CANCER" = as.factor(CANCER)) %>%
  mutate("LUNGS" = as.factor(LUNGS)) %>%
  mutate("HEART_ATTACK" = as.factor(HEART_ATTACK)) %>%
  mutate("STROKE" = as.factor(STROKE)) %>%
  mutate("PSYCH" = as.factor(PSYCH)) %>%
  mutate("ARTHRITIS" = as.factor(ARTHRITIS)) %>%
  mutate("OUT_PT" = as.factor(OUT_PT)) %>%
  mutate("DRUGS" = as.factor(DRUGS)) %>%
  mutate("HOME_CARE" = as.factor(HOME_CARE)) %>%
  mutate("SPECIAL_FAC" = as.factor(SPECIAL_FAC))

# Remove log transformation for logistic regression
ss_train$SPEND_SS = exp(ss_train$SPEND_SS)
ss_test$SPEND_SS = exp(ss_test$SPEND_SS)
ss_train$LAST_YEAR = exp(ss_train$LAST_YEAR)
ss_test$LAST_YEAR = exp(ss_test$LAST_YEAR)

# Confusion Matrix, model, test data, threshold if predicted probability >= thresh -> predict 1, otherwise predict 0
performance <- function(model, data, thresh){
  fit_vals <- predict(model,data,type='response')
  true_neg <- 0
  true_pos <- 0
  false_neg <- 0
  false_pos <- 0
  for(i in 1:length(data$SHOCK2)){
    if(fit_vals[i] >= thresh){
      if(data$SHOCK2[i]==1){
        true_pos <- true_pos + 1
      }else{
        false_pos <- false_pos + 1
      }
    }else{
      if(data$SHOCK2[i]==0){
        true_neg <- true_neg + 1
      }else{
        false_neg <- false_neg + 1
      }
    }
  }
  cm <- matrix(data=c(true_pos, false_pos, false_neg, true_neg), 2, 2, byrow = TRUE, dimnames = list("Predicted" = c("+", "-"), "Actual" = c("+", "-")))
  cat("Confusion Matrix:\n")
  print(cm)
  print((false_pos + false_neg)/length(data$HHIDPN))
}

performance2 <- function(model, data, thresh){
  fit_vals <- predict(model,data,type='response')
  true_neg <- 0
  true_pos <- 0
  false_neg <- 0
  false_pos <- 0
  for(i in 1:length(data$SHOCK2)){
    if(fit_vals[i] >= thresh){
      if(data$SHOCK2[i]==1){
        true_pos <- true_pos + 1
      }else{
        false_pos <- false_pos + 1
      }
    }else{
      if(data$SHOCK2[i]==0){
        true_neg <- true_neg + 1
      }else{
        false_neg <- false_neg + 1
      }
    }
  }
  return((true_pos + false_pos)/length(data$HHIDPN))
}

performance3 <- function(model, data, thresh){
  fit_vals <- predict(model,data,type='response')
  true_neg <- 0
  true_pos <- 0
  false_neg <- 0
  false_pos <- 0
  for(i in 1:length(data$SHOCK2)){
    if(fit_vals[i] >= thresh){
      if(data$SHOCK2[i]==1){
        true_pos <- true_pos + 1
      }else{
        false_pos <- false_pos + 1
      }
    }else{
      if(data$SHOCK2[i]==0){
        true_neg <- true_neg + 1
      }else{
        false_neg <- false_neg + 1
      }
    }
  }
  return((false_pos + false_neg)/length(data$HHIDPN))
}

t <- 1.15
# Create % increase variable - TRAIN
ss_train$percent_increase = rep(NA, length(ss_train$HHIDPN))
# Check wealth shock at each year
u <- unique(ss_train$HHIDPN)
k <- 1
for(i in u){
  this_person <- subset(ss_train, ss_train$HHIDPN==i)
  person_l <- length(this_person$HHIDPN)
  avg_person <- this_person$LAST_YEAR[1]
  for(j in 1:person_l){
    ss_train$percent_increase[k] <- ifelse(avg_person == 0, round(this_person$SPEND_SS[j], 3), round((this_person$SPEND_SS[j] - avg_person/j)/avg_person/j, 3))
    if(this_person$SPEND_SS[j] < t){
      avg_person <- avg_person + this_person$SPEND_SS[j]
    }
    k <- k + 1
  }
}

t <- 1.15
# Create % increase variable - TEST
ss_test$percent_increase = rep(NA, length(ss_test$HHIDPN))
# Check wealth shock at each year
u <- unique(ss_test$HHIDPN)
k <- 1
for(i in u){
  this_person <- subset(ss_test, ss_test$HHIDPN==i)
  person_l <- length(this_person$HHIDPN)
  avg_person <- this_person$LAST_YEAR[1]
  for(j in 1:person_l){
    ss_test$percent_increase[k] <- ifelse(avg_person == 0, round(this_person$SPEND_SS[j], 3), round((this_person$SPEND_SS[j] - avg_person/j)/avg_person/j, 3))
    if(this_person$SPEND_SS[j] < t){
      avg_person <- avg_person + this_person$SPEND_SS[j]
    }
    k <- k + 1
  }
}

t <- seq(1.1, 1.5, .05)
ter <- rep(NA, length(t))
k <- 1
for(t1 in t){
  ss_test$SHOCK2 = ifelse(ss_test$percent_increase >= t1, 1, 0)
  ss_test$SHOCK2 = ifelse(is.na(ss_test$SHOCK2), 0, ss_test$SHOCK2)
  
  # Make factor
  ss_test$SHOCK2 = as.factor(ss_test$SHOCK2)
  
  # Shock variable with 2 classes
  ss_train$SHOCK2 = ifelse(ss_train$percent_increase >= t1, 1, 0)
  ss_train$SHOCK2 = ifelse(is.na(ss_train$SHOCK2), 0, ss_train$SHOCK2)
  
  # Make factor
  ss_train$SHOCK2 = as.factor(ss_train$SHOCK2)
  
  mod <- glm(SHOCK2 ~ AGE + HEALTH_CHANGE + HBP + HEART_ATTACK + STROKE + ARTHRITIS + DRUGS + SPECIAL_FAC + HOSPITAL + DOCTOR + NURSING_HOME, family="binomial", data=ss_train)
  ter[k] <- performance2(mod, ss_train, .15)
  k <- k + 1
}
ter <- abs(ter - .06)
plot(t, ter, type = "l", xlab = "|delta_p", ylab = "Threshold", main = "Threshold and Error")

t <- seq(1.1, 1.5, .05)
ter <- rep(NA, length(t))
k <- 1
for(t1 in t){
  ss_test$SHOCK2 = ifelse(ss_test$percent_increase >= t1, 1, 0)
  ss_test$SHOCK2 = ifelse(is.na(ss_test$SHOCK2), 0, ss_test$SHOCK2)
  
  # Make factor
  ss_test$SHOCK2 = as.factor(ss_test$SHOCK2)
  
  # Shock variable with 2 classes
  ss_train$SHOCK2 = ifelse(ss_train$percent_increase >= t1, 1, 0)
  ss_train$SHOCK2 = ifelse(is.na(ss_train$SHOCK2), 0, ss_train$SHOCK2)
  
  # Make factor
  ss_train$SHOCK2 = as.factor(ss_train$SHOCK2)
  
  mod <- glm(SHOCK2 ~ AGE + HEALTH_CHANGE + HBP + HEART_ATTACK + STROKE + ARTHRITIS + DRUGS + SPECIAL_FAC + HOSPITAL + DOCTOR + NURSING_HOME, family="binomial", data=ss_train)
  ter[k] <- performance3(mod, ss_train, .15)
  k <- k + 1
}
ter <- abs(ter - .06)
plot(t, ter, type = "l", xlab = "|delta_p", ylab = "Threshold", main = "Threshold and Test Error")

t <- 1.15
# Shock variable with 2 classes
ss_test$SHOCK2 = ifelse(ss_test$percent_increase >= t, 1, 0)
ss_test$SHOCK2 = ifelse(is.na(ss_test$SHOCK2), 0, ss_test$SHOCK2)

# Make factor
ss_test$SHOCK2 = as.factor(ss_test$SHOCK2)

# Shock variable with 2 classes
ss_train$SHOCK2 = ifelse(ss_train$percent_increase >= t, 1, 0)
ss_train$SHOCK2 = ifelse(is.na(ss_train$SHOCK2), 0, ss_train$SHOCK2)

# Make factor
ss_train$SHOCK2 = as.factor(ss_train$SHOCK2)

# Logistic regression model - variables selected w/ backwards selection
mod <- glm(SHOCK2 ~ AGE + HEALTH_CHANGE + HBP + HEART_ATTACK + STROKE + ARTHRITIS + DRUGS + SPECIAL_FAC + HOSPITAL + DOCTOR + NURSING_HOME, family="binomial", data=ss_train)
summary(mod)

performance(mod, ss_train, .15)
performance(mod, ss_test, .15)

# Use predicted shock value to separate data
ss_train$PRED_SHOCK <- predict(mod,ss_train,type='response') >= .15
ss_test$PRED_SHOCK <- predict(mod,ss_test,type='response') >= .15

ss_train$SPEND_SS <- log(ss_train$SPEND_SS)
ss_test$SPEND_SS <- log(ss_test$SPEND_SS)

shock_yes <- subset(ss_train, ss_train$PRED_SHOCK==TRUE)
shock_no <- subset(ss_train, ss_train$PRED_SHOCK!=TRUE)

test_shock_yes <- subset(ss_test, ss_test$PRED_SHOCK==TRUE)
test_shock_no <- subset(ss_test, ss_test$PRED_SHOCK!=TRUE)

ss_train$SPEND_SS <- log(ss_train$SPEND_SS)
ss_test$SPEND_SS <- log(ss_test$SPEND_SS)

# Look for outliers
plot(shock_yes$AGE, shock_yes$SPEND_SS)

# Model
mod_yes <- lm(SPEND_SS ~ AGE + PSYCH + DRUGS + HOME_CARE + HOSPITAL + DOCTOR + NURSING_HOME, data = shock_yes)
summary(mod_yes)

# Residuals
plot(test_shock_yes$AGE, predict(mod_yes, test_shock_yes) - test_shock_yes$SPEND_SS)

# Test MSE
sum((predict(mod_yes, test_shock_yes) - test_shock_yes$SPEND_SS)^2, na.rm = TRUE)/921

# Look for outliers
plot(shock_no$AGE, shock_no$SPEND_SS)

# Model
mod_no <- lm(SPEND_SS ~ AGE + HEALTH_CHANGE + DRUGS + HOME_CARE + HEART_ATTACK + DOCTOR + NURSING_HOME, data = shock_no)
summary(mod_no)

# Residuals
plot(test_shock_no$AGE, predict(mod_no, test_shock_no) - test_shock_no$SPEND_SS)

# Test MSE
sum((predict(mod_no, test_shock_no) - test_shock_no$SPEND_SS)^2, na.rm = TRUE)/16591

write.csv(mod$coefficients, paste(pwd, "Method 1/log_model.csv", sep = ''))
write.csv(mod_yes$coefficients, paste(pwd, "Method 1/yes_model.csv", sep = ''))
write.csv(mod_no$coefficients, paste(pwd, "Method 1/no_model.csv", sep = ''))

# test_person: one health profile to simulate for
# baseline_spending: an estimation of adjusted spending in a non-shock year
# start_ages: age of test person at the start of the simulation
# n: number of simulations
simulation1 <- function(test_person){
  
  output_data <- as.data.frame(matrix(nrow = 1, ncol = 2))
  colnames(output_data) <- c("AGE", "SPENDING")
  l <- length(test_person$AGE)
  
  # Models
  mod1 <- read.csv(paste(pwd, "Method 1/log_model.csv", sep = ''))[,2]
  mod_yes <- read.csv(paste(pwd, "Method 1/yes_model.csv", sep = ''))[,2]
  mod_no <- read.csv(paste(pwd, "Method 1/no_model.csv", sep = ''))[,2]
  test_person <- suppressWarnings(select(test_person, -one_of("shock")))
  
  # Phase 1
  x <- select(test_person, c(AGE, HEALTH_CHANGE, HBP, HEART_ATTACK, STROKE, ARTHRITIS, DRUGS, 
                             SPECIAL_FAC, HOSPITAL, DOCTOR, NURSING_HOME))
  x <- cbind("Intercept" = rep(1, l), x)
  b <- mod1
  xb <- as.matrix(x)%*%b
  p <- 1/(1 + exp(-xb))
  shock <- p >= .16
  test_person <- cbind(test_person, shock)
  
  # Phase 2
  shock_yes <- subset(test_person, shock == TRUE)
  y <- length(shock_yes$AGE)
  x <- select(shock_yes, c(AGE, PSYCH, DRUGS, HOME_CARE, HOSPITAL, HEART_ATTACK, DOCTOR, NURSING_HOME))
  x <- cbind("Intercept" = rep(1, y), x)
  b <- mod_yes
  xb <- as.matrix(x)%*%b
  error <- rnorm(y, 0, 1.4)
  spend_yes <- exp(xb + error)
  spend_yes <- as.data.frame(cbind(shock_yes$AGE, spend_yes))
  colnames(spend_yes) <- c("AGE", "SPENDING")
  
  shock_no <- subset(test_person, shock != TRUE)
  n <- length(shock_no$AGE)
  x <- select(shock_no, c(AGE, HEALTH_CHANGE, DRUGS, HOME_CARE, HEART_ATTACK, DOCTOR, NURSING_HOME))
  x <- cbind("Intercept" = rep(1, n), x)
  b <- mod_no
  xb <- as.matrix(x)%*%b
  error <- rnorm(n, 0, 1.2)
  spend_no <- exp(xb + error)
  spend_no <- as.data.frame(cbind(shock_no$AGE, spend_no))
  colnames(spend_no) <- c("AGE", "SPENDING")
  
  # Combine
  output <- rbind(spend_yes, spend_no)
  output <- output[order(output$AGE),]
  output_data <- rbind(output_data, output)
  output_data <- output_data[-1, ]
  
  return(output_data)
}

# n: number of simulated individuals
simulation_wrapper <- function(n){
  # probabilities for start ages and years observed
  age_probs <- read.csv(paste(pwd, "Data Clean/age_probs.csv", sep = ''))[,3]
  yrs_probs <- read.csv(paste(pwd, "Data Clean/yrs_probs.csv", sep = ''))[,3]
  
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