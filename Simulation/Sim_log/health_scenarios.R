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
