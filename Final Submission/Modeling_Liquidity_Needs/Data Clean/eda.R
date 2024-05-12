## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("HEART_ATTACK", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$HEART_ATTACK==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$HEART_ATTACK <- as.character(plot$HEART_ATTACK)
ggplot(plot, aes(x=AGE, y=Value, fill=HEART_ATTACK)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("STROKE", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$STROKE==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$STROKE <- as.character(plot$STROKE)
ggplot(plot, aes(x=AGE, y=Value, fill=STROKE)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("HBP", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$HBP==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$HBP <- as.character(plot$HBP)
ggplot(plot, aes(x=AGE, y=Value, fill=HBP)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("DIABETES", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$DIABETES==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$DIABETES <- as.character(plot$DIABETES)
ggplot(plot, aes(x=AGE, y=Value, fill=DIABETES)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("CANCER", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$CANCER==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$CANCER <- as.character(plot$CANCER)
ggplot(plot, aes(x=AGE, y=Value, fill=CANCER)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("LUNGS", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$LUNGS==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$LUNGS <- as.character(plot$LUNGS)
ggplot(plot, aes(x=AGE, y=Value, fill=LUNGS)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("PSYCH", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$PSYCH==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$PSYCH <- as.character(plot$PSYCH)
ggplot(plot, aes(x=AGE, y=Value, fill=PSYCH)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("ARTHRITIS", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$ARTHRITIS==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$ARTHRITIS <- as.character(plot$ARTHRITIS)
ggplot(plot, aes(x=AGE, y=Value, fill=ARTHRITIS)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("OUT_PT", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$OUT_PT==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$OUT_PT <- as.character(plot$OUT_PT)
ggplot(plot, aes(x=AGE, y=Value, fill=OUT_PT)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("DRUGS", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$DRUGS==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$DRUGS <- as.character(plot$DRUGS)
ggplot(plot, aes(x=AGE, y=Value, fill=DRUGS)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("HOME_CARE", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$HOME_CARE==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}


## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$HOME_CARE <- as.character(plot$HOME_CARE)
ggplot(plot, aes(x=AGE, y=Value, fill=HOME_CARE)) + geom_area()

## Dataset for plotting
plot <- as.data.frame(matrix(rep(NA, 234), 39*2, 3)) 
colnames(plot) <- c("SPECIAL_FAC", "AGE", "Value")

k <- 1
for(i in 65:103){
  age_sub <- subset(ss_train, ss_train$AGE==i)
  for(j in 0:1){
    var_sub <- subset(age_sub, age_sub$SPECIAL_FAC==j)
    plot[k,] <- c(j, i, mean(var_sub$SPEND_SS, trim = .1, na.rm=TRUE))
    k <- k + 1
  }
}

## Plot
plot$Value[is.na(plot$Value)] <- 0
plot$SPECIAL_FAC <- as.character(plot$SPECIAL_FAC)
ggplot(plot, aes(x=AGE, y=Value, fill=SPECIAL_FAC)) + geom_area()

plot(ss_train$DOCTOR, ss_train$SPEND_SS, main = "Doctor", xlab = "Doctor Visits", ylab = "Adjusted Spending")
plot(ss_train$HOSPITAL, ss_train$SPEND_SS, main = "Hospital", xlab = "Hospital Nights", ylab = "Adjusted Spending")
plot(ss_train$NURSING_HOME, ss_train$SPEND_SS, main = "Nursing Home", xlab = "Nursing Home Nights", ylab = "Adjusted Spending")

ss_train$AGE_RANGE = rep(NA, length(ss_train$HHIDPN))
ss_train$AGE_RANGE <- ifelse(ss_train$AGE < 75, '65-74', ss_train$AGE_RANGE)
ss_train$AGE_RANGE <- ifelse(ss_train$AGE > 74 & ss_train$AGE < 85, '75-84', ss_train$AGE_RANGE)
ss_train$AGE_RANGE <- ifelse(ss_train$AGE > 84, '85+', ss_train$AGE_RANGE)
ggplot(ss_train, aes(x = AGE_RANGE, y = SPEND_SS, fill = AGE_RANGE)) +geom_boxplot()
