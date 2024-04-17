# test_person: one health profile to simulate for
# baseline_spending: an estimation of adjusted spending in a non-shock year
# start_ages: age of test person at the start of the simulation
# n: number of simulations
simulation1 <- function(test_person, start_ages, n){
  
  output_data <- as.data.frame(matrix(nrow = 1, ncol = 3))
  colnames(output_data) <- c("ID", "AGE", "SPENDING")
  l <- length(test_person$HHIDPN)
  test_person <- select(test_person, -c("HHIDPN"))
  
  # Models
  mod1 <- read.csv("/Users/jessicarizzo/Desktop/Project/Data Clean/log_model.csv")[,2]
  mod_yes <- read.csv("/Users/jessicarizzo/Desktop/Project/Data Clean/yes_model.csv")[,2]
  mod_no <- read.csv("/Users/jessicarizzo/Desktop/Project/Data Clean/no_model.csv")[,2]
  
  for(time in 1:n){
    test_person <- suppressWarnings(select(test_person, -one_of("AGE", "shock")))
    age <- seq(start_ages[time], by = 2, length.out = l)
    test_person <- cbind("AGE" = age, test_person)
    
    # Phase 1
    x <- select(test_person, c(AGE, HEALTH_CHANGE, HBP, STROKE, ARTHRITIS, DRUGS, SPECIAL_FAC, HOSPITAL, 
                               DOCTOR, NURSING_HOME))
    x <- cbind("Intercept" = rep(1, l), x)
    b <- mod1
    xb <- as.matrix(x)%*%b
    p <- 1/(1 + exp(-xb))
    shock <- p >= .16
    test_person <- cbind(test_person, shock)
    
    # Phase 2
    shock_yes <- subset(test_person, shock == TRUE)
    y <- length(shock_yes$AGE)
    x <- select(shock_yes, c(AGE, PSYCH, DRUGS, HOME_CARE, HOSPITAL, DOCTOR, NURSING_HOME))
    x <- cbind("Intercept" = rep(1, y), x)
    b <- mod_yes
    xb <- as.matrix(x)%*%b
    error <- rnorm(y, 0, 1.4)
    spend_yes <- exp(xb + error)
    spend_yes <- as.data.frame(cbind(shock_yes$AGE, spend_yes))
    colnames(spend_yes) <- c("AGE", "SPENDING")
    
    shock_no <- subset(test_person, shock != TRUE)
    n <- length(shock_no$AGE)
    x <- select(shock_no, c(AGE, HEALTH_CHANGE, DRUGS, HOME_CARE, DOCTOR, NURSING_HOME))
    x <- cbind("Intercept" = rep(1, n), x)
    b <- mod_no
    xb <- as.matrix(x)%*%b
    error <- rnorm(n, 0, 1.2)
    spend_no <- exp(xb + error)
    spend_no <- as.data.frame(cbind(shock_no$AGE, spend_no))
    colnames(spend_no) <- c("AGE", "SPENDING")
    
    # Combine
    id <- rep(paste("T", time, sep = ""), l)
    output <- rbind(spend_yes, spend_no)
    output <- cbind("ID" = id, output)
    output <- output[order(output$AGE),]
    output_data <- rbind(output_data, output)
  }
  output_data <- output_data[-1, ]
  return(output_data)
}

simulation1(noShocks, c(65, 67), 2)
simulation1(heartAttack, 65, 4, 1)
simulation1(cancer, 67, 1)

