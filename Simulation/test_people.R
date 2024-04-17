################################     HEALTH VARIABLES    #################################

HealthVars <- c("HEALTH_CHANGE", "HBP", "STROKE", "DIABETES", "CANCER", "PSYCH", "OUT_PT", 
                "HOME_CARE", "ARTHRITIS", "SPECIAL_FAC", "DRUGS", "HOSPITAL", "DOCTOR", 
                "NURSING_HOME")
numHealthVars <- length(HealthVars)

####################################     NO SHOCKS    ####################################
createNoShocks <- function(y){
  # Set everything to defaults
  noShocks <- as.data.frame(matrix(rep(NA, y*numHealthVars), y, numHealthVars))
  colnames(noShocks) <- HealthVars
  noShocks$HEALTH_CHANGE <- rep(0, y)
  noShocks[, 2:10] <- matrix(rep(0, y*9), y, 9)
  noShocks$DRUGS <- rep(1, y)
  noShocks$HOSPITAL <- rep(0, y)
  noShocks$DOCTOR <- rep(4, y)
  noShocks$NURSING_HOME <- rep(0, y)
  
  # Add HHIDPN
  HHIDPN <- rep("T1", y)
  noShocks <- cbind(HHIDPN, noShocks)
  return(noShocks)
}

####################################     HEART ATTACK    ####################################

createHeartAttack <- function(y){
  heartAttack <- as.data.frame(matrix(rep(NA, y*numHealthVars), y, numHealthVars))
  colnames(heartAttack) <- HealthVars
  heartAttack$HEALTH_CHANGE <- c(0, 3, 1, rep(0, y - 3))
  heartAttack$HBP <- c(0, 1, rep(0, y - 2))
  heartAttack$DIABETES <- c(0, 1, rep(0, y - 2))
  heartAttack$DRUGS <- rep(1, y)
  heartAttack$HOSPITAL <- c(0, 7, 1, rep(0, y - 3))
  heartAttack$DOCTOR <- c(4, 14, rep(8, y - 2))
  heartAttack$NURSING_HOME <- rep(0, y)
  heartAttack[, c(3, 5:10)] <- rep(0, y)
  
  # Add HHIDPN
  HHIDPN <- rep("T1", y)
  heartAttack <- cbind(HHIDPN, heartAttack)
  return(heartAttack)
}

####################################      CANCER     ####################################

createCancer <- function(y){
  cancer <- as.data.frame(matrix(rep(NA, y*numHealthVars), y, numHealthVars))
  colnames(cancer) <- HealthVars
  cancer$HEALTH_CHANGE <- c(0, 1, 2, -1, rep(0, y - 4))
  cancer$CANCER <- c(0, 1, rep(0, y - 2))
  cancer$HOSPITAL <- c(0, 6, 1, 1, rep(0, y - 4))
  cancer$NURSING_HOME <- rep(0, y)
  cancer$DOCTOR <- c(8, 16, 12, 10, rep(8, y - 4))
  cancer$DRUGS <- rep(1, y)
  cancer[,c(2:4, 6:10)] <- rep(0, y)
  
  # Add HHIDPN
  HHIDPN <- rep("T1", y)
  cancer <- cbind(HHIDPN, cancer)
  return(cancer)
}

####################################      CREATE     ####################################

noShocks <- createNoShocks(5)
heartAttack <- createHeartAttack(6)
cancer <- createCancer(5)
