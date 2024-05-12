#######################      PACKAGES        ###############################

#install.packages(tidyverse)
#install.packages(priceR)
#install.packages(gtools)
#install.packages(stringi)
#install.packages(ggplot2)
#install.packages(dpylr)
#install.packages(gridExtra)

library(tidyverse)
library(priceR)
library(tidyverse)
library(gtools)
library(stringi)
library(ggplot2)
library(dplyr)
library(gridExtra)

#################      SET WORKING DIRECTORY        #########################

# To location of Modeling_Liquidity_Needs

pwd <- setwd("/Users/libraryuser/Downloads/Modeling-Retirees-Liquidity-Needs-main")


#######################      DATA CLEAN        ###############################

source(paste(pwd, "/Data Clean/data_clean.R", sep = ''))

#######################         EDA           ###############################

source(paste(pwd, "/Data Clean/eda.R", sep = ''))

#######################      METHOD 1          ###############################

source(paste(pwd, "/Method 1/method1.R", sep = ''))
source(paste(pwd, "/Method 1/eval_sim.R", sep = ''))

#######################      METHOD 2         ###############################

# In Python