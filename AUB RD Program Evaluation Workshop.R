#---------R Code for Program Evaluation Workshop on April 17, 2024---------#
#-----------------Code adapted from the following textbook-----------------#
# A Practical Introduction to Regression Discontinuity Designs: Extensions
# Authors: Matias D. Cattaneo, Nicolas Idrobo and Rocao Titiunik
# SOFTWARE WEBSITE: https://rdpackages.github.io/
#--------------------------------------------------------------------------#
---# RD Application Workshop instructed by Neal Jawadekar
---# Date: April 17, 2024
#--------------------------------------------------------------------------#
# TO INSTALL/DOWNLOAD R PACKAGES/FUNCTIONS:
# install.packages('foreign')
# install.packages('ggplot2')
# install.packages('rddensity')
# install.packages('rdrobust')
# install.packages('rdlocrand')
#--------------------------------------------------------------------------#

############################################################################
############################################################################
################################ Sharp RD ##################################
############################################################################
############################################################################

# Cleaning the R environment
rm(list=ls())

# Loading packages
library(foreign)
library(ggplot2)
library(rddensity)
library(rdrobust)
library(rdlocrand)

#------------------#
# Loading the data #
#------------------#
# setwd("ENTER YOUR DIRECTORY HERE")

data <- read.dta("CIT_2024_CUP_discrete.dta")
nextGPA <- data$nextGPA
X <- data$X
T <- data$T

#---------------------------------------------------------#
# Plot distribution of running variable using a histogram #
#---------------------------------------------------------#
tempdata <- as.data.frame(X); colnames(tempdata) <- c("v1");
p <- ggplot(data=tempdata, aes(v1))+
  geom_histogram(breaks=seq(-2.8, 0, by = 0.1), col="black", fill="blue", alpha = 1)+
  geom_histogram(breaks=seq(0, 1.6, by = 0.1), col="black", fill="red", alpha = 1)+
  labs(x="Score", y="Number of Observations")+geom_vline(xintercept=0, color="black")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16), 
        axis.text=element_text(size = 16))
p

#---------------------------------------------------------#
#           Run manipulation test using rddensity         #
#---------------------------------------------------------#
out <- rddensity(X, bino = FALSE)
summary(out)


#---------------------------------------------------------#
#    Run exchangeability assumption checks on covariates  #
#---------------------------------------------------------#

# HS GPA percentile
summary(rdrobust(data$hsgrade_pct, X, bwselect = "cerrd"))
rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")

# Total number of credits in year 1
summary(rdrobust(data$totcredits_year1, X, bwselect = "cerrd"))
rdplot(data$totcredits_year1, X, x.label = "Score", y.label = "", title="")

# Age
summary(rdrobust(data$age_at_entry, X, bwselect = "cerrd"))
rdplot(data$age_at_entry, X, x.label = "Score", y.label = "", title="")

# Male 
summary(rdrobust(data$male, X, bwselect = "cerrd"))
rdplot(data$male, X, x.label = "Score", y.label = "", title="")

# Place of Birth
summary(rdrobust(data$bpl_north_america, X, bwselect = "cerrd"))
rdplot(data$bpl_north_america, X, x.label = "Score", y.label = "", title="")



#---------------------------------------------------------#
#         Plot the regression discontinuity data          #
#---------------------------------------------------------#
out <- rdplot(nextGPA, X,  binselect = 'esmv', x.label = 'Score', 
              y.label = 'Outcome', title = '')
summary(out)

#---------------------------------------------------------#
#    Run regression discontinuity analysis - Sharp        #
#                (Using triangular kernel)                #
#---------------------------------------------------------#
out <- rdrobust(nextGPA, X, kernel = 'triangular',  p = 1, 
                bwselect = 'mserd')
summary(out)

#---------------------------------------------------------#
#    Run regression discontinuity analysis - Sharp        #
#         (Using clustered standard errors)               #
#---------------------------------------------------------#
clustervar <- X
out <- rdrobust(nextGPA, X, vce = 'hc0', cluster = clustervar)
summary(out)



#########################################################################
#########################################################################
############################# Fuzzy RD ##################################
#########################################################################
#########################################################################

# Cleaning the R environment
rm(list=ls())

# Loading packages
library(foreign)
library(ggplot2)
library(rddensity)
library(rdrobust)
library(rdlocrand)

#------------------#
# Loading the data #
#------------------#
# setwd("ENTER YOUR DIRECTORY HERE")

data <- read.dta("CIT_2024_CUP_fuzzy.dta")
Y <- data$Y
X1 <- data$X1
T <- data$T
D <- data$D


#---------------------------------------------------------#
#          Evaluate the compliance using 2x2 table        #
#---------------------------------------------------------#
table(T,D)


#---------------------------------------------------------#
#  Evaluate the compliance using regression discontinuity #
#---------------------------------------------------------#
out <- rdrobust(D, X1)
summary(out)

#---------------------------------------------------------#
# Plot the first-stage using Regression Discontinuity plot#
#---------------------------------------------------------#
out <- rdplot(D, X1, title = "", x.label = "Distance to SISBEN cutoff", 
              y.label = "SPP recipient")
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16), 
                           axis.text=element_text(size = 16))
plot
  
#---------------------------------------------------------#
#               RD Plot of the reduced form               #
#---------------------------------------------------------#
out <- rdplot(Y, X1, p = 3, title = "", x.label = "Distance to SISBEN cutoff", 
              y.label = "Enrollment into university")
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16), 
                           axis.text=element_text(size = 16))
plot

#---------------------------------------------------------#
#    Run regression discontinuity analysis with Sharp     #
#          (a.k.a. Intention to Treat analysis)           #
#---------------------------------------------------------#
out <- rdrobust(Y, X1)
summary(out)

#---------------------------------------------------------#
#     Run regression discontinuity analysis with Fuzzy    #
#             (a.k.a. Per Protocol analysis)              #
#---------------------------------------------------------#
out <- rdrobust(Y, X1, fuzzy = D)
summary(out)
