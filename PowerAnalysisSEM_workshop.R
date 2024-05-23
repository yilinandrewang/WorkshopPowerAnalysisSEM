###########  Power Analysis in Structural Equation Modeling ##############
#                        Written by Y. Andre Wang                        #
#                         Last update: 24/05/23                          #

# This R script includes some operations that were used or referenced in
# the workshop on Power Analysis in Structural Equation Modeling.
# Take what is useful for you.


# Prepare Packages --------------------------------------------------------

# Name the packages needed
packages <- c("lavaan", "semTools", "simstandard", "semPower")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == F)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = T))



# Calculate expected factor loadings from scale reliability ---------------

# We use Spearman-Brown formula
rho <- .85 # scale reliability
k <- 10 # scale length (number of indicators)
lamba <- sqrt(rho/(k + (1 - k)*rho)) # factor loading
lamba



# Using RMSEA to conduct power analysis to detect model misfit ------------

# MacCallum-Browne-Sugawara approach: Calculate power
findRMSEApower(
  rmsea0 = .05, # RMSEA null (expected model fit)
  rmseaA = .01, # RMSEA alternative (RMSEA threshold tested against)
  df = 149, # model degrees of freedom
  n = 142) # sample size

# MacCallum-Browne-Sugawara approach: Calculate sample size
findRMSEAsamplesize(rmsea0 = .05, rmseaA = .01, df = 149, 
                    power = .8) # power level



# Calculate parameter values for standardized solutions -------------------

# Specify a model with factor loadings, regression coefficients, and 
# exogenuous covariance
m <- "
X =~ .67*x1 + .67*x2 + .67*x3 + .67*x4 + .67*x5
RSE =~ .60*rse1 + .60*rse2 + .60*rse3 + .60*rse4 + .60*rse5 + .60*rse6 + 
.60*rse7 + .60*rse8 + .60*rse9 + .60*rse10
Y =~ .71*y1 + .71*y2 + .71*y3 + .71*y4

Y ~ .2*X + .4*RSE
X ~~ .3*RSE
"

# Display complete model with correct (residual) variances
m_complete <- simstandard::model_complete(m)
cat(m_complete)

# Confirm the residual variace of Y with Wright's rules
1 - .2^2 - .4^2 - 2*.2*.4*.3 # Same result: 0.752



# Code template for power analysis to detect target effects ---------------

# In this template, I am using the same model described in the workshop
# Substitute with your model


# Specify population model
PopMod <- "
X =~ .67*x1 + .67*x2 + .67*x3 + .67*x4 + .67*x5
RSE =~ .60*rse1 + .60*rse2 + .60*rse3 + .60*rse4 + .60*rse5 + .60*rse6 + 
.60*rse7 + .60*rse8 + .60*rse9 + .60*rse10
Y =~ .71*y1 + .71*y2 + .71*y3 + .71*y4

Y ~ .2*X + .4*RSE
X ~~ .3*RSE

x1 ~~ 0.5511 * x1
x2 ~~ 0.5511 * x2
x3 ~~ 0.5511 * x3
x4 ~~ 0.5511 * x4
x5 ~~ 0.5511 * x5
rse1 ~~ 0.64 * rse1
rse2 ~~ 0.64 * rse2
rse3 ~~ 0.64 * rse3
rse4 ~~ 0.64 * rse4
rse5 ~~ 0.64 * rse5
rse6 ~~ 0.64 * rse6
rse7 ~~ 0.64 * rse7
rse8 ~~ 0.64 * rse8
rse9 ~~ 0.64 * rse9
rse10 ~~ 0.64 * rse10
y1 ~~ 0.4959 * y1
y2 ~~ 0.4959 * y2
y3 ~~ 0.4959 * y3
y4 ~~ 0.4959 * y4
X ~~ 1 * X
RSE ~~ 1 * RSE
Y ~~ 0.752 * Y
"


# Specify analysis model
AnMod <- "
X =~ x1 + x2 + x3 + x4 + x5
RSE =~ rse1 + rse2 + rse3 + rse4 + rse5 + rse6 + rse7 + rse8 + rse9 + rse10
Y =~ y1 + y2 + y3 + y4

Y ~ X + RSE
"

# Identify row IDs of parameters of interest
lavaanify(AnMod)
# e.g., Y ~ X is on row 20


# Set up and run Monte Carlo simulations

results <- NULL # Create a variable to save simulation results

set.seed(42) # Set simulation seed for reproducibility (seed can be any value)

for (i in 1:100) { # k = 100; replace with the number of iterations you want
  
  # Simulate and store data (sample size = 200; replace with your N)
  data <- as.data.frame(simulateData(PopMod, sample.nobs = 200)) 
  
  # Fit analysis model to data
  # (latent variables are standardized; if not, set std.lv = F)
  fit <- sem(model = AnMod, data = data, std.lv = T)
  
  # Store parameter row (here we focus on Y ~ X, which is on row 20)
  results <- rbind(results, parameterEstimates(fit)[20, ])
}

# Check estimates of the parameter in each simulated sample
head(results)


# Power estimation (alpha = 0.05; can replace .05 with desired alpha level)
mean(results$pvalue < .05) # around .68 for Y ~ X

