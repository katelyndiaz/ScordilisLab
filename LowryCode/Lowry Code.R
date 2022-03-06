library(dplyr)
setwd("~/Desktop/Thesis/Results")

# How to run:
# Enter your sample absorbances and the standard curve in the indicated area 
# Ctl-A --> Run


# Calculate standard curve:
x <- c(5, 
       5, 
       10, 
       10, 
       20, 
       20, 
       40, 
       40, 
       80, 
       80, 
       120, 
       120)

y <- c(0.0325, 
       0.0237, 
       0.0580, 
       0.0561, 
       0.1002, 
       0.0951, 
       0.2143, 
       0.2067, 
       0.3771, 
       0.3747, 
       0.5670, 
       0.5512)

# Alert when R^2 too low
if (summary(lm(y~x))$r.squared <= 0.95) {
  stop("$R^2$ value too low! Be aware!!")
} else{
  cat("R squared value is", summary(lm(y~x))$r.squared)
}

# Get slope and intercepts
mod_coef <- coef(lm(y~x))
intercept <- mod_coef[1]
slope <- mod_coef[2]

# Plot
plot(y ~ x, 
     xlab = "amount of BSA (mg)", 
     ylab = "absorbance", 
     main = "Lowry standard curve")
abline(a = intercept, b = slope)

# Enter your sample absorbances here, organized in (10 uL, 10 uL, 20 uL)
sample_absorbances = c(0.0435, 0.0502, 0.0563, 0.0098, 0.0106, 0.0181, 0.0343, 0.0262, 0.0460, 0.0348, 0.0328, 0.0698, 0.0387, 0.0355, 0.0710)

# This calculates the amount of protein (in mg) in each sample
amount_function <- function(absorbance) {
  amount <- (absorbance - intercept) / slope
  return(amount)
}

amount_of_protein <- amount_function(sample_absorbances)


# This calculates the concentrations of protein (in mg/mL) in each sample
concentration_function <- function(amount) {
  volumes =c(5, 5, 10)
  conc <- amount/volumes
  return(conc)
}

sample_concentrations <- concentration_function(amount_of_protein)


# This constructs a viewer-friendly table
conc_table <- matrix(data = sample_concentrations, nrow = 5, byrow = TRUE)
conc_table <- cbind(conc_table, avg = c(rowMeans(conc_table, na.rm = TRUE))) %>% as.data.frame()

View(conc_table)


# Do the following if you want to export the data:

# write.csv(conc_table, file = "C2C12 P6D0 0222.csv")
