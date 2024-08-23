# Models
# 1. Design-based approach: SRS estimator
# a. simple random sampling

# load data
data(grdAmazonia)
summary(grdAmazonia)
grdAmazonia_1 <- grdAmazonia[,1:3]

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia_1) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia_1$AGB)

# Generate 100 samples and compute means and confidence intervals
for (i in 1:100) {
  set.seed(300+i)
  units <- sample(N, size = n, replace = FALSE)
  mysample <- grdAmazonia_1[units, ]
  
  # Sample mean
  mz <- mean(mysample$AGB)
  means[i] <- mz
  
  # Calculate confidence interval using the survey package
  mysample$pi <- n / N
  design_si <- svydesign(id = ~1, probs = ~pi, data = mysample)
  ci <- confint(svymean(~AGB, design = design_si), level = 0.95)
  
  # Save confidence interval lower and upper bounds
  cis_lower[i] <- ci[1]
  cis_upper[i] <- ci[2]
}

# Calculate bias
bias_1 <- mean(means) - true_mean

# Calculate coverage
coverage_1 <- mean(cis_lower <= true_mean & cis_upper >= true_mean)

# Save results into a data frame
results_1 <- data.frame(
  Sample = 1:100,
  Mean = means,
  CI_Lower = cis_lower,
  CI_Upper = cis_upper
)


# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_1 <- results_1 %>%
  mutate(
    bias = Mean - overall_mean_agb,
    coverage = ifelse(CI_Lower <= overall_mean_agb & CI_Upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_1 <- mean(abs(results_1$bias))
coverage_1 <- mean(results_1$coverage)

# Output results
bias_1
coverage_1
####################################
# Plot: Visualized confidence intervals
# Add sample index to results_1
results_1$Sample <- 1:nrow(results_1)

# 绘制图形
ggplot(results_1, aes(x = Sample)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.5) +
  geom_point(aes(y = Mean), color = "black") +
  geom_hline(yintercept = overall_mean_agb, color = "red") +
  coord_flip() +
  labs(x = "Sample", y = "95% interval estimate of mean") +
  theme_minimal()
########################################################
# 1. Design-based approach:; HT estimator
# b. Stratified Sampling
# HT estimator(simple random estimator)
# Load necessary libraries
library(sampling)
library(dplyr)
library(survey)

# 5,5,5,85
# Calculate the total number in each stratum
N_h <- tapply(grdAmazonia$Biome, INDEX = grdAmazonia$Biome, FUN = length)
w_h <- N_h / sum(N_h)
n <- 100

# Set a minimum sample size for each stratum
min_samples <- 5

# Calculate the initial allocation
n_h <- trunc(n * w_h)

# Identify strata with sample sizes less than the minimum sample size
low_sample_layers <- names(n_h[n_h < min_samples])

# Adjust these strata to have the minimum sample size
n_h[low_sample_layers] <- min_samples

# Recalculate the remaining sample size
remaining_samples <- n - sum(n_h)

# Adjust the strata with the maximum sample size to ensure total samples equal to 100
max_stratum <- names(n_h)[which.max(n_h)]
n_h[max_stratum] <- n_h[max_stratum] + remaining_samples

# Ensure the total sample size is 100
print(sum(n_h))  # This should print 100

# Print the final allocation
print(n_h)

ord <- unique(grdAmazonia$Biome)


# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia_1$AGB)
set.seed(314)

# Generate 100 samples and compute means and confidence intervals
for (i in 1:100) {
  set.seed(300+i)
  units <- sampling::strata(
    grdAmazonia, stratanames = "Biome", size = n_h[ord], method = "srswr")
  mysample <- getdata(grdAmazonia, units) %>%
    mutate(x1 = x1 %>% jitter(amount = 25 / 2),
           x2 = x2 %>% jitter(amount = 25 / 2))
  
  # Calculate sample mean using survey package
  labels <- sort(unique(mysample$Biome))
  lut <- data.frame(Biome = labels, weight = as.numeric(N_h) / n_h)
  mysample <- merge(x = mysample, y = lut, by = "Biome")
  
  # Ensure there are at least two samples per stratum
  if (any(table(mysample$Biome) < 5)) {
    next
  }
  
  # # View the standard error and the mean at each strutum
  # means_per_stratum <- tapply(mysample$AGB, mysample$Biome, mean)
  # means_per_stratum <- tapply(mysample$AGB, mysample$Biome, var)
  # sds_per_stratum <- tapply(mysample$AGB, mysample$Biome, sd)
  # 
  # # print the result
  # print(paste("Iteration:", i))
  # print("Means per stratum:")
  # print(means_per_stratum)
  # print("Standard deviations per stratum:")
  # print(sds_per_stratum)
  # 
  
  design_stsi <- svydesign(id = ~ 1, strata = ~ Biome, weights = ~ weight, data = mysample)
  svy <- svymean(~ AGB, design_stsi)
  
  means[i] <- coef(svy)
  
  # Calculate confidence interval using the survey package
  ci <- confint(svy, level = 0.95)
  
  # Save confidence interval lower and upper bounds
  cis_lower[i] <- ci[1, 1]
  cis_upper[i] <- ci[1, 2]
}

# Calculate bias
bias_8 <- mean(means) - true_mean

# Calculate coverage
coverage_8 <- mean(cis_lower <= true_mean & cis_upper >= true_mean)

# Save results into a data frame
results_8 <- data.frame(
  Sample = 1:100,
  Mean = means,
  CI_Lower = cis_lower,
  CI_Upper = cis_upper
)

# Print bias and coverage
print(paste("Bias:", bias_8))
print(paste("Coverage:", coverage_8))

########################################################
# 1. Design-based approach: SRS estimator
# c.  Convenience sampling in one circular area

# Define the data
data(grdAmazonia)
grdAmazonia_1 <- grdAmazonia[, 1:3]

# Total number of samples
N <- nrow(grdAmazonia_1)

# Define the radius of the circle (in meters)
radius <- 250000

# Function to calculate the distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Perform 100 iterations of sampling and compute means and confidence intervals
n_iter <- 100
n_samples <- 100
results_4 <- data.frame(mean_agb = numeric(n_iter),
                        ci_lower = numeric(n_iter),
                        ci_upper = numeric(n_iter))

for (i in 1:n_iter) {
  set.seed(300+i)
  # Randomly select a point as the center of the circle
  center_index <- sample(N, size = 1)
  center_point <- grdAmazonia_1[center_index, ]
  
  # Find all points within the circle
  distances <- calculate_distance(grdAmazonia_1$x1, grdAmazonia_1$x2, center_point$x1, center_point$x2)
  within_circle_indices <- which(distances <= radius)
  within_circle <- grdAmazonia_1[within_circle_indices, ]
  
  # Randomly select samples
  if (nrow(within_circle) >= n_samples) {
    sample_indices <- sample(nrow(within_circle), size = n_samples, replace = FALSE)
    mysample <- within_circle[sample_indices, ]
    
    # Extract AGB data
    sample_agb <- mysample$AGB
    
    # Calculate sample mean
    sample_mean <- mean(sample_agb)
    
    # Calculate sample variance
    sample_variance <- var(sample_agb)
    
    # Use SRS estimator to calculate the variance of the population mean
    variance_of_mean <- (1 - n_samples / N) * (sample_variance / n_samples)
    
    # Calculate standard error
    se <- sqrt(variance_of_mean)
    
    # Calculate confidence interval
    alpha <- 0.05
    t_value <- qt(1 - alpha / 2, df = n_samples - 1)
    ci_lower <- sample_mean - t_value * se
    ci_upper <- sample_mean + t_value * se
    
    # Save results
    results_4[i, ] <- c(sample_mean, ci_lower, ci_upper)
  } else {
    results_4[i, ] <- c(NA, NA, NA)  # Fill with NA if the sample size is insufficient
  }
}

# Remove rows containing NA values
results_4 <- na.omit(results_4)

# Calculate the overall mean of AGB
overall_mean_agb <- mean(grdAmazonia_1$AGB)

# Calculate bias and coverage
results_4 <- results_4 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_4 <- mean(abs(results_4$bias))
coverage_4 <- mean(results_4$coverage)

# Output results
bias_4
coverage_4


########################################################
# 1. Design-based approach: SRS estimator
# d.  Convenience sampling in two circular areas

# Load necessary libraries
library(dplyr)
library(leaflet)

# Load dataset
data(grdAmazonia)
grdAmazonia_1 <- grdAmazonia[, 1:3]

# Set seed
set.seed(314)

# Total number of samples in the population
N <- nrow(grdAmazonia_1)

# Define the radius (in meters)
radius <- 250000

# Function to calculate the distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Initialize results dataframe
n_iter <- 100
n_samples_1 <- 30
n_samples_2 <- 70
results <- data.frame(mean_agb = numeric(n_iter),
                      ci_lower = numeric(n_iter),
                      ci_upper = numeric(n_iter))

# Overall AGB mean
overall_mean_agb <- mean(grdAmazonia_1$AGB)

for (i in 1:n_iter) {
  # Choose the first random point as the center of the first circle
  center_index1 <- sample(N, size = 1)
  center_point1 <- grdAmazonia_1[center_index1, ]
  
  # Find all points within the first circle
  distances1 <- calculate_distance(grdAmazonia_1$x1, grdAmazonia_1$x2, center_point1$x1, center_point1$x2)
  within_circle_indices1 <- which(distances1 <= radius)
  within_circle1 <- grdAmazonia_1[within_circle_indices1, ]
  
  # Ensure the second center is not within the first circle
  center_index2 <- sample(N, size = 1)
  center_point2 <- grdAmazonia_1[center_index2, ]
  while (calculate_distance(center_point1$x1, center_point1$x2, center_point2$x1, center_point2$x2) <= radius * 2) {
    center_index2 <- sample(N, size = 1)
    center_point2 <- grdAmazonia_1[center_index2, ]
  }
  
  # Find all points within the second circle
  distances2 <- calculate_distance(grdAmazonia_1$x1, grdAmazonia_1$x2, center_point2$x1, center_point2$x2)
  within_circle_indices2 <- which(distances2 <= radius)
  within_circle2 <- grdAmazonia_1[within_circle_indices2, ]
  
  # Sample within each circle
  if (nrow(within_circle1) >= n_samples_1 && nrow(within_circle2) >= n_samples_2) {
    sample_indices1 <- sample(nrow(within_circle1), size = n_samples_1, replace = FALSE)
    sample1 <- within_circle1[sample_indices1, ]
    
    sample_indices2 <- sample(nrow(within_circle2), size = n_samples_2, replace = FALSE)
    sample2 <- within_circle2[sample_indices2, ]
    
    # Combine the samples
    mysample <- bind_rows(sample1, sample2)
    
    # Extract AGB
    sample_agb <- mysample$AGB
    
    # estimate population mean 
    pop_mean <- sample_mean <- mean(sample_agb)
    
    # sample variance
    sample_variance <- var(sample_agb)
    
    # estimate the population variance
    variance_of_mean <- (1 - n_samples / N) * (sample_variance / n_samples)
    
    # standard error
    se <- sqrt(variance_of_mean)
    
    # 95% CI
    alpha <- 0.05
    t_value <- qt(1 - alpha / 2, df = n_samples - 1)
    ci_lower <- sample_mean - t_value * se
    ci_upper <- sample_mean + t_value * se
    
    
    # Save the results
    results[i, ] <- c(sample_mean, ci_lower, ci_upper)
  } else {
    results[i, ] <- c(NA, NA, NA)  # If the sample size is insufficient, fill with NA
  }
}

# Remove rows with NA values
results <- na.omit(results)

# Calculate bias and coverage
results_12 <- results %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage rate
bias_12 <- mean(abs(results_12$bias))
coverage_12 <- mean(results_12$coverage)

# Output results
bias_12
coverage_12
######################################################
# 2. Model-assisted approach: 1)simple regression estimator
# a. SRS

# Load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100
N <- nrow(grdAmazonia)

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

# Generate 100 samples and compute means and confidence intervals
for (i in 1:100) {
  set.seed(300+i)
  mysample <- grdAmazonia %>%
    dplyr::select(AGB, lnSWIR2) %>%
    slice_sample(n = n)
  
  # Calculate sample mean for AGB (study variable)
  mz_sam <- mean(mysample$AGB)
  
  # Calculate regression coefficients using ordinary least squares manually
  # Create the design matrix X with a column of ones (intercept) and the lnSWIR2 variable
  X <- cbind(rep(1, n), mysample$lnSWIR2)
  # Calculate the inverse of the matrix product of X transpose and X
  XXinv <- solve(t(X) %*% X)
  # Calculate the matrix product of X transpose and the response variable AGB
  Xz <- t(X) %*% mysample$AGB
  # multiply XXinv with Xz, and transpose the result
  ab <- t(XXinv %*% Xz)
  
  
  # Calculate population and sample means of lnSWIR2 (covariate)
  mx_pop <- mean(grdAmazonia$lnSWIR2)
  mx_sam <- mean(mysample$lnSWIR2)
  
  # Calculate regression estimate of AGB mean
  mz_regr <- mz_sam + ab[2] * (mx_pop - mx_sam)
  means[i] <- mz_regr #save the result
  
  # Extract residuals
  e <- mysample$AGB - (ab[1] + ab[2] * mysample$lnSWIR2)
  
  # Calculate residual variance
  S2e <- var(e)
  
  # Calculate standard error
  se_mz_regr <- sqrt((1 - n / N) * S2e / n)
  
  # Calculate S^2(x) and weight g
  S2x <- sum((mysample$lnSWIR2 - mean(mysample$lnSWIR2))^2) / n
  g <- 1 + ((mx_pop - mx_sam) * (mysample$lnSWIR2 - mx_sam)) / S2x
  
  # Alternative approximate standard error
  S2ge <- sum(g^2 * e^2) / (n - 1)
  se_mz_regr_alt <- sqrt((1 - n / N) * S2ge / n)
  
  # OR Method using package: using package survey to get the mean and se straightforward
  # mysample$fpc <- N
  # design_si <- svydesign(id = ~ 1, data = mysample,  fpc = ~ fpc)
  # populationtotals <- c(N, sum(grdAmazonia$lnSWIR2))
  # mysample_cal <- calibrate(design_si, formula = ~ lnSWIR2,
  #                           population = populationtotals, calfun = "linear")
  # svymean(~ AGB, mysample_cal)
  
  # Calculate confidence intervals
  ci_lower <- mz_regr - qt(0.975, df = n - 1) * se_mz_regr_alt
  ci_upper <- mz_regr + qt(0.975, df = n - 1) * se_mz_regr_alt
  
  # Save confidence interval bounds
  cis_lower[i] <- ci_lower
  cis_upper[i] <- ci_upper
}

# Calculate bias
bias_2 <- mean(means) - true_mean

# Calculate coverage
coverage_2 <- mean(cis_lower <= true_mean & cis_upper >= true_mean)

# Save results into a data frame
results_2 <- data.frame(
  Sample = 1:100,
  Mean = means,
  CI_Lower = cis_lower,
  CI_Upper = cis_upper
)

# Print bias and coverage
print(paste("Bias:", bias_2))
print(paste("Coverage:", coverage_2))

######################################
# 2. Model-assisted approach: 2) multiple regression estimator
# a. SRS

# Load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # Sample size
N <- nrow(grdAmazonia) # Total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

# Generate 100 samples and compute means and confidence intervals
for (i in 1:100) {
  set.seed(300 + i)
  # Sampling
  mysample <- grdAmazonia %>%
    dplyr::select(AGB, lnSWIR2, Terra_PP, Prec_dm, Elevation, Clay) %>%
    slice_sample(n = n)
  
  # # View the model selection process
  # models <- regsubsets(AGB ~ ., data = mysample, nvmax = 4)
  # res_sum <- summary(models)
  # res_sum$outmat
  # res_sum$adjr2
  
  # Automatic selection of the best model
  best_model_index <- which.max(res_sum$adjr2)
  best_model_vars <- names(coef(models, best_model_index))[-1]  # 去掉截距项
  best_model_formula <- as.formula(paste("AGB ~", paste(best_model_vars, collapse = " + ")))
  # coef(models, best_model_index)
  
  # Setting up design objects
  mysample$fpc <- nrow(grdAmazonia)
  design_si <- svydesign(id = ~ 1, data = mysample, fpc = ~ fpc)
  
  # Define general information for calibration
  totals <- c(nrow(grdAmazonia))
  for (var in best_model_vars) {
    totals <- c(totals, sum(grdAmazonia[[var]]))
  }
  
  # Calibration Design
  mysample_cal <- calibrate(
    design_si, formula = best_model_formula,
    population = totals, calfun = "linear"
  )
  
  # estimate the population mean
  regr <- svymean(~ AGB, mysample_cal)
  means[i] <- coef(regr)
  
  # standard error
  se <- SE(regr)
  
  # 95% CI
  cis_lower[i] <- means[i] - qt(0.975, df = n - 1) * se
  cis_upper[i] <- means[i] + qt(0.975, df = n - 1) * se
}

# Calculate bias
bias_3 <- mean(means) - true_mean

# Calculate coverage
coverage_3 <- mean(cis_lower <= true_mean & cis_upper >= true_mean)

# Save results into a data frame
results_3 <- data.frame(
  Sample = 1:100,
  Mean = means,
  CI_Lower = cis_lower,
  CI_Upper = cis_upper
)

# Print bias and coverage
print(paste("Bias:", bias_3))
print(paste("Coverage:", coverage_3))

#################################################
# 2. Model-assisted approach: Separate estimator
# b. SS
library(dplyr)
library(survey)

# Load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100
N <- nrow(grdAmazonia)

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

# Define stratum sizes
grdAmazonia$Biome <- as.factor(grdAmazonia$Biome)
biomes <- c("Mangroves","Forest_dry","Grassland","Forest_moist")
levels(grdAmazonia$Biome) <- biomes
N_h <- table(grdAmazonia$Biome)  # Population size per stratum
n_h <- c(5, 5, 5, 85)  # Sample size per stratum

# Generate 100 samples and compute means and confidence intervals
for (i in 1:100) {
  set.seed(300 + i)
  
  # Perform stratified sampling
  units <- sampling::strata(grdAmazonia, stratanames = "Biome", 
                            size = n_h[unique(grdAmazonia$Biome)], method = "srswr")
  mysample <- getdata(grdAmazonia, units)
  
  # Separate regression estimator calculations
  mz_h_regr <- numeric(length(biomes))
  v_mz_h_regr <- numeric(length(biomes))
  b_h <- numeric(length(biomes))
  
  for (h in seq_along(biomes)) {
    subsam <- subset(mysample, Biome == biomes[h])
    lm_sample <- lm(AGB ~ lnSWIR2, data = subsam)
    b_h[h] <- coef(lm_sample)[2]
    mz_h_sam <- mean(subsam$AGB)
    mx_h_pop <- mean(grdAmazonia$lnSWIR2[grdAmazonia$Biome == biomes[h]])
    mx_h_sam <- mean(subsam$lnSWIR2)
    mz_h_regr[h] <- mz_h_sam + b_h[h] * (mx_h_pop - mx_h_sam)
    
    e <- residuals(lm_sample)
    S2e_h <- var(e)
    v_mz_h_regr[h] <- (1 - n_h[h] / N_h[h]) * S2e_h / n_h[h]
  }
  
  # Weighted average of stratum estimates
  w_h <- N_h / sum(N_h)
  mz_sepreg <- sum(w_h * mz_h_regr)
  
  # Calculate standard error of the separate regression estimator
  se_mz_sepreg <- sqrt(sum((w_h^2) * v_mz_h_regr))
  
  means[i] <- mz_sepreg
  cis_lower[i] <- mz_sepreg - qt(0.975, df = n - 1) * se_mz_sepreg
  cis_upper[i] <- mz_sepreg + qt(0.975, df = n - 1) * se_mz_sepreg
}

# Calculate bias
bias_9 <- abs(mean(means) - true_mean)

# Calculate coverage
coverage_9 <- mean(cis_lower <= true_mean & cis_upper >= true_mean)

# Save results into a data frame
results_9 <- data.frame(
  Sample = 1:100,
  Mean = means,
  CI_Lower = cis_lower,
  CI_Upper = cis_upper
)

# Print bias and coverage
print(paste("Bias:", bias_9))
print(paste("Coverage:", coverage_9))

#################################
# Print the process result of one of the iterations(Separate estimator)
i <- 1
set.seed(300 + i)

# Sampling
units <- sampling::strata(grdAmazonia, stratanames = "Biome", 
                          size = n_h[unique(grdAmazonia$Biome)], method = "srswr")
mysample <- getdata(grdAmazonia, units)

# Initializing Variables
mz_h_regr <- numeric(length(biomes))
v_mz_h_regr <- numeric(length(biomes))
b_h <- numeric(length(biomes))
b0_h <- numeric(length(biomes))


for (h in seq_along(biomes)) {
  subsam <- subset(mysample, Biome == biomes[h])
  lm_sample <- lm(AGB ~ lnSWIR2, data = subsam)
  
  # Estimated slope of the regression model
  b_h[h] <- coef(lm_sample)[2]
  b0_h[h] <- coef(lm_sample)[1]
  
  # sample mean
  mz_h_sam <- mean(subsam$AGB)
  
  # population mean and sample mean
  mx_h_pop <- mean(grdAmazonia$lnSWIR2[grdAmazonia$Biome == biomes[h]])
  mx_h_sam <- mean(subsam$lnSWIR2)
  
  # Adjusted estimated mean
  mz_h_regr[h] <- mz_h_sam + b_h[h] * (mx_h_pop - mx_h_sam)
  
  # var adn se
  e <- residuals(lm_sample)
  S2e_h <- var(e)
  
  # estimate variance
  v_mz_h_regr[h] <- (1 - n_h[h] / N_h[h]) * S2e_h / n_h[h]
  
  # output the result
  print(paste("Biome:", biomes[h]))
  print(paste("Estimated Slope beta0 (b0_h):", b0_h[h]))
  print(paste("Estimated Slope (b_h):", b_h[h]))
  print(paste("Estimated Mean (mz_h_regr):", mz_h_regr[h]))
  print(paste("Estimated Variance (v_mz_h_regr):", v_mz_h_regr[h]))
}
#################################################################
# c. Convenience sampling in one region
# 1) simple regression estimator

# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# 定义圆的半径（单位：米）
radius <- 250000

# 计算两点之间的距离函数
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# 进行100次抽样并计算均值和置信区间
n_iter <- 100
n_samples <- 100
results_5 <- data.frame(mean_agb = numeric(n_iter),
                        ci_lower = numeric(n_iter),
                        ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

radius <- 250000

for (i in 1:100) {
  set.seed(300+i)
  # randomly select a center of a circle
  center_index <- sample(N, size = 1)
  center_point <- grdAmazonia[center_index, ]
  
  # all points n the circle area
  distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point$x1, center_point$x2)
  within_circle_indices <- which(distances <= radius)
  within_circle <- grdAmazonia[within_circle_indices, ]
  
  # sampling
  if (nrow(within_circle) >= 100) {
    sample_indices <- sample(nrow(within_circle), size = 100, replace = FALSE)
    mysample <- within_circle[sample_indices, ]
    
    # finite population correction(FPC)
    mysample$fpc <- N
    
    # define the survey design
    design_si <- svydesign(id = ~1, data = mysample, fpc = ~fpc)
    
    # define the population totals for calibrate
    populationtotals <- c(N, sum(grdAmazonia$lnSWIR2))
    
    # calibrate the survey design
    mysample_cal <- calibrate(design_si, formula = ~ lnSWIR2,
                              population = populationtotals, calfun = "linear")
    # estimate the mean
    regr <- svymean(~AGB, mysample_cal)
    
    # calibrated_weights
    calibrated_weights <- weights(mysample_cal)
    
    # inclusion_probabilities
    inclusion_probabilities <- calibrated_weights / N
    
    # eastimated mean
    mz_regr <- coef(regr)
    means[i] <- mz_regr
    # estimated se
    se_mz_regr_alt <- SE(regr)
    
    # CI
    ci_lower <- mz_regr - qt(0.975, df = n - 1) * se_mz_regr_alt
    ci_upper <- mz_regr + qt(0.975, df = n - 1) * se_mz_regr_alt
    
    cis_lower[i] <- ci_lower
    cis_upper[i] <- ci_upper
    
    # results
    results_5[i, ] <- c(mz_regr, ci_lower, ci_upper)
  } else {
    results_5[i, ] <- c(NA, NA, NA)
  }
}


# delete NA
results_5 <- na.omit(results_5)

# mean for AGB
overall_mean_agb <- mean(grdAmazonia$AGB)

# bias and coverage
results_5 <- results_5 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

bias_5 <- mean(abs(results_5$bias))
coverage_5 <- mean(results_5$coverage)

bias_5
coverage_5

###############################################
# 2) multiple regression estimator
# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

radius <- 250000

# function to calculate the distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Create an empty data box to save the results
n_iter <- 100
n_samples <- 100
results_6 <- data.frame(mean_agb = numeric(n_iter),
                        ci_lower = numeric(n_iter),
                        ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

radius <- 250000

for (i in 1:100) {
  set.seed(300+i)
  # center of circle
  center_index <- sample(N, size = 1)
  center_point <- grdAmazonia[center_index, ]
  
  # Find all points within the circle
  distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point$x1, center_point$x2)
  within_circle_indices <- which(distances <= radius)
  within_circle <- grdAmazonia[within_circle_indices, ]
  
  # select samples
  if (nrow(within_circle) >= 100) {
    sample_indices <- sample(nrow(within_circle), size = 100, replace = FALSE)
    mysample <- within_circle[sample_indices, ]
    
    # # The process of model selection
    # models <- regsubsets(AGB ~ lnSWIR2 + Terra_PP + Prec_dm + Elevation + Clay, data = mysample, nvmax = 4)
    # res_sum <- summary(models)
    # res_sum$outmat
    # res_sum$adjr2
    
    
    # model selection
    best_model_index <- which.max(res_sum$adjr2)
    best_model_vars <- names(coef(models, best_model_index))[-1]  # 去掉截距项
    best_model_formula <- as.formula(paste("AGB ~", paste(best_model_vars, collapse = " + ")))
    # coef(models, best_model_index)
    
    
    # design objects
    mysample$fpc <- nrow(grdAmazonia)
    design_si <- svydesign(id = ~ 1, data = mysample, fpc = ~ fpc)
    
    # calibration
    totals <- c(nrow(grdAmazonia))
    for (var in best_model_vars) {
      totals <- c(totals, sum(grdAmazonia[[var]]))
    }
    
    mysample_cal <- calibrate(
      design_si, formula = best_model_formula,
      population = totals, calfun = "linear"
    )
    
    
    
    # Calculate the model-assisted estimate of the mean
    regr <- svymean(~ AGB, mysample_cal)
    mean_gab <- coef(regr)
    
    # Extract standard error
    se <- SE(regr)
    
    # Calculate confidence intervals
    ci_lower <- mean_gab - qt(0.975, df = n - 1) * se
    ci_upper <- mean_gab + qt(0.975, df = n - 1) * se
    
    # save results
    results_6[i, ] <- c(mean_gab, ci_lower, ci_upper)
  } else {
    results_6[i, ] <- c(NA, NA, NA)  # if samples are not enought, NA
  }
}


# delete rows with NA
results_6 <- na.omit(results_6)

# true mean
overall_mean_agb <- mean(grdAmazonia$AGB)

# compute the bias and coverage
results_6 <- results_6 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )


bias_6 <- mean(abs(results_6$bias))
coverage_6 <- mean(results_6$coverage)


bias_6
coverage_6
######################################################################
# d. Convenience sampling in two regions
# 1) Simple regression estimator

# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

radius <- 250000

# function
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# empty data frame to save the results
n_iter <- 100
n_samples <- 100
results <- data.frame(mean_agb = numeric(n_iter),
                      ci_lower = numeric(n_iter),
                      ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

radius <- 250000



for (i in 1:100) {
  set.seed(300+i)
  # Choose the first random point as the center of the first circle
  center_index1 <- sample(N, size = 1)
  center_point1 <- grdAmazonia[center_index1, ]
  
  # Find all points within the first circle
  distances1 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point1$x1, center_point1$x2)
  within_circle_indices1 <- which(distances1 <= radius)
  within_circle1 <- grdAmazonia[within_circle_indices1, ]
  
  # Ensure the second center is not within the first circle
  center_index2 <- sample(N, size = 1)
  center_point2 <- grdAmazonia[center_index2, ]
  while (calculate_distance(center_point1$x1, center_point1$x2, center_point2$x1, center_point2$x2) <= radius * 2) {
    center_index2 <- sample(N, size = 1)
    center_point2 <- grdAmazonia[center_index2, ]
  }
  
  # Find all points within the second circle
  distances2 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point2$x1, center_point2$x2)
  within_circle_indices2 <- which(distances2 <= radius)
  within_circle2 <- grdAmazonia[within_circle_indices2, ]
  
  # sampling
  if (nrow(within_circle1) >= n_samples_1 && nrow(within_circle2) >= n_samples_2) {
    sample_indices1 <- sample(nrow(within_circle1), size = n_samples_1, replace = FALSE)
    sample1 <- within_circle1[sample_indices1, ]
    
    sample_indices2 <- sample(nrow(within_circle2), size = n_samples_2, replace = FALSE)
    sample2 <- within_circle2[sample_indices2, ]
    
    # Combine the samples
    mysample <- bind_rows(sample1, sample2)
    
    # finite population correction(FPC)
    mysample$fpc <- N
    
    # define the survey design
    design_si <- svydesign(id = ~1, data = mysample, fpc = ~fpc)
    
    # define the population totals for calibrate
    populationtotals <- c(N, sum(grdAmazonia$lnSWIR2))
    
    # calibrate the survey design
    mysample_cal <- calibrate(design_si, formula = ~ lnSWIR2,
                              population = populationtotals, calfun = "linear")
    # estimate the mean
    regr <- svymean(~AGB, mysample_cal)
    
    # # calibrated_weights
    # calibrated_weights <- weights(mysample_cal)
    # 
    # # inclusion_probabilities
    # inclusion_probabilities <- calibrated_weights / N
    
    # eastimated mean
    mz_regr <- coef(regr)
    means[i] <- mz_regr
    # estimated se
    se_mz_regr_alt <- SE(regr)
    
    # CI
    ci_lower <- mz_regr - qt(0.975, df = n - 1) * se_mz_regr_alt
    ci_upper <- mz_regr + qt(0.975, df = n - 1) * se_mz_regr_alt
    
    cis_lower[i] <- ci_lower
    cis_upper[i] <- ci_upper
    
    # results
    results[i, ] <- c(mz_regr, ci_lower, ci_upper)
  } else {
    results[i, ] <- c(NA, NA, NA)
  }
}


# delete NA
results <- na.omit(results)

# mean for AGB
overall_mean_agb <- mean(grdAmazonia$AGB)

# bias and coverage
results_13 <- results %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

bias_13 <- mean(abs(results_13$bias))
coverage_13 <- mean(results_13$coverage)

bias_13
coverage_13

##########################################################
# 2) multiple regression estimator

# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

radius <- 250000

calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

n_iter <- 100
n_samples <- 100
results <- data.frame(mean_agb = numeric(n_iter),
                      ci_lower = numeric(n_iter),
                      ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

radius <- 250000
library(leaps)


for (i in 1:100) {
  set.seed(300+i)
  # Choose the first random point as the center of the first circle
  center_index1 <- sample(N, size = 1)
  center_point1 <- grdAmazonia[center_index1, ]
  
  # Find all points within the first circle
  distances1 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point1$x1, center_point1$x2)
  within_circle_indices1 <- which(distances1 <= radius)
  within_circle1 <- grdAmazonia[within_circle_indices1, ]
  
  # Ensure the second center is not within the first circle
  center_index2 <- sample(N, size = 1)
  center_point2 <- grdAmazonia[center_index2, ]
  while (calculate_distance(center_point1$x1, center_point1$x2, center_point2$x1, center_point2$x2) <= radius * 2) {
    center_index2 <- sample(N, size = 1)
    center_point2 <- grdAmazonia[center_index2, ]
  }
  
  # Find all points within the second circle
  distances2 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point2$x1, center_point2$x2)
  within_circle_indices2 <- which(distances2 <= radius)
  within_circle2 <- grdAmazonia[within_circle_indices2, ]
  
  # sampling
  if (nrow(within_circle1) >= n_samples_1 && nrow(within_circle2) >= n_samples_2) {
    sample_indices1 <- sample(nrow(within_circle1), size = n_samples_1, replace = FALSE)
    sample1 <- within_circle1[sample_indices1, ]
    
    sample_indices2 <- sample(nrow(within_circle2), size = n_samples_2, replace = FALSE)
    sample2 <- within_circle2[sample_indices2, ]
    
    # Combine the samples
    mysample <- bind_rows(sample1, sample2)
    
    # # model selection process
    # models <- regsubsets(AGB ~ lnSWIR2 + Terra_PP + Prec_dm + Elevation + Clay, data = mysample, nvmax = 4)
    # res_sum <- summary(models)
    # res_sum$outmat
    # res_sum$adjr2
    
    # model selecition
    best_model_index <- which.max(res_sum$adjr2)
    best_model_vars <- names(coef(models, best_model_index))[-1]  # 去掉截距项
    best_model_formula <- as.formula(paste("AGB ~", paste(best_model_vars, collapse = " + ")))
    # coef(models, best_model_index)
    
    
    # design
    mysample$fpc <- nrow(grdAmazonia)
    design_si <- svydesign(id = ~ 1, data = mysample, fpc = ~ fpc)
    
    # calibrations
    totals <- c(nrow(grdAmazonia))
    for (var in best_model_vars) {
      totals <- c(totals, sum(grdAmazonia[[var]]))
    }
    
    mysample_cal <- calibrate(
      design_si, formula = best_model_formula,
      population = totals, calfun = "linear"
    )
    
    
    
    # Calculate the model-assisted estimate of the mean
    regr <- svymean(~ AGB, mysample_cal)
    mean_gab <- coef(regr)
    
    # Extract standard error
    se <- SE(regr)
    
    # Calculate confidence intervals
    ci_lower <- mean_gab - qt(0.975, df = n - 1) * se
    ci_upper <- mean_gab + qt(0.975, df = n - 1) * se
    
    
    # results
    results[i, ] <- c(mean_gab, ci_lower, ci_upper)
  } else {
    results[i, ] <- c(NA, NA, NA)
  }
}


# delete NA
results <- na.omit(results)

# mean for AGB
overall_mean_agb <- mean(grdAmazonia$AGB)

# bias and coverage
results_13_2 <- results %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

bias_13_2 <- mean(abs(results_13_2$bias))
coverage_13_2 <- mean(results_13_2$coverage)

bias_13_2
coverage_13_2

######################################################
# 3. Model-based approach
# 3.1 Simple linear regression model
# Check whether the effect of categorical variables on the model is significant, using convenience samples


# Load data
data(grdAmazonia)

# Compute the logarithm of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))


# Transform multiple categorical variables into factor variables
grdAmazonia <- grdAmazonia %>%
  mutate(Biome = as.factor(Biome),
         Ecoregion = as.factor(Ecoregion))

# Get the levels of Ecoregion_f
biome_levels <- levels(grdAmazonia$Biome)
ecoregion_levels <- levels(grdAmazonia$Ecoregion)

# Set random seed
set.seed(314)

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

radius <- 250000

calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}
radius <- 250000

# Sampling
center_index <- sample(N, size = 1)
center_point <- grdAmazonia[center_index, ]
# all points n the circle area
distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point$x1, center_point$x2)
within_circle_indices <- which(distances <= radius)
within_circle <- grdAmazonia[within_circle_indices, ]
sample_indices <- sample(nrow(within_circle), size = 100, replace = FALSE)
mysample <- within_circle[sample_indices, ]


# the sample size of categorical variables at each level
table(mysample$Biome) # result: all in the same stratum,it can be just ignored in the model
table(mysample$Ecoregion) # to check the importance of it in model

# Drop the Biome
mysample <- mysample %>% select(-Biome)

# To check if the ecoregion effective significant to the model
# Build full model
full_model <- lm(AGB ~ ., data = mysample)

# Build reduced model without Ecoregion_f
reduced_model <- lm(AGB ~ . - Ecoregion, data = mysample)

# Compare models using ANOVA
model_comparison <- anova(reduced_model, full_model)
print(model_comparison)

######################################################
# a. SRS
# Load data
data(grdAmazonia)

# Compute the logarithm of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Set random seed
set.seed(314)

# Define parameters
n <- 100 # Sample size
N <- nrow(grdAmazonia) # Total population size
n_iter <- 100 # Number of iterations

# Initialize the results dataframe
results <- data.frame(mean_agb = numeric(n_iter),
                      ci_lower = numeric(n_iter),
                      ci_upper = numeric(n_iter))

true_mean <- mean(grdAmazonia$AGB)

for (i in 1:n_iter) {
  # Randomly select a point as the center
  set.seed(300+i)
  units <- sample(N, size = n, replace = FALSE)
  mysample <- grdAmazonia[units, ]
  
  # Fit the initial linear regression model
  initial_model_1 <- lm(AGB ~ lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay , data = mysample)
  # linear regression model with interaction
  # initial_model_2 <- lm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2 , data = mysample)
  
  # Perform stepwise regression using the step() function
  # if use the model with interaction, change to initial_model_2
  final_model <- step(initial_model_1, direction = "backward", trace = FALSE)
  
  # summary(final_model)
  # plot(final_model)
  
  # # Variogram
  # gdata<- list(data= final_model$residuals, coords=cbind(mysample$x1, mysample$x2))
  # 
  # set.seed(4)
  # cutoff=abs(5000000-6500000)/2
  # bin <- variog(gdata, max.dist=cutoff)
  # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
  # par(mfrow=c(1,1))
  # plot(bin,envelope=binenv)
  
  # Use the final model for prediction
  predictions <- predict(final_model, newdata = grdAmazonia, se.fit = TRUE)
  # Predicted value and standard error
  fit <- predictions$fit
  estimated_mean = mean(fit)
  
  # calculat the estimated standard error
  # design matrix
  X_new <- model.matrix(final_model, data = grdAmazonia)
  # covariance matrix
  cov_matrix <- vcov(final_model)
  # Compute the total se of the predicted mean value
  N <- nrow(grdAmazonia)
  a <- matrix(rep(1/N, N), ncol=1)
  aa= t(a) %*% X_new
  final_var <- aa %*% (cov_matrix %*% t(aa))
  final_se <- sqrt(final_var)
  
  # CI
  alpha <- 0.05
  t_value <- qt(1 - alpha / 2, df = final_model$df.residual)
  ci_lower <- estimated_mean - t_value*final_se
  ci_upper <- estimated_mean + t_value*final_se
  
  # Save results
  results$mean_agb[i] <- estimated_mean
  results$ci_lower[i] <- ci_lower
  results$ci_upper[i] <- ci_upper
}

# Calculate overall mean AGB
overall_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_7 <- results %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_7 <- mean(abs(results_7$bias))
coverage_7 <- mean(results_7$coverage)

# Output results
bias_7
coverage_7
#########################################################################
# b. SS

# Load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100
N <- nrow(grdAmazonia)

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

# Initialize the results dataframe
results <- data.frame(mean_agb = numeric(n_iter),
                      ci_lower = numeric(n_iter),
                      ci_upper = numeric(n_iter))

# Define stratum sizes
grdAmazonia$Biome <- as.factor(grdAmazonia$Biome)
biomes <- c("Mangroves","Forest_dry","Grassland","Forest_moist")
levels(grdAmazonia$Biome) <- biomes
N_h <- table(grdAmazonia$Biome)  # Population size per stratum
n_h <- c(5, 5, 5, 85)  # Sample size per stratum

# Generate 100 samples and compute means and confidence intervals
for (i in 1:100) {
  set.seed(300 + i)
  
  # Perform stratified sampling
  units <- sampling::strata(grdAmazonia, stratanames = "Biome", 
                            size = n_h[unique(grdAmazonia$Biome)], method = "srswr")
  mysample <- getdata(grdAmazonia, units)
  
  # fit the LM model
  initial_model_1 <- lm(AGB ~ lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay , data = mysample)
  # Model with interaction
  #initial_model_2 <- lm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2 , data = mysample)
  
  # Perform stepwise regression using the step() function
  # if use the model with interaction, change to initial_model_2
  final_model <- step(initial_model_2, direction = "backward", trace = FALSE)
  
  
  # summary(final_model)
  # plot(final_model)
  
  # # Variogram
  # gdata<- list(data= final_model$residuals, coords=cbind(mysample$x1, mysample$x2))
  # set.seed(4)
  # cutoff=abs(5000000-6500000)/2
  # bin <- variog(gdata, max.dist=cutoff)
  # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
  # par(mfrow=c(1,1))
  # plot(bin,envelope=binenv)
  
  # Use the final model for prediction
  predictions <- predict(final_model, newdata = grdAmazonia, se.fit = TRUE)
  # Predicted value and standard error
  fit <- predictions$fit
  estimated_mean = mean(fit)
  
  # design matrix
  X_new <- model.matrix(final_model, data = grdAmazonia)
  # covariance matrix
  cov_matrix <- vcov(final_model)
  # Compute the total se of the predicted mean value
  N <- nrow(grdAmazonia)
  a <- matrix(rep(1/N, N), ncol=1)
  aa= t(a) %*% X_new
  final_var <- aa %*% (cov_matrix %*% t(aa))
  final_se <- sqrt(final_var)
  
  # CI
  alpha <- 0.05
  t_value <- qt(1 - alpha / 2, df = final_model$df.residual)
  ci_lower <- estimated_mean - t_value*final_se
  ci_upper <- estimated_mean + t_value*final_se
  
  # Save results
  results$mean_agb[i] <- estimated_mean
  results$ci_lower[i] <- ci_lower
  results$ci_upper[i] <- ci_upper
}

# Calculate overall mean AGB
overall_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_7_4 <- results %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_7_4 <- mean(abs(results_7_4$bias))
coverage_7_4 <- mean(results_7_4$coverage)

# Output results
bias_7_4
coverage_7_4
######################################################################
# c. Convenience sampling in one region

# Load data
data(grdAmazonia)

# Compute the logarithm of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # Sample size
N <- nrow(grdAmazonia) # Total population size
n_iter <- 100 # Number of iterations

# Define the radius of the circle (in meters)
radius <- 250000

# Function to calculate distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Initialize the results dataframe
results7_5 <- data.frame(mean_agb = numeric(n_iter),
                         ci_lower = numeric(n_iter),
                         ci_upper = numeric(n_iter))

true_mean <- mean(grdAmazonia$AGB)

for (i in 1:n_iter) {
  # Randomly select a point as the center
  set.seed(300+i)
  center_index <- sample(N, size = 1)
  center_point <- grdAmazonia[center_index, ]
  
  # Find all points within the circular range
  distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point$x1, center_point$x2)
  within_circle_indices <- which(distances <= radius)
  within_circle <- grdAmazonia[within_circle_indices, ]
  
  # Randomly select samples
  sample_indices <- sample(nrow(within_circle), size = n, replace = FALSE)
  mysample <- within_circle[sample_indices, ]
  
  # Fit the initial linear regression model
  initial_model_1 <- lm(AGB ~ lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay , data = mysample)
  # linear regression model with interaction
  # initial_model_2 <- lm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2 , data = mysample)
  
  # Perform stepwise regression using the step() function
  # if use the model with interaction, change to initial_model_2
  final_model <- step(initial_model_1, direction = "backward", trace = FALSE)
  
  # summary(final_model)
  # plot(final_model)
  
  # # Variogram
  # gdata<- list(data= final_model$residuals, coords=cbind(mysample$x1, mysample$x2))
  # 
  # set.seed(4)
  # cutoff=abs(5000000-6500000)/2
  # bin <- variog(gdata, max.dist=cutoff)
  # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
  # par(mfrow=c(1,1))
  # plot(bin,envelope=binenv)
  
  # Use the final model for prediction
  predictions <- predict(final_model, newdata = grdAmazonia, se.fit = TRUE)
  # Predicted value and standard error
  fit <- predictions$fit
  estimated_mean = mean(fit)
  
  # calculat the estimated standard error
  # design matrix
  X_new <- model.matrix(final_model, data = grdAmazonia)
  # covariance matrix
  cov_matrix <- vcov(final_model)
  # Compute the total se of the predicted mean value
  N <- nrow(grdAmazonia)
  a <- matrix(rep(1/N, N), ncol=1)
  aa= t(a) %*% X_new
  final_var <- aa %*% (cov_matrix %*% t(aa))
  final_se <- sqrt(final_var)
  
  # CI
  alpha <- 0.05
  t_value <- qt(1 - alpha / 2, df = final_model$df.residual)
  ci_lower <- estimated_mean - t_value*final_se
  ci_upper <- estimated_mean + t_value*final_se
  
  # Save results
  results7_5$mean_agb[i] <- estimated_mean
  results7_5$ci_lower[i] <- ci_lower
  results7_5$ci_upper[i] <- ci_upper
}

# Calculate overall mean AGB
overall_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_7_5 <- results7_5 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_7_5 <- mean(abs(results_7_5$bias))
coverage_7_5 <- mean(results_7_5$coverage)
#plot(final_model,x=mysample$Elevation)
# Output results
bias_7_5
coverage_7_5

############################################################
# d. Convenience sampling in two regions
# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# 定义圆的半径（单位：米）
radius <- 250000

# 计算两点之间的距离函数
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# 进行100次抽样并计算均值和置信区间
n_iter <- 100
n_samples <- 100
results <- data.frame(mean_agb = numeric(n_iter),
                      ci_lower = numeric(n_iter),
                      ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

radius <- 250000


for (i in 1:n_iter) {
  # Choose the first random point as the center of the first circle
  center_index1 <- sample(N, size = 1)
  center_point1 <- grdAmazonia[center_index1, ]
  
  # Find all points within the first circle
  distances1 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point1$x1, center_point1$x2)
  within_circle_indices1 <- which(distances1 <= radius)
  within_circle1 <- grdAmazonia[within_circle_indices1, ]
  
  # Ensure the second center is not within the first circle
  center_index2 <- sample(N, size = 1)
  center_point2 <- grdAmazonia[center_index2, ]
  while (calculate_distance(center_point1$x1, center_point1$x2, center_point2$x1, center_point2$x2) <= radius * 2) {
    center_index2 <- sample(N, size = 1)
    center_point2 <- grdAmazonia[center_index2, ]
  }
  
  # Find all points within the second circle
  distances2 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point2$x1, center_point2$x2)
  within_circle_indices2 <- which(distances2 <= radius)
  within_circle2 <- grdAmazonia[within_circle_indices2, ]
  
  
  if (nrow(within_circle1) >= n_samples_1 && nrow(within_circle2) >= n_samples_2) {
    sample_indices1 <- sample(nrow(within_circle1), size = n_samples_1, replace = FALSE)
    sample1 <- within_circle1[sample_indices1, ]
    
    sample_indices2 <- sample(nrow(within_circle2), size = n_samples_2, replace = FALSE)
    sample2 <- within_circle2[sample_indices2, ]
    
    # Combine the samples
    mysample <- bind_rows(sample1, sample2)
    
    # Fit the initial linear regression model
    initial_model_1 <- lm(AGB ~ lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay , data = mysample)
    # linear regression model with interaction
    # initial_model_2 <- lm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2 , data = mysample)
    
    # Perform stepwise regression using the step() function
    # if use the model with interaction, change to initial_model_2
    final_model <- step(initial_model_1, direction = "backward", trace = FALSE)
    
    # summary(final_model)
    # plot(final_model)
    
    # # Variogram
    # gdata<- list(data= final_model$residuals, coords=cbind(mysample$x1, mysample$x2))
    # 
    # set.seed(4)
    # cutoff=abs(5000000-6500000)/2
    # bin <- variog(gdata, max.dist=cutoff)
    # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
    # par(mfrow=c(1,1))
    # plot(bin,envelope=binenv)
    
    # Use the final model for prediction
    predictions <- predict(final_model, newdata = grdAmazonia, se.fit = TRUE)
    # Predicted value and standard error
    fit <- predictions$fit
    estimated_mean = mean(fit)
    
    # calculat the estimated standard error
    # design matrix
    X_new <- model.matrix(final_model, data = grdAmazonia)
    # covariance matrix
    cov_matrix <- vcov(final_model)
    # Compute the total se of the predicted mean value
    N <- nrow(grdAmazonia)
    a <- matrix(rep(1/N, N), ncol=1)
    aa= t(a) %*% X_new
    final_var <- aa %*% (cov_matrix %*% t(aa))
    final_se <- sqrt(final_var)
    
    # CI
    alpha <- 0.05
    t_value <- qt(1 - alpha / 2, df = final_model$df.residual)
    ci_lower <- estimated_mean - t_value*final_se
    ci_upper <- estimated_mean + t_value*final_se
    
    # Save results
    results$mean_agb[i] <- estimated_mean
    results$ci_lower[i] <- ci_lower
    results$ci_upper[i] <- ci_upper
  } else {
    results[i, ] <- c(NA, NA, NA)
  }
}

# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_14 <- results %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_14 <- mean(abs(results_14$bias))
coverage_14 <- mean(results_14$coverage)
#plot(final_model,x=mysample$Elevation)
# Output results
bias_14
coverage_14

########################################################
# 3.2 Generalized regression model
# a. SRS

# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# data frame to save results
n_iter <- 100
n_samples <- 100
results30_2 <- data.frame(mean_agb = numeric(n_iter),
                          ci_lower = numeric(n_iter),
                          ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

for (i in 1:n_iter) {
  set.seed(300+i)
  units <- sample(N, size = n, replace = FALSE)
  mysample <- grdAmazonia[units, ]
  
  # GLM (Gamma(log))
  initial_model_7 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay), data = mysample,family = Gamma(link='log'))
  #GLM with interaction
  # initial_model_7_2 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2, data = mysample,family = Gamma(link='log'))
  
  # model selection 
  # change to "initial_model _7_2" if want to optimize GLM with interaction
  final_glm_model <- step(initial_model_7, direction = "backward", trace = FALSE)
  
  # Extract the formula from final_glm_model
  final_formula <- formula(final_glm_model)
  
  # Build the GAM model with the selected formula
  gam_model <- gam(final_formula, data = mysample, family = Gamma(link='log'))
  
  # summary(gam_model)
  # par(mfrow = c(2,2))
  # plot(final_glm_model)
  # gam.check(gam_model)
  
  # Variogram
  # gdata<- list(data= gam_model$residuals, coords=cbind(mysample$x1, mysample$x2))
  # par(mfrow=c(1,1))
  # set.seed(4)
  # cutoff=abs(5000000-6500000)/2
  # bin <- variog(gdata, max.dist=cutoff)
  # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
  # plot(bin,envelope=binenv)
  
  # Use the final model for prediction
  predictions <- predict.gam(gam_model, newdata = grdAmazonia, se.fit = TRUE, type='response')
  predict_mean <- mean(predictions$fit)
  
  # Standard error: Simulation method
  # design matrix
  X_new <- predict(gam_model, newdata = grdAmazonia, type = "lpmatrix")
  # Simulation Parameter Set
  set.seed(8+i)
  br <- mvtnorm::rmvnorm(n = 1000, mean = coef(gam_model), sigma = vcov(gam_model))
  # Compute linear predictive values and apply nonlinear transformations
  mean_AGB <- numeric(1000)
  for (j in 1:1000) {
    lp <- X_new %*% br[j, ]
    mean_AGB[j] <- mean(exp(lp))
  }
  
  # Calculate the confidence interval
  alpha <- 0.05  # for a 95% confidence interval
  ci_lower <- quantile(mean_AGB, probs = alpha / 2)
  ci_upper <- quantile(mean_AGB, probs = 1 - alpha / 2)
  
  # Save results
  results30_2$mean_agb[i] <- predict_mean
  results30_2$ci_lower[i] <- ci_lower
  results30_2$ci_upper[i] <- ci_upper
}

# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_30_2 <- results30_2 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_30_2 <- mean(abs(results_30_2$bias))
coverage_30_2 <- mean(results_30_2$coverage)
#plot(final_model,x=mysample$Elevation)
# Output results
bias_30_2
coverage_30_2

########################################################################
# b. SS
# SS data with GLM

# Load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100
N <- nrow(grdAmazonia)

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

# Initialize the results dataframe
results31_2 <- data.frame(mean_agb = numeric(n_iter),
                          ci_lower = numeric(n_iter),
                          ci_upper = numeric(n_iter))

# Define stratum sizes
grdAmazonia$Biome <- as.factor(grdAmazonia$Biome)
biomes <- c("Mangroves","Forest_dry","Grassland","Forest_moist")
levels(grdAmazonia$Biome) <- biomes
N_h <- table(grdAmazonia$Biome)  # Population size per stratum
n_h <- c(5, 5, 5, 85)  # Sample size per stratum

for (i in 1:n_iter) {
  set.seed(300+i)
  # Perform stratified sampling
  units <- sampling::strata(grdAmazonia, stratanames = "Biome", 
                            size = n_h[unique(grdAmazonia$Biome)], method = "srswr")
  mysample <- getdata(grdAmazonia, units)
  
  # GLM (Gamma(log))
  initial_model_7 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay), data = mysample,family = Gamma(link='log'))
  #GLM with interaction
  # initial_model_7_2 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2, data = mysample,family = Gamma(link='log'))
  
  # model selection 
  # change to "initial_model _7_2" if want to optimize GLM with interaction
  final_glm_model <- step(initial_model_7, direction = "backward", trace = FALSE)
  
  # Extract the formula from final_glm_model
  final_formula <- formula(final_glm_model)
  
  # Build the GAM model with the selected formula
  gam_model <- gam(final_formula, data = mysample, family = Gamma(link='log'))
  
  # summary(gam_model)
  # par(mfrow = c(2,2))
  # plot(final_glm_model)
  # gam.check(gam_model)
  
  # Variogram
  # gdata<- list(data= gam_model$residuals, coords=cbind(mysample$x1, mysample$x2))
  # par(mfrow=c(1,1))
  # set.seed(4)
  # cutoff=abs(5000000-6500000)/2
  # bin <- variog(gdata, max.dist=cutoff)
  # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
  # plot(bin,envelope=binenv)
  
  # Use the final model for prediction
  predictions <- predict.gam(gam_model, newdata = grdAmazonia, se.fit = TRUE, type='response')
  predict_mean <- mean(predictions$fit)
  
  # Standard error: Simulation method
  # design matrix
  X_new <- predict(gam_model, newdata = grdAmazonia, type = "lpmatrix")
  # Simulation Parameter Set
  set.seed(8+i)
  br <- mvtnorm::rmvnorm(n = 1000, mean = coef(gam_model), sigma = vcov(gam_model))
  # Compute linear predictive values and apply nonlinear transformations
  mean_AGB <- numeric(1000)
  for (j in 1:1000) {
    lp <- X_new %*% br[j, ]
    mean_AGB[j] <- mean(exp(lp))
  }
  
  # Calculate the confidence interval
  alpha <- 0.05  # for a 95% confidence interval
  ci_lower <- quantile(mean_AGB, probs = alpha / 2)
  ci_upper <- quantile(mean_AGB, probs = 1 - alpha / 2)
  
  # Save results
  results30_2$mean_agb[i] <- predict_mean
  results30_2$ci_lower[i] <- ci_lower
  results30_2$ci_upper[i] <- ci_upper
}

# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_31_2 <- results31_2 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_31_2 <- mean(abs(results_31_2$bias))
coverage_31_2 <- mean(results_31_2$coverage)
#plot(final_model,x=mysample$Elevation)
# Output results
bias_31_2
coverage_31_2

#####################################################################
# c. Convenience sampling in one region
# Load data
data(grdAmazonia)

# Compute the logarithm of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # Sample size
N <- nrow(grdAmazonia) # Total population size
n_iter <- 100 # Number of iterations

# Define the radius of the circle (in meters)
radius <- 250000

# Function to calculate distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Initialize the results dataframe
results32 <- data.frame(mean_agb = numeric(n_iter),
                        ci_lower = numeric(n_iter),
                        ci_upper = numeric(n_iter))

# Initialize the results dataframe
results42 <- data.frame(mean_agb = numeric(n_iter),
                        ci_lower = numeric(n_iter),
                        ci_upper = numeric(n_iter))

true_mean <- mean(grdAmazonia$AGB)


for (i in 1:n_iter) {
  # Randomly select a point as the center
  set.seed(300+i)
  center_index <- sample(N, size = 1)
  center_point <- grdAmazonia[center_index, ]
  
  # Find all points within the circular range
  distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point$x1, center_point$x2)
  within_circle_indices <- which(distances <= radius)
  within_circle <- grdAmazonia[within_circle_indices, ]
  
  # Randomly select samples
  sample_indices <- sample(nrow(within_circle), size = n, replace = FALSE)
  mysample <- within_circle[sample_indices, ]
  
  # GLM (Gamma(log))
  initial_model_7 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay), data = mysample,family = Gamma(link='log'))
  #GLM with interaction
  # initial_model_7_2 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2, data = mysample,family = Gamma(link='log'))
  
  # model selection 
  # change to "initial_model _7_2" if want to optimize GLM with interaction
  final_glm_model <- step(initial_model_7, direction = "backward", trace = FALSE)
  
  # Extract the formula from final_glm_model
  final_formula <- formula(final_glm_model)
  
  # Build the GAM model with the selected formula
  gam_model <- gam(final_formula, data = mysample, family = Gamma(link='log'))
  
  # summary(gam_model)
  # par(mfrow = c(2,2))
  # plot(final_glm_model)
  # gam.check(gam_model)
  
  # Variogram
  # gdata<- list(data= gam_model$residuals, coords=cbind(mysample$x1, mysample$x2))
  # par(mfrow=c(1,1))
  # set.seed(4)
  # cutoff=abs(5000000-6500000)/2
  # bin <- variog(gdata, max.dist=cutoff)
  # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
  # plot(bin,envelope=binenv)
  
  # Use the final model for prediction
  predictions <- predict.gam(gam_model, newdata = grdAmazonia, se.fit = TRUE, type='response')
  predict_mean <- mean(predictions$fit)
  
  # Standard error: Simulation method
  # design matrix
  X_new <- predict(gam_model, newdata = grdAmazonia, type = "lpmatrix")
  # Simulation Parameter Set
  set.seed(8+i)
  br <- mvtnorm::rmvnorm(n = 1000, mean = coef(gam_model), sigma = vcov(gam_model))
  # Compute linear predictive values and apply nonlinear transformations
  mean_AGB <- numeric(1000)
  for (j in 1:1000) {
    lp <- X_new %*% br[j, ]
    mean_AGB[j] <- mean(exp(lp))
  }
  
  # Calculate the confidence interval
  alpha <- 0.05  # for a 95% confidence interval
  ci_lower <- quantile(mean_AGB, probs = alpha / 2)
  ci_upper <- quantile(mean_AGB, probs = 1 - alpha / 2)
  
  # Save results
  results42$mean_agb[i] <- predict_mean
  results42$ci_lower[i] <- ci_lower
  results42$ci_upper[i] <- ci_upper
}

# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_42 <- results42 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_42 <- mean(abs(results_42$bias))
coverage_42 <- mean(results_42$coverage)
#plot(final_model,x=mysample$Elevation)
# Output results
bias_42
coverage_42
##############################################################
# d. Convenience sampling in two regions
# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

radius <- 250000

calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

n_iter <- 100
n_samples <- 100
results21 <- data.frame(mean_agb = numeric(n_iter),
                        ci_lower = numeric(n_iter),
                        ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

radius <- 250000
i=20
for (i in 1:n_iter) {
  set.seed(300+i)
  # Choose the first random point as the center of the first circle
  center_index1 <- sample(N, size = 1)
  center_point1 <- grdAmazonia[center_index1, ]
  
  # Find all points within the first circle
  distances1 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point1$x1, center_point1$x2)
  within_circle_indices1 <- which(distances1 <= radius)
  within_circle1 <- grdAmazonia[within_circle_indices1, ]
  
  # Ensure the second center is not within the first circle
  center_index2 <- sample(N, size = 1)
  center_point2 <- grdAmazonia[center_index2, ]
  while (calculate_distance(center_point1$x1, center_point1$x2, center_point2$x1, center_point2$x2) <= radius * 2) {
    center_index2 <- sample(N, size = 1)
    center_point2 <- grdAmazonia[center_index2, ]
  }
  
  # Find all points within the second circle
  distances2 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point2$x1, center_point2$x2)
  within_circle_indices2 <- which(distances2 <= radius)
  within_circle2 <- grdAmazonia[within_circle_indices2, ]
  
  
  if (nrow(within_circle1) >= n_samples_1 && nrow(within_circle2) >= n_samples_2) {
    sample_indices1 <- sample(nrow(within_circle1), size = n_samples_1, replace = FALSE)
    sample1 <- within_circle1[sample_indices1, ]
    
    sample_indices2 <- sample(nrow(within_circle2), size = n_samples_2, replace = FALSE)
    sample2 <- within_circle2[sample_indices2, ]
    
    # Combine the samples
    mysample <- bind_rows(sample1, sample2)
    
    # GLM (Gamma(log))
    initial_model_7 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay), data = mysample,family = Gamma(link='log'))
    #GLM with interaction
    # initial_model_7_2 <- glm(AGB ~ (lnSWIR2 + Terra_PP + Elevation + Prec_dm + Clay)^2, data = mysample,family = Gamma(link='log'))
    
    # model selection 
    # change to "initial_model _7_2" if want to optimize GLM with interaction
    final_glm_model <- step(initial_model_7, direction = "backward", trace = FALSE)
    
    # Extract the formula from final_glm_model
    final_formula <- formula(final_glm_model)
    
    # Build the GAM model with the selected formula
    gam_model <- gam(final_formula, data = mysample, family = Gamma(link='log'))
    
    # summary(gam_model)
    # par(mfrow = c(2,2))
    # plot(final_glm_model)
    # gam.check(gam_model)
    
    # Variogram
    # gdata<- list(data= gam_model$residuals, coords=cbind(mysample$x1, mysample$x2))
    # par(mfrow=c(1,1))
    # set.seed(4)
    # cutoff=abs(5000000-6500000)/2
    # bin <- variog(gdata, max.dist=cutoff)
    # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
    # plot(bin,envelope=binenv)
    
    # Use the final model for prediction
    predictions <- predict.gam(gam_model, newdata = grdAmazonia, se.fit = TRUE, type='response')
    predict_mean <- mean(predictions$fit)
    
    # Standard error: Simulation method
    # design matrix
    X_new <- predict(gam_model, newdata = grdAmazonia, type = "lpmatrix")
    # Simulation Parameter Set
    set.seed(8+i)
    br <- mvtnorm::rmvnorm(n = 1000, mean = coef(gam_model), sigma = vcov(gam_model))
    # Compute linear predictive values and apply nonlinear transformations
    mean_AGB <- numeric(1000)
    for (j in 1:1000) {
      lp <- X_new %*% br[j, ]
      mean_AGB[j] <- mean(exp(lp))
    }
    
    # Calculate the confidence interval
    alpha <- 0.05  # for a 95% confidence interval
    ci_lower <- quantile(mean_AGB, probs = alpha / 2)
    ci_upper <- quantile(mean_AGB, probs = 1 - alpha / 2)
    
    # Save results
    results21$mean_agb[i] <- predict_mean
    results21$ci_lower[i] <- ci_lower
    results21$ci_upper[i] <- ci_upper
  } else {
    results21[i, ] <- c(NA, NA, NA)
  }
}

# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_50 <- results21 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_50 <- mean(abs(results_50$bias))
coverage_50 <- mean(results_50$coverage)
#plot(final_model,x=mysample$Elevation)
# Output results
bias_50
coverage_50

########################################################
# 3.3 Generalized additive model
# a. SRS



# b. SS


# c. Convenience sampling in one region
# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

radius <- 250000

calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# data frame to save results
n_iter <- 100
n_samples <- 100
results40_2 <- data.frame(mean_agb = numeric(n_iter),
                          ci_lower = numeric(n_iter),
                          ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

radius <- 250000

for (i in 1:n_iter) {
  set.seed(300+i)
  center_index <- sample(N, size = 1)
  center_point <- grdAmazonia[center_index, ]
  
  # Find all points within the circular range
  distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point$x1, center_point$x2)
  within_circle_indices <- which(distances <= radius)
  within_circle <- grdAmazonia[within_circle_indices, ]
  
  # Randomly select samples
  sample_indices <- sample(nrow(within_circle), size = n, replace = FALSE)
  mysample <- within_circle[sample_indices, ]
  
  # Fit the GAM model
  initial_model_8 <- gam(AGB ~ s(lnSWIR2) + s(Terra_PP) +s(Prec_dm) + s(Elevation) + s(Clay) , data = mysample)
  
  # summary(gam_model)
  # par(mfrow = c(2,3))
  # plot(initial_model_8, residuals = TRUE)
  # gam.check(initial_model_8)
  
  # # Variogram
  # gdata<- list(data= initial_model_8$residuals, coords=cbind(mysample$x1, mysample$x2))
  # par(mfrow=c(1,1))
  # set.seed(4)
  # # cutoff=abs(5000000-6500000)/2
  # bin <- variog(gdata, max.dist=cutoff)
  # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
  # plot(bin,envelope=binenv)
  
  # Use the final model for prediction
  predictions <- predict.gam(initial_model_8, newdata = grdAmazonia, se.fit = TRUE, type='response')
  predict_mean <- mean(predictions$fit)
  
  # SE: Simulation method
  # Simulation Parameter Set
  X_new <- predict(initial_model_8, newdata = grdAmazonia, type = "lpmatrix")
  set.seed(8+i)
  br <- mvtnorm::rmvnorm(n = 1000, mean = coef(initial_model_8), sigma = vcov(initial_model_8))
  # Compute linear predictive values and apply nonlinear transformations
  mean_AGB <- numeric(1000)
  for (j in 1:1000) {
    lp <- X_new %*% br[j, ]
    mean_AGB[j] <- mean(lp)
  }
  
  # Calculate the confidence interval
  alpha <- 0.05  # for a 95% confidence interval
  ci_lower <- quantile(mean_AGB, probs = alpha / 2)
  ci_upper <- quantile(mean_AGB, probs = 1 - alpha / 2)
  
  # Save results
  results40_2$mean_agb[i] <- predict_mean
  results40_2$ci_lower[i] <- ci_lower
  results40_2$ci_upper[i] <- ci_upper
}

# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_40_2 <- results40_2 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_40_2 <- mean(abs(results_40_2$bias))
coverage_40_2 <- mean(results_40_2$coverage)

# Output results
bias_40_2
coverage_40_2
###########################################################
# d. Convenience sampling in two regions
# load data
data(grdAmazonia)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population

# Initialize objects to save results
means <- numeric(100)
cis_lower <- numeric(100)
cis_upper <- numeric(100)

radius <- 250000

calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# data frame to save results
n_iter <- 100
n_samples <- 100
results40 <- data.frame(mean_agb = numeric(n_iter),
                        ci_lower = numeric(n_iter),
                        ci_upper = numeric(n_iter))

# Calculate the true population mean for AGB
true_mean <- mean(grdAmazonia$AGB)

for (i in 1:n_iter) {
  set.seed(300+i)
  # Choose the first random point as the center of the first circle
  center_index1 <- sample(N, size = 1)
  center_point1 <- grdAmazonia[center_index1, ]
  
  # Find all points within the first circle
  distances1 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point1$x1, center_point1$x2)
  within_circle_indices1 <- which(distances1 <= radius)
  within_circle1 <- grdAmazonia[within_circle_indices1, ]
  
  # Ensure the second center is not within the first circle
  center_index2 <- sample(N, size = 1)
  center_point2 <- grdAmazonia[center_index2, ]
  while (calculate_distance(center_point1$x1, center_point1$x2, center_point2$x1, center_point2$x2) <= radius * 2) {
    center_index2 <- sample(N, size = 1)
    center_point2 <- grdAmazonia[center_index2, ]
  }
  
  # Find all points within the second circle
  distances2 <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2, center_point2$x1, center_point2$x2)
  within_circle_indices2 <- which(distances2 <= radius)
  within_circle2 <- grdAmazonia[within_circle_indices2, ]
  
  if (nrow(within_circle1) >= n_samples_1 && nrow(within_circle2) >= n_samples_2) {
    sample_indices1 <- sample(nrow(within_circle1), size = n_samples_1, replace = FALSE)
    sample1 <- within_circle1[sample_indices1, ]
    
    sample_indices2 <- sample(nrow(within_circle2), size = n_samples_2, replace = FALSE)
    sample2 <- within_circle2[sample_indices2, ]
    
    # Combine the samples
    mysample <- bind_rows(sample1, sample2)
    
    # Fit the GAM model
    initial_model_8 <- gam(AGB ~ s(lnSWIR2) + s(Terra_PP) +s(Prec_dm) + s(Elevation) + s(Clay) , data = mysample)
    
    # summary(gam_model)
    # par(mfrow = c(2,3))
    # plot(initial_model_8, residuals = TRUE)
    # gam.check(initial_model_8)
    
    # # Variogram
    # gdata<- list(data= initial_model_8$residuals, coords=cbind(mysample$x1, mysample$x2))
    # par(mfrow=c(1,1))
    # set.seed(4)
    # # cutoff=abs(5000000-6500000)/2
    # bin <- variog(gdata, max.dist=cutoff)
    # binenv <- variog.mc.env(gdata,obj.var=bin,nsim=100)
    # plot(bin,envelope=binenv)
    
    # Use the final model for prediction
    predictions <- predict.gam(initial_model_8, newdata = grdAmazonia, se.fit = TRUE, type='response')
    predict_mean <- mean(predictions$fit)
    
    # SE: Simulation method
    # Simulation Parameter Set
    X_new <- predict(initial_model_8, newdata = grdAmazonia, type = "lpmatrix")
    set.seed(8+i)
    br <- mvtnorm::rmvnorm(n = 1000, mean = coef(initial_model_8), sigma = vcov(initial_model_8))
    # Compute linear predictive values and apply nonlinear transformations
    mean_AGB <- numeric(1000)
    for (j in 1:1000) {
      lp <- X_new %*% br[j, ]
      mean_AGB[j] <- mean(lp)
    }
    
    # Calculate the confidence interval
    alpha <- 0.05  # for a 95% confidence interval
    ci_lower <- quantile(mean_AGB, probs = alpha / 2)
    ci_upper <- quantile(mean_AGB, probs = 1 - alpha / 2)
    
    # Save results
    results40$mean_agb[i] <- predict_mean
    results40$ci_lower[i] <- ci_lower
    results40$ci_upper[i] <- ci_upper
  } else {
    results40[i, ] <- c(NA, NA, NA)
  }
}

# Calculate overall mean AGB
overa_mean_agb <- mean(grdAmazonia$AGB)

# Calculate bias and coverage
results_40 <- results40 %>%
  mutate(
    bias = mean_agb - overall_mean_agb,
    coverage = ifelse(ci_lower <= overall_mean_agb & ci_upper >= overall_mean_agb, 1, 0)
  )

# Calculate average bias and coverage
bias_40 <- mean(abs(results_40$bias))
coverage_40 <- mean(results_40$coverage)
#plot(final_model,x=mysample$Elevation)
# Output results
bias_40
coverage_40
