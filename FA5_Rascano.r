# JAN CELINE RASCANO 
# FORMATIVE ASSESSMENT #5

# EXERCISE 8.18
# Initializing the data
population <- c(9, 12, 15)
prob <- rep(1/3, 3)

# Generating all possible samples of size 2 with replacement
samples <- expand.grid(sample1 = population, sample2 = population)

# Calculating the mean of each sample
samples$mean <- rowMeans(samples[, c("sample1", "sample2")])

# Calculating xbar (mean of sample means)
xbar <- mean(samples$mean)

# Calculating P(xbar) (probability distribution of sample means)
pxbar <- table(samples$mean) / length(samples$mean)

# Calculating the xbar, p(xbar), xbar * p(xbar), and xbar^2 * p(xbar)
x_bar <- c(9, 10.5, 12, 13.5, 15)
px_bar <- c(0.1111111, 0.2222222, 0.3333333, 0.2222222, 0.1111111)

xbar_pxbar <- x_bar * px_bar
xbar2_pxbar <- x_bar^2 * px_bar

# Displaying the results

result <- cbind(samples, stringsAsFactors = FALSE)
print(result)

cat("Below Is the xbar[TOP PART] and p(xbar)[BOTTOM PART] \n")
print(pxbar)

cat("xbar X p(xbar): ", xbar_pxbar, "12 \n")
cat("xbar X p(xbar): ", xbar2_pxbar, "147 \n")
# END OF EXERCISE 8.18



# EXERCISE 8.21
# Calculating the mean 
population_mean <- (3+7+11+15)/4
cat("Population Mean:", population_mean, "\n")

# Calculating the population standard deviation
x <- ((3-population_mean)**2 + (7-population_mean)**2 + (11-population_mean)**2 + (15-population_mean)**2)/4  
population_sd <- sqrt(x)
cat("Population Standard Deviation:", population_sd, "\n")

# Calculating the Mean of the Sampling Distribution of Means
# As long as the sampling process is unbiased, the sampling distribution of means is equal to population mean. Hence we have,
cat("Mean of the Sampling Distribution of Means:", population_mean, "\n")

# Calculating the Standard Deviation of the Sampling Distribution of Means
sd_samp_dist_mean <- population_sd/sqrt(2) 
cat("Standard Deviation of the Sampling Distribution of Means:", sd_samp_dist_mean, "\n")
# END OF EXERCISE 8.21



# EXERCISE 8.34
# Probability of less than 40% boys
p_less_than_40_percent <- pbinom(80, size = 200, prob = 0.5)
cat("Probability of less than 40% boys:", p_less_than_40_percent, "\n")

# Probability of between 43% and 57% girls
p_between_43_and_57_percent <- pbinom(114, size = 200, prob = 0.5) - pbinom(86 - 1, size = 200, prob = 0.5)
cat("Probability of between 43% and 57% girls", p_between_43_and_57_percent, "\n")

# Probability of more than 54% boys
p_more_than_54_percent <- 1 - pbinom(108 - 1, size = 200, prob = 0.5)
cat("Probability of between 43% and 57% girls", p_more_than_54_percent, "\n")
# END OF EXERCISE 8.34



# EXERCISE 8.49
# Initializing the  data
credit_hours <- c(6, 9, 12, 15, 18)
probabilities <- c(0.1, 0.2, 0.4, 0.2, 0.1)

# Calculating mean (μ)
mean_value <- sum(credit_hours * probabilities)

# Calculating variance (σ^2)
variance <- sum((credit_hours - mean_value)^2 * probabilities)

# Displaying mean and variance
cat("Mean (μ):", mean_value, "\n")
cat("Variance (σ^2):", variance, "\n\n")

# Generating all possible samples of size 2 with replacement
all_samp <- expand.grid(credit_hours, credit_hours)

# Calculating means for each sample
samp_means <- rowMeans(all_samp)

# Calculating probabilities for each sample
samp_prob <- apply(all_samp, 1, function(samp) prod(probabilities[credit_hours %in% samp]))

# Displaying the header
cat("First Second  Mean  Probability\n")

# Displaying the results
for (x in 1:nrow(all_samp)) {
  cat(all_samp[x, 1], "  ", all_samp[x, 2], "   ", round(samp_means[x], 1), "   ", round(samp_prob[x], 4), "\n")
}
# END OF EXERCISE 8.49


