

# Load necessary package
#install.packages("ggplot2")
library(ggplot2)


########### Simulating Beta distribution as prior ###############

# Define the parameters:
alpha <- 2
beta <- 2

# Create a sequence of x values from 0 to 1
x <- seq(0, 1, length.out = 100)

# Compute the density of the Beta distribution for each x value
y <- dbeta(x, shape1 = alpha, shape2 = beta)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the Beta distribution with grey-blue color
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "#6699CC") +
  geom_area(fill = "lightblue", alpha=0.5) +
  labs(title = "Beta Distribution (2, 2)",
       x = "x",
       y = "Density") +
  theme_minimal()



########### Simulating Exponential distribution as prior ###############

# Define the rate parameter for the exponential distribution
rate <- 1.54

# Create a sequence of x values from 0 to 5 (to cover a range of the distribution)
x <- seq(0, 5, length.out = 100)

# Compute the density of the exponential distribution for each x value
y <- dexp(x, rate = rate)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the exponential distribution
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "#6699CC") +
  geom_area(fill = "lightblue", alpha = 0.5) +
  labs(title = "Exponential Distribution (rate = 1.54)",
       x = "x",
       y = "Density") +
  theme_minimal()


########### Simulating Gamma distribution as prior ###############

# Define the parameters for the Gamma distribution
shape <- 2
rate <- 0.5

# Create a sequence of x values
x <- seq(0, 10, length.out = 100)

# Compute the density of the Gamma distribution for each x value
y <- dgamma(x, shape = shape, rate = rate)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the Gamma distribution with light blue fill (half transparent)
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "#6699CC") +
  labs(title = "Gamma Distribution (shape = 2, rate = 0.5)",
       x = "x",
       y = "Density") +
  theme_minimal()


## now let's make some comparisons:
# Define the parameters for the Gamma distributions
shape1 <- 2
rate1 <- 0.5
shape2 <- 2
rate2 <- 1

# Create a sequence of x values
x <- seq(0, 10, length.out = 100)

# Compute the density of the Gamma distributions for each x value
y1 <- dgamma(x, shape = shape1, rate = rate1)
y2 <- dgamma(x, shape = shape2, rate = rate2)

# Create a data frame for plotting
data1 <- data.frame(x = x, y = y1, distribution = "Gamma(2, 0.5)")
data2 <- data.frame(x = x, y = y2, distribution = "Gamma(2, 1)")
data <- rbind(data1, data2)

# Plot the Gamma distributions
ggplot(data, aes(x = x, y = y, fill = distribution, color = distribution)) +
  geom_area(data = data1, fill = "lightblue", alpha = 0.5) +
  geom_line(data = data1, color = "#6699CC") +
  geom_area(data = data2, fill = "lightcoral", alpha = 0.5) +
  geom_line(data = data2, color = "red") +
  labs(title = "Gamma Distributions",
       x = "x",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("Gamma(2, 0.5)" = "lightblue", "Gamma(2, 1)" = "lightcoral")) +
  scale_color_manual(values = c("Gamma(2, 0.5)" = "#6699CC", "Gamma(2, 1)" = "red"))



########### Simulating Half-normal distribution as prior ###############

# Define the parameters for the Half-Normal distribution
sigma <- 2.5

# Create a sequence of x values from 0 to a reasonable upper limit
x <- seq(0, 10, length.out = 100)

# Compute the density of the Half-Normal distribution for each x value
# Half-Normal is a truncated Normal distribution with mean = 0 and sd = sigma
y <- (2 / (sigma * sqrt(2 * pi))) * exp(-x^2 / (2 * sigma^2))

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the Half-Normal distribution
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "#6699CC") +
  labs(title = "Half-Normal Distribution (sigma = 2.5)",
       x = "x",
       y = "Density") +
  theme_minimal()


########### Simulating for log-normal distribution as prior ###############


# Define the parameters for the Log-Normal distribution
mu <- -1.1931
sigma <- 1

# Create a sequence of x values
x <- seq(0, 5, length.out = 100)

# Compute the density of the Log-Normal distribution for each x value
y <- dlnorm(x, meanlog = mu, sdlog = sigma)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the Log-Normal distribution
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "#6699CC") +
  labs(title = "Log-Normal Distribution (mean = 0.5, fat tail)",
       subtitle = "params: mu= -1.1931, sigma=1",
       x = "x",
       y = "Density") +
  theme_minimal()




########### Simulating for normal distribution as prior ###############


# Define the parameters for the Normal distribution
mean <- 0
sd <- 2.5

# Create a sequence of x values
x <- seq(-10, 10, length.out = 100)

# Compute the density of the Normal distribution for each x value
y <- dnorm(x, mean = mean, sd = sd)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the Normal distribution
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "#6699CC") +
  labs(title = "Normal Distribution (mean = 0, sd = 2.5)",
       x = "x",
       y = "Density") +
  theme_minimal()




########### Simulating for t-student distribution as prior ###############

# Define the parameters for the Student's t-distribution
df <- 3    # Degrees of freedom
mean <- 0  # Location parameter (mean)
scale <- 2 # Scale parameter

# Create a sequence of x values
x <- seq(-10, 10, length.out = 100)

# Compute the density of the Student's t-distribution for each x value
y <- dt((x - mean) / scale, df = df) / scale

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the Student's t-distribution
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "#6699CC") +
  labs(title = "Student's t-Distribution (df = 3, mean = 0, scale = 2)",
       x = "x",
       y = "Density") +
  theme_minimal()



########### Simulating for cauchy distribution as prior ###############

# Define the parameters for the Cauchy distribution
location <- 0  # Location parameter (median)
scale <- 2.5   # Scale parameter

# Create a sequence of x values
x <- seq(-10, 10, length.out = 1000)

# Compute the density of the Cauchy distribution for each x value
y <- dcauchy(x, location = location, scale = scale)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the Cauchy distribution
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "#6699CC") +
  labs(title = "Cauchy Distribution (location = 0, scale = 2.5)",
       x = "x",
       y = "Density") +
  theme_minimal()