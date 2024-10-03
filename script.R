
f <- function(x){
  return(10*x^2 - 2*x + 5)
}

get_sim_data <- function(f, sample_size = 100) {
  x <- runif(n = sample_size, min = -6, max = 6)
  error <- rnorm(n = sample_size, mean = 0, sd = 100)
  
  f_x <- sapply(x, f ) + error
  return(data.frame(x,f_x))
}
sample_1 <- get_sim_data(f)

# Fit models
lm1 <- lm(formula = f_x ~ 1, data = sample_1)
lm2 <- lm(formula = f_x ~ poly(x ,1), data = sample_1)
lm3 <- lm(formula = f_x ~ 1 + poly(x, 2), data = sample_1)
lm4 <- lm(formula = f_x ~ 1 + poly(x, 9), data = sample_1)

# Make predictions
pred1 <- predict(lm1, sample_1)
pred2 <- predict(lm2, sample_1)
pred3 <- predict(lm3, sample_1)
pred4 <- predict(lm4, sample_1)

