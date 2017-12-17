library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(mboost) 

set.seed(1234)

# True function
foo <- function(x) (0.5 - 0.9 * exp(-50 * x^2)) * x

# Data simulation
n = 150

df <- data.frame(x = runif(n, -0.2, 0.2))
df$t <- foo(df$x)
df$y <- df$t + 0.02 * rnorm(n)


# Boosting model
mod_fit <- gamboost(y ~ x, data = df)
cvr <- cvrisk(mod_fit, grid = 1:200)
m_opt <- mstop(cvr)


# Fitted values
df$y_hat_1 <- fitted(mod_fit[1])
df$y_hat_5 <- fitted(mod_fit[5])
df$y_hat_20 <- fitted(mod_fit[20])
df$y_hat_20 <- fitted(mod_fit[20])
df$y_hat_opt <- fitted(mod_fit[m_opt])

# Residuals
df$res_1 <- df$y - df$y_hat_1
df$res_5 <- df$y - df$y_hat_5
df$res_20 <- df$y - df$y_hat_20
df$res_opt <- df$y - df$y_hat_opt


# Fitting
p1 <- df %>%
  select(x, y, t, contains("y_hat")) %>%
  melt(., id.vars = c("x", "y", "t")) %>%
  mutate(variable = factor(variable, labels = c("m = 1", "m = 5", "m = 20", "m*"))) %>%
  ggplot(.) + 
    geom_point(aes(x, y)) +
    geom_line(aes(x, t), linetype = "dashed") + 
    geom_line(aes(x, value), color = "red") + 
    facet_wrap(~variable, nrow = 1) + 
    theme_minimal() + 
    labs(title = "Model fit at boosting iteration m")

# Residuals
p2 <- df %>%
  select(x, contains("res")) %>%
  melt(., id.vars = c("x")) %>%
  mutate(variable = factor(variable, labels = c("m = 1", "m = 5", "m = 20", "m*"))) %>%
  ggplot(., aes(x, value)) + 
    geom_point() +
    geom_smooth(se = FALSE) + 
    facet_wrap(~variable, nrow = 1) + 
    theme_minimal() + 
    labs(title = "Model residuals at boosting iteration m",
       y = "residuals")

grid.arrange(p1, p2)




