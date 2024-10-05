library("ggplot2")
library("gridExtra")

create_reg_variables <- function(n_size, beta0, beta1, corr = 0, sigma = 0) {
  if (abs(corr) > 1) {
    stop("correlation must be range [-1, 1]")
  }
  
  var_mat <- matrix(c(1.0, corr, corr, 1.0), ncol = 2)
  x_vector <- MASS::mvrnorm(n_size, mu = c(0, 0), Sigma = var_mat)
  y_vector <- beta0 * x_vector[, 1] +
    beta1 * x_vector[, 2] +
    rnorm(n_size, 0, sigma)
  
  return(data.frame(x_vector = x_vector, y_vector = y_vector))
}

# plot scatters
dir.create("./output/graphs", recursive = TRUE)
for (corr in c(-0.8, -0.4, 0.0, 0.4, 0.8)){
  vectors <- create_reg_variables(100, 1, 2, corr = corr, sigma = 1)
  range_ax <- c(min(vectors[, 1:2]), max(vectors[, 1:2]))
  
  g <- ggplot(
    data = vectors,
    mapping = aes(x = x_vector.1, y = x_vector.2)
  ) +
    geom_point(alpha = 0.5) +
    labs(
      title = paste("correlation Factor 1 and 2: ", corr, sep = ""),
      x = "Factor 1",
      y = "Factor 2",
    ) +
    coord_fixed(xlim = range_ax, ylim = range_ax)

  ggsave(
    paste("./output/graphs/two_factors_corr", corr, ".png", sep = ""),
    g,
    dpi = 400,
    height = 4,
    width = 4,
  )
}

# simulation
iter = 100
corr_iter = seq(-0.9, 0.9, by = 0.1)
size_iter <- c(10, 100, 1000, 10000)
scores <- data.frame(row.names = corr_iter)
for (n_size in size_iter){
  sd.1 <- c()
  sd.2 <- c()
  for (corr in corr_iter){
    factors.1 <- c()
    factors.2 <- c()
    for (idx in 1:iter){
      vectors <- create_reg_variables(n_size, 1, 1, corr = corr, sigma = 1)
      result <- lm(y_vector ~ x_vector.1 + x_vector.2, data = vectors)
      
      factors.1 <- c(factors.1, coef(summary(result))["x_vector.1", "Estimate"])
      factors.2 <- c(factors.2, coef(summary(result))["x_vector.2", "Estimate"])
    }
    
    sd.1 <- c(sd.1, sd(factors.1))
    sd.2 <- c(sd.2, sd(factors.2))
  }
  scores[, paste(n_size, "factor.1", sep = ".")] = sd.1
  scores[, paste(n_size, "factor.2", sep = ".")] = sd.2
}
write.csv(scores, "./output/scores.csv")

# plot aggregation
scores <- cbind(corr = rownames(scores), scores)
row.names(scores) <- 1:nrow(scores)
stack_scores <- data.frame()
for (n_size in size_iter){
  sub_scores <- scores[
    c(
      "corr",
      paste(n_size, "factor.1", sep = "."),
      paste(n_size, "factor.2", sep = ".")
    )
  ]
  colnames(sub_scores) <- c("corr", "factor.1", "factor.2")
  sub_scores$n.size <- n_size
  stack_scores <- rbind(stack_scores, sub_scores)
}

g1 <- ggplot(
    data = stack_scores,
    mapping = aes(
      x = factor(corr, levels = corr_iter), y = factor.1, fill = factor(n.size)
    )
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "coefficient sd for Factor.1", x = "correlation")
g2 <- ggplot(
    data = stack_scores,
    mapping = aes(
      x = factor(corr, levels = corr_iter), y = factor.2, fill = factor(n.size)
    )
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "coefficient sd for Factor.2", x = "correlation")

ggsave(
  paste("./output/graphs/sd_aggregation.png"),
  grid.arrange(g1, g2),
  dpi = 400,
  height = 6,
  width = 8,
)
