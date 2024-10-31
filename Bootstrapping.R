############################################
###              Bootstrap               ###
############################################


# Motivação

# Estamos interessados em estimar a diferença salarial entre esposos na população
# Brasileira. Uma amostra de n = 4 apontou (esposo - esposa) = [6, -3, 5, 3] 
# (em unidades de 1000 dolares anuais)
#  Assumindo normalidade (ou invocando o TCL), um IC 95% seria:
y <- c(6, -3, 5, 3)
n <- length(y)
y_bar <- mean(y)
sigma_hat <- sd(y)

c(y_bar - qt(0.975, n - 1) * sigma_hat/sqrt(n), y_bar + qt(0.975, n - 1) * sigma_hat/sqrt(n))
c(y_bar - qnorm(0.975) * sigma_hat/sqrt(n), y_bar + qnorm(0.975) * sigma_hat/sqrt(n))


# o que acontece de utilizarmos bootstrap?

## Caso 1:
B <- 100
y_bar_boot <- c()
for (b in 1:B) {
  sample_bootstrap <- sample(1:n, n, replace = TRUE)
  y_boot <- y[sample_bootstrap]
  y_bar_boot[b] <-  mean(y_boot)
}

mean(y_bar_boot)

sd_boot = sd(y_bar_boot)
vies_boot = mean(y_bar_boot) - y_bar

c(y_bar - qt(0.975, n - 1) * sd_boot/sqrt(n), y_bar + qt(0.975, n - 1) * sd_boot/sqrt(n))
c((y_bar - vies_boot) - qt(0.975, n - 1) * sd_boot, (y_bar - vies_boot) + qt(0.975, n - 1) * sd_boot)

## Caso 2
B <- 1000
y_bar_boot <- c()
for (b in 1:B) {
  sample_bootstrap <- sample(1:n, n, replace = TRUE)
  y_boot <- y[sample_bootstrap]
  y_bar_boot[b] <-  mean(y_boot)
}

quantile(y_bar_boot, c(0.025, 0.975))

## Caso 3
alpha <- 0.05
z <- qnorm(sum(y_bar_boot <= y_bar) / (B + 1))
t_minus_i <- c()
for (i in 1:n) {
  t_minus_i[i] <- mean(y[-i])
}
t_bar = mean(t_minus_i)

a <- sum((t_bar - t_minus_i)^3) / (6 * (sum((t_minus_i - t_bar)^2)^{3/2}))
a1 <- pnorm(z + (z - qnorm(1 - alpha / 2)) / (1 + a*(z - qnorm(1 - alpha / 2))) )
a2 <- pnorm(z + (z + qnorm(1 - alpha / 2)) / (1 + a*(z + qnorm(1 - alpha / 2))) )

quantile(y_bar_boot, c(a1, a2))



### Simulacao
mc <- 5000
mu <- 0.5
alpha <- 0.05
n <- 10
cobertura_ic_0 <- c()
cobertura_ic_1 <- c()
cobertura_ic_2 <- c()
cobertura_ic_3 <- c()
for (i in 1:mc) {
  set.seed(123 + i)
  x <- rchisq(n, mu) # rnorm(n, mu) #
  x_bar <- mean(x)
  sigma_hat <- sd(x)
  ## IC assumindo normalidade
  IC_0 <- c(x_bar - qnorm(1 - alpha / 2) * sigma_hat / sqrt(n), x_bar + qnorm(1 - alpha / 2) * sigma_hat / sqrt(n))
  cobertura_ic_0[i] <- ifelse(mu >= IC_0[1] && mu <= IC_0[2], 1, 0)
  ## IC Caso 1
  x_bar_boot <- c()
  for (b in 1: 200) x_bar_boot[b] <-  mean(x[sample(1:n, n, replace = TRUE)])
  vies_boot <- mean(x_bar_boot) - x_bar
  sigma_boot <- sd(x_bar_boot)
  IC_1 <- c((x_bar - vies_boot) - qnorm(1 - alpha / 2) * sigma_boot, (x_bar - vies_boot) + qnorm(1 - alpha / 2) * sigma_boot)
  cobertura_ic_1[i] <- ifelse(mu >= IC_1[1] && mu <= IC_1[2], 1, 0)
  ## IC Caso 2
  x_bar_boot2 <- c()
  for (b in 1: 2000) x_bar_boot2[b] <-  mean(x[sample(1:n, n, replace = TRUE)])
  IC_2 <- quantile(x_bar_boot2, c(alpha / 2, 1 - alpha / 2))
  cobertura_ic_2[i] <- ifelse(mu >= IC_2[1] && mu <= IC_2[2], 1, 0)
  ## IC Caso 3
  
  
}
mean(cobertura_ic_0)
mean(cobertura_ic_1)
mean(cobertura_ic_2)
mean(cobertura_ic_3)


# Regressão

mc <- 5000
n <- 15
B <- 1000
teste_0 <- c()
teste_1 <- c()
beta_boot <- c()
for (i in 1:mc) {
  print(i)
  x <- rnorm(n, 10, 1)
  u <- rt(n, 3) #rexp(n, 0.5) - 2 #rnorm(n, 0, 1)
  y <- 0.3 + 1.2 * x + u
  beta_hat <- coef(lm(y ~ x))[2]
  teste_0[i] <- ifelse(coef(summary(lm(y ~ x)))[2, 4] < 0.05, 1, 0)
  for (b in 1:B) {
    index_boot <- sample(1:n, n, replace = TRUE)
    beta_boot[i] <- coef(summary(lm(y[index_boot] ~ x[index_boot])))[2, 1]
  }
  IC_1 <- quantile(beta_boot, c(0.025, 0.975))
  teste_1[i] <- ifelse(IC_1[1] <= 0  && 0 <= IC_1[2], 1, 0)
}
mean(teste_0) # 0.7288
mean(teste_1) # 0.951



mc <- 5000
n <- 15
B <- 1000
teste_0 <- c()
teste_1 <- c()
beta_boot <- c()
for (i in 1:mc) {
  print(i)
  x <- rnorm(n, 10, 1)
  u <- c(rnorm(n/3, 0, 1), rnorm(n/3, 0, 2), rnorm(n/3, 0, 3))
  y <- 0.3 + 1.2 * x + u
  beta_hat <- coef(lm(y ~ x))[2]
  teste_0[i] <- ifelse(coef(summary(lm(y ~ x)))[2, 4] < 0.05, 1, 0)
  for (b in 1:B) {
    index_boot <- sample(1:n, n, replace = TRUE)
    beta_boot[i] <- coef(summary(lm(y[index_boot] ~ x[index_boot])))[2, 1]
  }
  IC_1 <- quantile(beta_boot, c(0.025, 0.975))
  teste_1[i] <- ifelse(IC_1[1] <= 0  && 0 <= IC_1[2], 1, 0)
}
mean(teste_0) # 0.7288
mean(teste_1) # 0.951

# Casos mais complexos, precisam de procedimentos bootstrap mais sofisticados
## Heretocedasticidade: ver https://www.tandfonline.com/doi/abs/10.1080/07474939908800440
## Autocorrelação ver: http://www.monticini.eu/wp/rdavidson.pdf
## Para uma referência mais simples, ver: https://www.john-fox.ca/Companion/appendices/Appendix-Bootstrapping.pdf




