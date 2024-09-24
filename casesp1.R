#####################################
#####       Cases P1       ##########
#####################################

# Q1 [dataset meap0_01]
## Sua tarefa é estimar um modelo de math4 sobre lunch, enroll e exppp
## há uma suspeita de que os erros não são homocedasticos
## verifique a suposicao e, de ser o caso, apresente modelos
## alternativos para poder fazer inferencia valida (escreva
## um pseudo-codigo para cada alternativa)
## Seu colega suspeita que o modelo correto deve ser 
## modelo <- lm(math4 ~lunch + log(enroll) + log(exppp)
## repita a analise anteior, qual modelo escolheria?
## Interprete o modelo



library(wooldridge)
library(lmtest)
library(car)
modelo <- lm(math4 ~lunch + enroll + exppp, data = meap00_01)
summary(modelo)
# Verificar heterocedasticidade
bptest(modelo)  

# Alternativa 1 White 
white_estimator <- hccm(modelo, type = "hc0")
coef(modelo) / sqrt(diag(white_estimator))

# P-valor
2*(1- pnorm(abs(coef(modelo) / sqrt(diag(white_estimator)))))
2*(1 - pnorm(abs(summary(modelo)$coef[,3])))

# Alternativa 2: MQP

reg_aux <- lm(log(resid(modelo)^2) ~ lunch + enroll + exppp, data = meap00_01)
w <- 1/exp(fitted(reg_aux))
modelo_mqgf <- lm(math4 ~lunch + enroll + exppp, weight = w, data = meap00_01)
bptest(modelo_mqgf)



modelo <- lm(math4 ~lunch + log(enroll) + log(exppp), data = meap00_01)
summary(modelo)
# Verificar heterocedasticidade
bptest(modelo)  

# Alternativa 1 White 
white_estimator <- hccm(modelo, type = "hc0")
coef(modelo) / sqrt(diag(white_estimator))

# P-valor
2*(1- pnorm(abs(coef(modelo) / sqrt(diag(white_estimator)))))
2*(1 - pnorm(abs(summary(modelo)$coef[,3])))

# Alternativa 2: MQP

reg_aux <- lm(log(resid(modelo)^2) ~ lunch + log(enroll) + log(exppp), data = meap00_01)
w <- 1/exp(fitted(reg_aux))
modelo_mqgf <- lm(math4 ~lunch + log(enroll) + log(exppp), weight = w, data = meap00_01)
bptest(modelo_mqgf)

