install.packages("AER")
install.packages("stargazer")
library("AER")
library("stargazer")
library(readxl)

Passageiros_cambio_medio_2002_2019 <- read_excel("Passageiros cambio medio -2002-2019.xlsx", col_types = c("numeric", "numeric", "numeric", "numeric"))
View(Passageiros_cambio_medio_2002_2019)
Passageiros_cambio_log <- Passageiros_cambio_medio_2002_2019
Passageiros_cambio_log[ , c(1,3,4)] <- log(Passageiros_cambio_medio_2002_2019[ ,c(1,3,4)])
#View(Passageiros_cambio_log)
A = matrix(as.numeric(unlist(Passageiros_cambio_log)),nrow=nrow(Passageiros_cambio_log))
Y = A[ , c(1)]
X = A[ , c(2,3)] #coeficiente e variavel independente de x
Z = A[ , c(2,4)] #variavel instrumental
Transposta_X = t(X) #transposta de X
Transposta_Z = t(Z)
Inversa_transposta_Z_vezes_X = solve(Transposta_Z %*% X)


Estimador_IV = Inversa_transposta_Z_vezes_X %*% Transposta_Z %*% Y
Estimador_IV

#teste pra ver se deu certo
reg_iv <- ivreg(Y ~ X | Z)
reg_iv

#calcular a variancia - jeito chique
e <- as.matrix(reg_iv$residuals)
e2 <- t(e)%*%e
sigma_squared <- mean(e2)/216
var_iv <- (sigma_squared )*(Inversa_transposta_Z_vezes_X)%*%(Transposta_Z%*%Z)%*%(solve(Transposta_X%*%Z))
var_iv

#calcular variancia - jeito do greene
Inversa_Transposta_X_vezes_X = solve(Transposta_X %*% X)
P = X %*% Inversa_Transposta_X_vezes_X  %*% Transposta_X
M = diag(216) - P
e = M%*%Y
e_chapeu = (diag(216) - X %*% Inversa_transposta_Z_vezes_X %*% Transposta_Z)%*%e
Var_amostral = as.numeric((t(e_chapeu)%*%e_chapeu)/216)
Var_assintotica_IV = Var_amostral*(Inversa_transposta_Z_vezes_X)%*%(Transposta_Z%*%Z)%*%solve(Transposta_X%*%Z)
Var_assintotica_IV 

#teste pra ver se deu certo
vcov(reg_iv)
