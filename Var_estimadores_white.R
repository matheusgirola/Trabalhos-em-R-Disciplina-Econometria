library(readxl)
Passageiros_cambio_medio_2002_2019 <- read_excel("Passageiros cambio medio -2002-2019.xlsx", col_types = c("numeric", "numeric", "numeric"))
View(Passageiros_cambio_medio_2002_2019)
Passageiros_cambio_log <- Passageiros_cambio_medio_2002_2019
Passageiros_cambio_log[ , c(1,3)] <- log(Passageiros_cambio_medio_2002_2019[ ,c(1,3)])
View(Passageiros_cambio_log)
A = matrix(as.numeric(unlist(Passageiros_cambio_log)),nrow=nrow(Passageiros_cambio_log))
X = A[ , c(2,3)] #constante variavel independente de x
T = t(X) #transposta de X
Inversa = solve(T%*% X)
P = round(X %*% Inversa %*%T, 12)
M = diag(216) - P
Y = A[ , c(1)]
e = M%*%Y
b = round(Inversa %*%T, 12)%*%Y
#Processo pra achar aquela matriz com os erros quadrados
e_squared = round(e^2,10)
x = matrix(e_squared, 216, 216)
E =  diag(diag(x)) #matriz que o professor pede com os erros quadrados na diagonal
#agr sim calcular a variancia white
Var_b_white = Inversa %*% T %*% E %*% X %*% Inversa*(216/214)

#library(sandwich)
#modelo <- lm(Y ~X)
#sandwich::vcovHC(modelo)