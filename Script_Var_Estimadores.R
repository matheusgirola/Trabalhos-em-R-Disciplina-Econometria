library(readxl)
Passageiros_cambio_medio_2002_2019 <- read_excel("Passageiros cambio medio -2002-2019.xlsx", col_types = c("numeric", "numeric", "numeric"))
View(Passageiros_cambio_medio_2002_2019)
Passageiros_cambio_log <- Passageiros_cambio_medio_2002_2019
Passageiros_cambio_log[ , c(1,3)] <- log(Passageiros_cambio_medio_2002_2019[ ,c(1,3)])
View(Passageiros_cambio_log)
A = matrix(as.numeric(unlist(Passageiros_cambio_log)),nrow=nrow(Passageiros_cambio_log))
X = A[ , c(2,3)] #coeficiente e variavel independente de x
T = t(X) #transposta de X
Inversa = solve(T%*% X)
P = round(X %*% Inversa %*%T, 12)
M = diag(216) - P
Y = A[ , c(1)]
e = M%*%Y #a partir daqui que come�a msm o exercicio
Var_amostral = as.numeric((t(e)%*%e)/214)
Var_b_cond_X = Var_amostral*Inversa
Var_b_cond_X 
