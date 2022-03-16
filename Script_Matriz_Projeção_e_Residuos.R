library(readxl)
Passageiro_cambio_medio_2019 <- read_excel("Passageiro-cambio-medio-2019.xlsx", col_types = c("numeric", "numeric", "numeric"))
View(Passageiro_cambio_medio_2019)
A = matrix(as.numeric(unlist(Passageiro_cambio_medio_2019)),nrow=nrow(Passageiro_cambio_medio_2019))
X = A[ , c(2,3)] #coeficiente e variavel independente de x
T = t(X) #transposta de X
Inversa = solve(T%*% X)
P = round(X %*% Inversa %*%T, 12)
M = diag(12) - P
#Propriedades de M
M == t(M) #simetria
round(M, 9) == round(M%*%M, 9) #idempotencia
round( M %*% X, 9) == 0 #MX =0 
#Propriedades de P
P == t(P) #simetria
round(P,9) == round(P%*%P, 9) #idempotencia
round( P %*% X, 9) == X #PX = X
# PM = MP = 0
round( M %*% P, 9) == 0
round( P %*% M, 9) == 0 

