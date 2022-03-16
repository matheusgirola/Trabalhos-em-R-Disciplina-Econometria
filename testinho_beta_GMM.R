library("AER")
library("stargazer")
library("gmm")

data("CigarettesSW")
#summary(CigarettesSW)

#View(CigarettesSW)

##MONTAR O DATASET
  # calcular o preço real per capita
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  
  # calcular os impostos sobre vendas
  CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

  # Adicionar renda real
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)

  # Adicionar imposto sobre cigarros
  CigarettesSW$cigtax <- with(CigarettesSW, tax/cpi)

  # obter dados em cross-section para 1995
  c1995 <- subset(CigarettesSW, year == "1995")

#MONTAR AS VARIAVEIS PROS CALCULOS
  Y <- c1995[ , c(5)]
  X <- c1995[ , c(10,12)]
  Z <- c1995[ , c(11,12,13)] #IV

  constante <-rep(c(1), each = 48)
  
  X <- as.matrix(cbind(constante,X))
  Z <- as.matrix(cbind(constante,Z))
  
  Transposta_X = t(X) #transposta de X
  Transposta_Z = t(Z) #tranposta de Z
  
  #Calcular erro do MQO
  Inversa_Transposta_X_vezes_X = solve(Transposta_X %*% X)
  P = X %*% Inversa_Transposta_X_vezes_X  %*% Transposta_X
  M = diag(48) - P
  e = M%*%Y
  #processo pra obter a matriz D com os erro quadrado na diagonal
  e_squared = e^2
  x = matrix(e_squared, 48, 48)
  D =  diag(diag(x))
  
  #Calcular W e beta com heterocedasticidade
  S_chapeu = (t(Z) %*% D %*% Z)/48
  W_hetero = solve(S_chapeu)
  
  Beta_GMM_Hetero = solve(t(X)%*%Z%*%W_hetero%*%t(Z)%*%X)%*%t(X)%*%Z%*%W_hetero%*%t(Z)%*%Y
  
  #Calcular W e beta com homocedasticidade
  
  sigma_quadrado = (t(e)%*%e)/(45)
  S_homo = as.numeric(sigma_quadrado)*((t(Z)%*%Z)/48)
  W_homo = solve(S_homo)
  Beta_GMM_Homo = solve(t(X)%*%Z%*%W_homo%*%t(Z)%*%X)%*%t(X)%*%Z%*%W_homo%*%t(Z)%*%Y
  
  #TESTE
  gmm((c1995[ , c(5)]) ~ (c1995[ , c(10)]) + (c1995[ , c(12)]) , ~ (c1995[ ,c(11)]) + (c1995[ , c(12)]) + (c1995[ , c(13)]))
  ivreg(formula = Y ~ X | Z)