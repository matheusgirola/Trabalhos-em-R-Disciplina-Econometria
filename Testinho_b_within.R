library(AER)
library(plm)
library(stargazer)
library(fastDummies)

data(Fatalities)

#definir taxa de fatalidade
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

#definir dummy variable
Fatalities <- dummy_cols(Fatalities , select_columns = 'state')

D <- as.matrix(Fatalities[ , 36:83])
X <- as.matrix(Fatalities[ , c(7)])
Y <- as.matrix(Fatalities[ , c(35)])

#Calcular Md
M_D = diag(336) - D%*%solve(t(D)%*%D)%*%t(D)

#Calcular b_within
b_within = solve(t(X)%*%M_D%*%X)%*%(t(X)%*%M_D%*%Y)


#teste
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")
fatal_fe_mod
