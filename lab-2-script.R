#
library(cluster)
library(factoextra)
library(tidyverse)

# Guardamos la URL de la base de datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"

# Leemos directamente sobre los datos y se guarda en un data frame
# El separador de columas es una coma ","
cars <- read.table(url, sep = ",")

# Colocamos los nombres a las columnas
colnames(cars) <- c("buyingPrice", 
                    "maintenanceCost", 
                    "numberOfDoors", 
                    "numberOfPersons",
                    "sizeOfLuggageBoot",
                    "safety",
                    "decision")

######## PRE - PROCESAMIENTO ############

# Se cambian todos los valores a numericos mediante
# la creacion de funciones y el uso de sapply.

# Funcion para cambiar los precios y costos a numeros
changeToNumberPriceAndCost <- function(x) {
  if(x == 'vhigh') {return(3)}
  else if (x == 'high') {return(2)}
  else if (x == 'med') {return(1)}
  else if (x == 'low') {return(0)}
}

# Se aplica la funcion anterior a las columnas correspondientes
cars$buyingPrice <- sapply(cars$buyingPrice, changeToNumberPriceAndCost)
cars$maintenanceCost <- sapply(cars$maintenanceCost, changeToNumberPriceAndCost)

# Funcion para cambiar el numero de puertas a valor numerico
changeToNumberDoors <- function(x) {
  if(x == '2') {return(0)}
  else if(x == '3') {return(1)}
  else if(x == '4') {return(2)}
  else if(x == '5more') {return(3)}
}

# Se aplica a la columna correspondiente
cars$numberOfDoors <- sapply(cars$numberOfDoors, changeToNumberDoors)

# Funcion para cambiar el numero de personas a valor numerico
ChangeToNumberPersons <- function(x) {
  if(x == '2') {return(0)}
  else if(x == '4') {return(1)}
  else if(x == 'more') {return(2)}
}

# Se aplica la funcion a la columna correspondiente
cars$numberOfPersons <- sapply(cars$numberOfPersons, ChangeToNumberPersons)

# Funcion para cambiar el tamanio del maletero a valor numerico
changeToNumberLuggage <- function(x) {
  if(x == 'big') {return(2)}
  else if(x == 'med') {return(1)}
  else if(x == 'small') {return(0)}
}

# Se aplica la funcion a la columna correspondiente
cars$sizeOfLuggageBoot <- sapply(cars$sizeOfLuggageBoot, changeToNumberLuggage)

# Funcion para cambiar la seguridad a valor numerico
changeToNumberSafety <- function(x) {
  if(x == 'low') {return(0)}
  else if(x == 'med') {return(1)}
  else if(x == 'high') {return(2)}
} 

# Se aplica a la columna correspondiente
cars$safety <- sapply(cars$safety, changeToNumberSafety)

# Funcion para cambiar la decision a valor numerico
changeToNumberDecision <- function(x) {
  if(x == 'unacc') {return(0)}
  else if(x == 'acc') {return(1)}
  else if(x == 'good') {return(2)}
  else if(x == 'vgood') {return(3)}
}

# Se aplica a la columna correspondiente
cars$decision <- sapply(cars$decision, changeToNumberDecision)

# Se verifica que los tipos de atributos sean numericos
sapply(cars, class)

####### APLICACION DE K-MEANS ############

# Se seleccionan las variables mas influyentes 
importantVariables <- cars %>% select(buyingPrice, maintenanceCost, safety, numberOfPersons, decision)
testing <- cars %>% select(safety, decision)

testing2 <- cars %>% select(buyingPrice, maintenanceCost)
fit_testing2 <- pam(testing2, 16)
fviz_cluster(fit_testing2, data = testing2, geom = c("point"))
fviz_nbclust(testing2, pam, method = "wss")

fit_testing_pam <- pam(testing, 6, stand = FALSE)
fit_pam <- pam(importantVariables, 3, stand = FALSE)
fit_kmeans <- kmeans(importantVariables, centers = 5, nstart = 25)



fit_pam$silinfo

fviz_cluster(fit_kmeans, data = importantVariables, geom = c("point"))

fviz_cluster(fit_pam, data = importantVariables, geom = c("point"))
fviz_cluster(fit_testing_pam, data = testing, geom = c("point"))

fviz_cluster(fit_kmeans, data = importantVariables, geom = c("point"))
print(fit_kmeans)
fviz_nbclust(testing, pam, method = "silhouette")

matriz1 <- daisy(importantVariables, metric = "gower")
fit_pam <- pam(matriz1, k = 3) 
fviz_cluster(matriz1, importantVariables, geom = c("point"))
fviz_cluster(matriz1, data = importantVariables, geom = c("point"))

fit_pam$data = importantVariables
fviz_cluster(fit_pam)

j_dist <- dist(importantVariables, method = "binary", p = 4)
fit_pam <- pam(j_dist, k = 6, diss = TRUE) 
fviz_cluster(fit_pam)


class(matriz)

