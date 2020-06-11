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

head(cars)

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

head(cars)

####### APLICACION DE K-MEANS ############

####### CON TODAS LAS VARIABLES ###########

# Todas las variables
allVariables <- cars %>% select(buyingPrice, maintenanceCost, safety, numberOfPersons, numberOfDoors, sizeOfLuggageBoot)

# Se calculan las distancias
distances <- daisy(allVariables, metric = "gower")

# Se aplica el algoritmo
fit_pam <- pam(distances, k = 4)
fit_pam$data <- allVariables

# Se obtiene el grafico con los clusters
fviz_cluster(fit_pam, geom = c("point"), ellipse.type = "norm", stand = FALSE)

# Para conocer el numero optimo de cluster "k"
fviz_nbclust(allVariables, pam, method = "silhouette")
fviz_nbclust(allVariables, pam, method = "wss")


###### CON VARIABLES MAS INFLUYENTES ########

# Solo con las variables mas importantes

# Se seleccionan las variables mas influyentes 
importantVariables <- cars %>% select(buyingPrice, maintenanceCost, safety, numberOfPersons)

# Se calculan las distancias
distances_imp <- daisy(importantVariables, metric = "gower")

# Se aplica el algoritmo
fit_pam_imp <- pam(distances_imp, k = 6)
fit_pam_imp$data <- importantVariables

# Se grafican los clusters
fviz_cluster(fit_pam_imp, geom = c("point"), ellipse.type = "norm", stand = FALSE)

# Se obtienen el numero optimo de "k"
fviz_nbclust(importantVariables, pam, method = "silhouette")
fviz_nbclust(importantVariables, pam, method = "wss")

########## CON DOS VARIABLES #############

# Con dos variables
twoVariables <- cars %>% select(safety, numberOfPersons)

# Se calculan las distancias
distances_two <- daisy(twoVariables, metric = "gower")

fit_pam_two <- pam(distances_two, k = 2)
fit_pam_two$data <- twoVariables

fviz_cluster(fit_pam_two, geom = c("point"))

fviz_nbclust(twoVariables, pam, method = "silhouette")
fviz_nbclust(twoVariables, pam, method = "wss")


