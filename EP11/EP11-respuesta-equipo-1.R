# Cargar las librerías
library(tidyverse)
library(car)
library(caret)
library(leaps)

# Se leen los datos
datos <- read.csv2("Desktop/EI-EP-09-10-11/EP10/EP09 Datos.csv")

# Se verifica que los datos se leen correctamente
head(datos)

# Se crea una nueva columna para calcular el IMC
datos[["IMC"]] <- datos[["Weight"]]/((datos[["Height"]])/100)^2

# Se crea una nueva columna para determinar si está con sobrepeso o no
# según el IMC.
datos[["EN"]] <- ifelse(datos[["IMC"]] >= 25, 1, 0)

# Se verifica que se hayan agregado correctamente las columnas
head(datos)

# Enunciado
# 1. Definir la semilla a uwatilizar, que corresponde a los primeros cinco dígitos
# del RUN del integrante de mayor edad del equipo.
set.seed(20785)

# 2. Seleccionar una muestra de 100 personas, asegurando que la mitad tenga
# estado nutricional "sobrepeso" y la otra mitad "no sobrepeso".
sobrepeso <- datos %>% filter(EN == 1) %>% sample_n(50)
nosobrepeso <- datos %>% filter(EN == 0) %>% sample_n(50)

# Se unen las dos muestras
muestra <- rbind(sobrepeso, nosobrepeso)

# 3. Usando las herramientas del paquete leaps, realizar una búsqueda 
# exhaustiva para seleccionar entre dos y ocho predictores que ayuden a estimar 
# la variable Peso (Weight), obviamente sin considerar las nuevas variables IMC 
# ni EN, y luego utilizar las funciones del paquete caret para construir un 
# modelo de regresión lineal múltiple con los predictores escogidos y 
# evaluarlo usando bootstrapping.

# Se eliminan las variables IMC y EN del conjunto
datos_n <- muestra %>% select(-c(IMC, EN)) 

# Se hace el modelo con un máximo de 8 predictores
modelo <- regsubsets(Weight ~ ., data = datos_n, nbest = 1, nvmax = 8,
                     method = "exhaustive")
plot(modelo)

# Los predictores con menos BIC son:
# 

# Se ajusta el modelo usando boostrapping
modelo_boot <- train(Weight ~ ., data = datos_n, method = "lm",
                     trControl = trainControl(method = "boot", number = 2999))

# Se imprimen los resultados
summary(modelo_boot)

# Haciendo un poco de investigación sobre el paquete caret, en particular cómo 
# hacer Recursive Feature Elimination (RFE), construir un modelo de regresión 
# lineal múltiple para predecir la variable IMC que incluya entre 10 y 20 
# predictores, seleccionando el conjunto de variables que maximice R2 y que 
# use cinco repeticiones de validación cruzada de cinco pliegues para evitar 
# el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura 
# ni estado nutricional –Weight, Height, EN respectivamente). 






