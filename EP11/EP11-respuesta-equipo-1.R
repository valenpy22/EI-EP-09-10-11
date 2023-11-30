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
# Waist.Girth
# Forearm.Girth
# Height

datos_n <- datos_n %>% select(Weight, Waist.Girth, Forearm.Girth, Height)


# Se ajusta el modelo usando boostrapping
modelo_boot <- train(Weight ~ ., data = datos_n, method = "lm",
                     trControl = trainControl(method = "boot", number = 999))

# Se imprimen los resultados
summary(modelo_boot)

# 3. Haciendo un poco de investigación sobre el paquete caret, en particular cómo 
# hacer Recursive Feature Elimination (RFE), construir un modelo de regresión 
# lineal múltiple para predecir la variable IMC que incluya entre 10 y 20 
# predictores, seleccionando el conjunto de variables que maximice R2 y que 
# use cinco repeticiones de validación cruzada de cinco pliegues para evitar 
# el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura 
# ni estado nutricional –Weight, Height, EN respectivamente). 

predicciones <- predict(modelo_boot, datos_n)
error <- datos_n[["Weight"]] - predicciones
rmse <- sqrt(mean(error ** 2))
cat("\nEl RMSE es: : ", rmse)

# Se tiene un R² ajustado de 0,9241.
# Por lo tanto, el modelo explica el 92,41% de la variabilidad de los
# datos.

# Se procede a obtener los residuos y estadísticas de influencia
# de los casos
evaluacion_modelo <- data.frame(predicted.probabilities = 
                                  fitted(modelo_boot[["finalModel"]]))

evaluacion_modelo[["residuos_estandarizados"]] <- rstandard(modelo_boot[["finalModel"]])
evaluacion_modelo[["residuos_estudiantizados"]] <- rstudent(modelo_boot[["finalModel"]])
evaluacion_modelo[["distancia_cook"]] <- cooks.distance(modelo_boot[["finalModel"]])
evaluacion_modelo[["dfbeta"]] <- dfbeta(modelo_boot[["finalModel"]])
evaluacion_modelo[["dffit"]] <- dffits(modelo_boot[["finalModel"]])
evaluacion_modelo[["apalancamiento"]] <- hatvalues(modelo_boot[["finalModel"]])
evaluacion_modelo[["covratio"]] <- covratio(modelo_boot[["finalModel"]])

cat("Influencia de los casos: \n")

# 5% de los residuos estandarizados
sospechosos1 <- which(abs(evaluacion_modelo[["residuos_estandarizados"]]) > 1.96)
cat(" # Residuos estandarizados fuera del 95% esperado # \n", sospechosos1)

# Observaciones con distancia de Cook donde son mayor a 1
sospechosos2 <- which(evaluacion_modelo[["distancia_cook"]] > 1)
cat(" # Residuos con distancia de Cook mayor a 1 # \n", sospechosos2)
cat(" No hay residuos con distancia de Cook mayor a 1 \n")

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n
apalancamiento_prom <- ncol(datos_n) / nrow(datos_n)

sospechosos3 <- which(evaluacion_modelo[["apalancamiento"]] > 2*apalancamiento_prom)
cat(" # Residuos con apalancamiento fuera del rango promedio, teniendo como promedio
        igual a ", apalancamiento_prom, " ", sospechosos3)

# Observaciones con dfbeta mayor o igual a 1
sospechosos4 <- which(apply(evaluacion_modelo[["dfbeta"]] >= 1, 1, any))
cat(" # Residuos con dfbeta mayor o igual a 1 #\n", sospechosos4)

# Observaciones con covarianzas fuera del rango
cvri_inferior <- 1 - 3*apalancamiento_prom
cvri_superior <- 1 + 3*apalancamiento_prom

sospechosos5 <- which(evaluacion_modelo[["covratio"]] < cvri_inferior
                      | evaluacion_modelo[["covratio"]] > cvri_superior)

cat(" # Residuos con razón de covarianza fuera de rango #", sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3,
                 sospechosos4, sospechosos5)

sospechosos <- sort(unique(sospechosos))

cat(" # Se tienen, en total, los siguientes datos observados sospechosos #\n") 
print(round(evaluacion_modelo[sospechosos, c("distancia_cook", "apalancamiento", "covratio")],
                                                                                     3))
# Pueden haber observaciones atípicas, pero la distancia de cook
# en todas se aleja de 1, por lo que no es causa de preocupación.

# Se procede a hacer el test durbinWatson para ver la independencia de los datos
cat("# Independencia de los residuos #\n")
print(durbinWatsonTest(modelo_boot[["finalModel"]]))

# Se tiene que el valor de p es 0,514, así pudiendo afirmar que los 
# residuos son independientes.
# Es decir, el modelo es confiable.