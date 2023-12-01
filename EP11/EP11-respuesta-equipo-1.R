# Se cargan las librerías
library(tidyverse)
library(car)
library(caret)
library(leaps)

# Se leen los datos
datos <- read.csv2("Desktop/EI-EP-09-10-11/EP10/EP09 Datos.csv")

# Se verifica que los datos se leen correctamente
head(datos)

# Se crea una nueva columna para calcular el IMC, tomando el peso y
# dividiéndolo por su altura en metros al cuadrado.
datos[["IMC"]] <- datos[["Weight"]]/((datos[["Height"]])/100)^2

# Se crea una nueva columna para determinar si está con sobrepeso o no
# según el IMC.
# 1: IMC >= 25: Sobrepeso
# 0: IMC < 25: No sobrepeso
datos[["EN"]] <- ifelse(datos[["IMC"]] >= 25, 1, 0)

# Se factoriza esta columna
datos[["EN"]] <- factor(datos[["EN"]])

# Se verifica que se hayan agregado correctamente las columnas
head(datos)

# Enunciado
# 1. Definir la semilla a utilizar, que corresponde a los primeros cinco dígitos
# del RUN del integrante de mayor edad del equipo.
set.seed(20785)

# 2. Seleccionar una muestra de 100 personas, asegurando que la mitad tenga
# estado nutricional "sobrepeso" y la otra mitad "no sobrepeso".
sobrepeso <- datos %>% filter(EN == 1) %>% sample_n(50)
nosobrepeso <- datos %>% filter(EN == 0) %>% sample_n(50)

# Se unen las dos muestras
muestra <- rbind(sobrepeso, nosobrepeso)

################################################################################
# MODELO DE REGRESIÓN LINEAL MÚLTIPLE PARA PREDECIR LA VARIABLE PESO
################################################################################
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

# Se muestra el gráfico
plot(modelo)

# Los predictores con menos BIC son:
# Waist.Girth
# Forearm.Girth
# Height

# Se seleccionan solo estos predictores (columnas)
datos_n <- datos_n %>% select(Weight, Waist.Girth, Forearm.Girth, Height)

# Se ajusta el modelo usando boostrapping
modelo_n <- train(Weight ~ ., data = datos_n, method = "lm",
                     trControl = trainControl(method = "boot", number = 999))

# Se imprimen los resultados
summary(modelo_n)

# Se tiene un R² ajustado de 0,9218, esto quiere decir que el modelo explica
# un 92,18% de la variabilidad de los datos, lo que es bastante alto.

# Se hace una predicción con este modelo y los datos
predicciones <- predict(modelo_n, datos_n)
error <- datos_n[["Weight"]] - predicciones
rmse <- sqrt(mean(error ** 2))
cat("\nEl RMSE es: : ", rmse)

# Se procede a obtener los residuos y estadísticas de influencia
# de los casos
evaluacion_modelo <- data.frame(predicted.probabilities = 
                                  fitted(modelo_n[["finalModel"]]))

evaluacion_modelo[["residuos_estandarizados"]] <- rstandard(modelo_n[["finalModel"]])
evaluacion_modelo[["residuos_estudiantizados"]] <- rstudent(modelo_n[["finalModel"]])
evaluacion_modelo[["distancia_cook"]] <- cooks.distance(modelo_n[["finalModel"]])
evaluacion_modelo[["dfbeta"]] <- dfbeta(modelo_n[["finalModel"]])
evaluacion_modelo[["dffit"]] <- dffits(modelo_n[["finalModel"]])
evaluacion_modelo[["apalancamiento"]] <- hatvalues(modelo_n[["finalModel"]])
evaluacion_modelo[["covratio"]] <- covratio(modelo_n[["finalModel"]])

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

# Se juntan todos los sospechosos para poder evaluar cuántos hay en total
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3,
                 sospechosos4, sospechosos5)

# Se ordenan y se eliminan los sospechosos duplicados
sospechosos <- sort(unique(sospechosos))

cat(" # Se tienen, en total, los siguientes datos observados sospechosos #\n") 
print(round(evaluacion_modelo[sospechosos, c("distancia_cook", "apalancamiento", "covratio")],
                                                                                     3))
# Pueden haber observaciones atípicas, pero la distancia de cook en todas se 
# aleja de 1, por lo que no es causa de preocupación y se puede seguir con la
# evaluación del modelo sin problemas.

# Se procede a hacer el test durbinWatson para ver la independencia de los datos
cat("# Independencia de los residuos #\n")
print(durbinWatsonTest(modelo_n[["finalModel"]]))

# Se tiene que el valor de p es 0,514, así pudiendo afirmar que los residuos son 
# independientes. 
# Es decir, el modelo es confiable.

################################################################################
# MODELO DE REGRESIÓN LINEAL MÚLTIPLE PARA PREDECIR LA VARIABLE IMC
################################################################################
# 4. Haciendo un poco de investigación sobre el paquete caret, en particular cómo 
# hacer Recursive Feature Elimination (RFE), construir un modelo de regresión 
# lineal múltiple para predecir la variable IMC que incluya entre 10 y 20 
# predictores, seleccionando el conjunto de variables que maximice R2 y que 
# use cinco repeticiones de validación cruzada de cinco pliegues para evitar 
# el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura 
# ni estado nutricional –Weight, Height, EN respectivamente). 

# Se eliminan las columnas que no deben ser considerados
datos_imc <- datos %>% select(-c(Weight, Height, EN))

# Se separa la variable de respuesta de los demás predictores
IMC <- datos_imc[["IMC"]]
datos_imc[["IMC"]] <- NULL

# Se crea un conjunto de control
datos_control <- rfeControl(functions = lmFuncs, method="repeatedcv",
                            number = 5, repeats = 5, verbose = FALSE)

# Se crea el modelo, teniendo de 10 a 20 predictores.
modelo_imc <- rfe(datos_imc, IMC, rfeControl = datos_control, 
                  sizes = 10:20, metric = "Rsquared")

# Se imprime el modelo y las variables que se seleccionaron
print(modelo_imc)
cat("Variables seleccionadas: \n")
print(modelo_imc[["optVariables"]])

# Teniendo:
# "Gender"
# "Knees.diameter"         
# "Forearm.Girth"
# "Elbows.diameter"        
# "Calf.Maximum.Girth"
# "Ankles.diameter"        
# "Wrist.Minimum.Girth"
# "Waist.Girth"            
# "Biacromial.diameter"
# "Thigh.Girth"            
# "Biiliac.diameter"
# "Bicep.Girth"            
# "Bitrochanteric.diameter"
# "Knee.Girth"             
# "Ankle.Minimum.Girth"
# "Wrists.diameter"        
# "Hip.Girth"
# "Chest.Girth" 

print(modelo_imc)

print(ggplot(modelo_imc))

# Se tiene que el modelo se ajusta mejor con las 18 variables anteriores, teniendo
# un R² de 0,8694, así pudiendo concluir que el modelo explica la variabilidad 
# en un 86,94% de la variabilidad de los casos.

################################################################################
# MODELO DE REGRESIÓN LOGÍSTICA MÚLTIPLE PARA PREDECIR LA VARIABLE EN
################################################################################

# 5. Usando RFE, construir un modelo de regresión logística múltiple para la 
# variable EN que incluya el conjunto, de entre dos y seis, predictores que 
# entregue la mejor curva ROC y que utilice validación cruzada dejando uno 
# fuera para evitar el sobreajuste (obviamente no se debe considerar las 
# variables Peso, Estatura –Weight y Height respectivamente– ni IMC).

# Se descartan las columnas que no se pueden usar
datos_en <- muestra %>% select(-c(Weight, Height, IMC)) 

# Se separa la variable de respuesta de los demás predictores, anulándola
EN <- datos_en[["EN"]]
datos_en[["EN"]] <- NULL

lrFuncs$summary <- twoClassSummary

# Se crea el conjunto de datos para la construccion del modelo
construccion <- rfeControl(functions = lrFuncs, method = "LOOCV")
entrenamiento <- trainControl(method = "none", classProbs = TRUE,
                              summaryFunction = twoClassSummary)

# Se crea el modelo para predecir la variable EN con una cantidad de predictores
# mínimo de 2 y un máximo de 6.
modelo_en <- rfe(datos_en, EN, metric = "ROC", rfeControl = construccion,
                 trControl = entrenamiento, sizes = 2:6)

# Se muestra el modelo
print(modelo_en)

# Se consideran 4 variables (al tener el asterisco marcado ahí) y se tiene 
# que con las 4 variables:
# Biacromial.diameter
# Bitrochanteric.diameter
# Calf.Maximum.Girth
# Waist.Girth
# se puede predecir mejor el modelo.
# Con 4 variables el ROC es de 0,9632, siendo muy cercano a 1, por lo que se
# sugiere que tiene una muy buena capacidad discriminativa.
# Para el modelo seleccionado se tiene una sensibilidad del 94%, lo que indica
# que el modelo es muy bueno detectando la clase positiva.
# Además, se tiene una especificidad del 86%, lo que indica que es bueno evitando
# falsos positivos.

# Gracias al modelo se puede ver que el ROC se maximiza para cuando se tienen 4
# predictores, con un valor de 96,32%.
print(ggplot(modelo_en))

# Se evalúa la calidad predictiva del modelo
predicciones <- predict(modelo_en, datos_en)[["pred"]]
cat("La calidad predictiva del modelo es: ")
print(confusionMatrix(predicciones, EN))

# Con la matriz de confusión, se puede ver que el modelo predijo correctamente
# 47 como negativos y 3 incorrectamente como positivos.
# Además, predijo correctamente 47 como positivos y 3 incorrectamente como
# negativos.
# Se tiene una precisión del 94%, lo que significa que el 94% de las veces el
# modelo hizo una predicción correcta.
# El intervalo de confianza para la precisión es del 97,4% al 97,77%, lo que es
# bastante alto y hace que el modelo sea seguro.
# La tasa de no información es del 0.5, que sería la precisión de un modelo
# que siempre predice la clase más frecuente sin ninguna información.
# El valor estadístico Kappa es de 0.88, lo que significa que tiene una muy buena
# concordancia entre las predicciones y observaciones reales.
# El valor p de McNemar es 1, lo que indica que no hay una diferencia
# significativa en el rendimiento del modelo entre las clases positivas
# y negativas.
# Tanto la sensibilidad como la especificidad son del 94%, lo que significa que
# el modelo es igualmente bueno para identificar casos positivos y para evitar
# falsos positivos.
# El valor predictivo positivo y negativo son del 94%, lo que significa que cuando
# el modelo predice una clase, es correcto el 94% de las veces.
# La prevalencia tiene un valor del 0.5, lo que significa que la proporción
# de los casos positivos en el conjunto de datos es del 50%.
# La precisión equilibrada también es del 94%, lo que significa que el modelo es 
# equilibrado y tiene un buen rendimiento para ambas clases.
# Se indica que la clase '0' se considera la clase positiva para el cálculo de
# las estadísticas.

# CONCLUSIÓN GENERAL
# El modelo funciona excepcionalmente bien con el conjunto de datos al tener
# una alta precisión, sensibilidad y especificidad. 

# 6. Pronunciarse sobre la confiabilidad y el poder predictivo de los 
# modelos obtenidos. 
# HECHO EN CADA ÍTEM 

