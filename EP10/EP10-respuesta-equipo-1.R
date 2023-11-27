# Importar librerias
library(dplyr)

# 1. El equipo crea la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida por el cuadrado de su estatura (en metros).
# 2. Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional (bajo peso, normal, sobrepeso, obesidad, obesidad mórbida), para efectos de este ejercicio, usaremos dos clases: sobrepeso (IMC ≥ 25,0) y no sobrepeso (IMC < 25,0).
# 3. El equipo crea la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.

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

# Ahora podemos construir un modelo de regresión logística para predecir la 
# variable EN, de acuerdo con las siguientes instrucciones:
# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro 
# dígitos del RUN (sin considerar el dígito verificador) del integrante de mayor 
# edad del equipo.
set.seed(5547)

# 2. Seleccionar una muestra de 90 mujeres (si la semilla es un número par) o 
# 90 hombres (si la semilla es impar), asegurando que la mitad tenga estado 
# nutricional “sobrepeso” y la otra mitad “no sobrepeso” en cada caso. Dividir 
# esta muestra en dos conjuntos: los datos de 60 personas (30 con EN “sobrepeso”) 
# para utilizar en la construcción de los modelos y 30 personas (15 con EN 
# “sobrepeso”) para poder evaluarlos.

# Se filtran los datos para tener solo hombres
hombres <- datos[datos$Gender == 1, ]

# Se saca una muestra de 45 hombres que tengan sobrepeso
sobrepeso <- hombres %>% filter(EN == 1) %>% sample_n(45)
# Se saca una muestra de 45 hombres que no tengan sobrepeso
nosobrepeso <- hombres %>% filter(EN == 0) %>% sample_n(45)

# Se unen las dos muestras
muestra <- rbind(sobrepeso, nosobrepeso)

# Se unen las muestras donde se extraen 30 hombres con sobrepeso
# y 30 hombres sin sobrepeso
set_construccion <- rbind(
  sample_n(muestra %>% filter(EN == 1), 30),
  sample_n(muestra %>% filter(EN == 0), 30)
)

# Se saca la muestra restante tomando a la "muestra global" y a la 
# submuestra, restándose
set_evaluacion <- setdiff(muestra, set_construccion)

# 3. Recordar las ocho posibles variables predictoras seleccionadas de forma 
# aleatoria en el ejercicio anterior.

# VARIABLES
# Bitrochanteric.diameter
# Elbows.diameter
# Wrists.diameter
# Knees.diameter
# Waist.Girth
# Hip.Girth
# Knee.Girth
# Ankle.Minimum.Girth

# 4. Seleccionar, de las otras variables, una que el equipo considere que 
# podría ser útil para predecir la clase EN, justificando bien esta selección.
# Variable escogida: -> Navel.Girth
# Creemos que es determinante, ya que la mayoría de la grasa se almacena en 
# esta zona del cuerpo.

# 5. Usando el entorno R y paquetes estándares, construir un modelo de 
# regresión logística con el predictor seleccionado en el paso anterior y 
# utilizando de la muestra obtenida.

# Construir el modelo de regresión logística empleando la variable "Navel.Girth"
# con la muestra de construcción
modelo_logistico <- glm(EN ~ Navel.Girth,
                        family = binomial(link = "logit"), data = set_construccion)

# Resumen del modelo
summary(modelo_logistico)

# Realizar predicciones sobre el conjunto de evaluación
predicciones_prob <- predict(modelo_logistico, newdata = set_evaluacion, type = "response")
predicciones <- ifelse(predicciones_prob > 0.5, 1, 0)

# Crear una matriz de confusión para evaluar la sensibilidad y especificidad
matriz_confusion <- table(Prediccion = predicciones, Real = set_evaluacion$EN)

# Calculando sensibilidad y especificidad
sensibilidad <- matriz_confusion[1, 1] / sum(matriz_confusion[, 1])
especificidad <- matriz_confusion[0, 0] / sum(matriz_confusion[, 0])

# Imprimir los resultados
print(paste("Sensibilidad:", sensibilidad))
print(paste("Especificidad:", especificidad))

# 6. Usando herramientas estándares1 para la exploración de modelos del entorno 
# R, buscar entre dos y cinco predictores de entre las variables seleccionadas 
# al azar, recordadas en el punto 3, para agregar al modelo obtenido en el paso 5.
modelo_logistico_2 <- glm(EN ~ Navel.Girth + Bitrochanteric.diameter, 
                          family = binomial(link = "logit"), data = set_construccion)

# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de 
# ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún problema.


# 8. Usando código estándar1, evaluar el poder predictivo de los modelos con 
# los datos de las 40 personas que no se incluyeron en su construcción en 
# términos de sensibilidad y especificidad.


