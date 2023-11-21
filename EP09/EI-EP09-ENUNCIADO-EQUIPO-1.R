# Importar librerías
library(ggpubr)
library(boot) # Para bootstrapping
library(dplyr) # Para manipulación de datos
library(multcomp) # Para comparaciones post-hoc
library(ez)
library(car)

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos 
# del RUN (sin considerar el dígito verificador) del integrante de menor edad del equipo.
set.seed(5689)

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 
# hombres (si la semilla es impar).
datos <- read.csv2("EP09 Datos.csv", fileEncoding = "UTF-8")

head(datos)

hombres <- datos[datos$Gender == 1, ]
muestra_hombres <- sample_n(hombres, 50)

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.
# Bitrochanteric.diameter
# Elbows.diameter
# Wrists.diameter
# Knees.diameter
# Waist.Girth
# Hip.Girth
# Knee.Girth
# Ankle.Minimum.Girth

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría 
# ser útil para predecir la variable Peso, justificando bien esta selección.
# Thigh.Girth: Debido a que en esta zona se encuentra el músculo más grande del cuerpo,
# se acumula bastante grasa y puede ser una buena variable para predecir el peso.

# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el 
# predictor seleccionado en el paso anterior.
modelo <- lm(Weight ~ Thigh.Girth, data = muestra_hombres)
print(summary(modelo))

# Revisar variabilidad que sea distinta de 0

# Residuos homocedásticos (varianzas similares)
car::ncvTest(modelo)

# Residuos deben seguir una distribución cercana a la normal centrada en cero

# 6. Usando herramientas para la exploración de modelos del entorno R, buscar entre 
# dos y cinco predictores de entre las variables seleccionadas al azar en el punto 
# 3, para agregar al modelo de regresión lineal simple obtenido en el paso 5.
# Waist.Girth
# Hip.Girth

cat("#### Modelo inicial ####")
print(modelo)

modelo <- update(modelo, . ~ . + Waist.Girth)
cat("#### Modelo con predictor Waist.Girth ####")
print(modelo)

cat("#### Modelo con predictor Hip.Girth ####")
modelo <- update(modelo, . ~ . + Hip.Girth)
print(modelo)

cat("#### Modelo con predictor Wrist.diameter ####")
modelo <- update(modelo, . ~ . + Wrists.diameter)
print(modelo)

cat("#### Modelo con predictor Knees.diameter ####")
modelo <- update(modelo, . ~ . + Knees.diameter)
print(modelo)

cat("#### Modelo con predictor Ankle.Minimum.Girth ####")
modelo <- update(modelo, . ~ . + Ankle.Minimum.Girth)
print(modelo)

# No debe existir multicolinealidad
vifs <- vif(modelo)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs:\n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

# Independencia de los residuos 
cat("Prueba de Durbin-Watson para autocorrelaciones")
cat("entre errores:\n")
print(durbinWatsonTest(modelo))


# Comprobar normalidad de los residuos
cat("\nPrueba de normalidad para los residuos:\n")
print(shapiro.test(modelo$residuals))


# Comprobar homocedasticidad de las residuos
cat("Prueba de homocedasticidad de los residuos:\n")
print(ncvTest(modelo))

# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con 
# las condiciones que deben cumplir.

# En una primera instancia ocupamos 2 variables la cuales fueron Waist.Girth y 
# Hip.Girth pero solo con estas no se estaba cumpliendo la condicion de 
# homocedasticidad por lo que agregamos  Wrists.diameter, Knees.diameter y 
# Ankle.Minimum.Girth y con estas nuevas varaibles ya estabamos cumpliendo todas
# las condiciones

# 8. Evaluar el poder predictivo del modelo en datos no utilizados para construirlo 
# (o utilizando validación cruzada).

n <- nrow(datos)
n_entrenamiento <- floor(0.7 * n)

muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

mse_entrenamiento <- mean(modelo$residuals ** 2)

predicciones <- predict(modelo, prueba)

error <- prueba[["Thigh.Girth"]] - predicciones
mse_prueba <- mean(error ** 2)

cat(mse_prueba)
