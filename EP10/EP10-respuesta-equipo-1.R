# Importar librerias


# 1. El equipo crea la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida por el cuadrado de su estatura (en metros).
# 2. Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional (bajo peso, normal, sobrepeso, obesidad, obesidad mórbida), para efectos de este ejercicio, usaremos dos clases: sobrepeso (IMC ≥ 25,0) y no sobrepeso (IMC < 25,0).
# 3. El equipo crea la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.



# Ahora podemos construir un modelo de regresión logística para predecir la variable EN, de acuerdo con las siguientes instrucciones:
# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito verificador) del integrante de mayor edad del equipo.
# 2. Seleccionar una muestra de 90 mujeres (si la semilla es un número par) o 90 hombres (si la semilla es impar), asegurando que la mitad tenga estado nutricional “sobrepeso” y la otra mitad “no sobrepeso” en cada caso. Dividir esta muestra en dos conjuntos: los datos de 60 personas (30 con EN “sobrepeso”) para utilizar en la construcción de los modelos y 30 personas (15 con EN “sobrepeso”) para poder evaluarlos.
# 3. Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en el ejercicio anterior.
# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase EN, justificando bien esta selección.
# 5. Usando el entorno R y paquetes estándares1, construir un modelo de regresión logística con el predictor seleccionado en el paso anterior y utilizando de la muestra obtenida.
# 6. Usando herramientas estándares1 para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo obtenido en el paso 5.
# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún problema.
# 8. Usando código estándar1, evaluar el poder predictivo de los modelos con los datos de las 40 personas que no se incluyeron en su construcción en términos de sensibilidad y especificidad.