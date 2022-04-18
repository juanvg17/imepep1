# Actividad 3 IME
# Grupo 6: Angel Avendaño, Jorge González y Juan Vásquez

# Se cargan los paquetes necesario, en caso de no estar instalados se instalan.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

# Se carga el archivo de datos CSV.
población <- read.csv2(file.choose(new = FALSE), encoding = "UTF-8")

# Se copia el codigo del documento.
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(700) # ----- PREGUNTA 1 -----
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

# ----- PREGUNTA 2 -----

# Se estandariza la distribución normal (media 0, desviación 1).
ingreso.estandar <- (ingreso.normal - media.ingreso) / sd.ingreso

# Se muestran el promedio y la desviación estándar por consola.
mean(ingreso.estandar)
sqrt (sum((ingreso.estandar - mean(ingreso.estandar))^2) / 5000 )

# ----- PREGUNTA 3 -----

# Construcción de las dos distribuciones ??2, cada una con 4 y 7 grados de libertad.

# 5 grados de libertad
Libertad5 <- sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2

#Garfico
hist(rchisq(5000,5))
#para comprobar
hist(Libertad4)

# 7 grados de libertad
Libertad7 <- sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2 + sample(ingreso.estandar, 5000)^2
#Garfico 
hist(Libertad7)
#para comprobar
hist(rchisq(5000,7))


# ----- PREGUNTA 4 -----
#. Usando las dos distribuciones ??2 generadas en el punto anterior, construyan una distribución F.
# Construcción de la distribución F

distribuciónF <- (Libertad5 / 5) / (Libertad7 / 7)
plot(density(distribuciónF))

#para comprobar con metodo de R
plot(density(rf(5000, 5, 7)))







