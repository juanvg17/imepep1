# Actividad 3 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez

# Se cargan los paquetes necesario, en caso de no estar instalados se instalan.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

# Se carga el archivo de datos CSV.
poblaci�n <- read.csv2(file.choose(new = FALSE), encoding = "UTF-8")

# Se copia el codigo del documento.
tama�o <- nrow(poblaci�n)
ingreso <- as.numeric(poblaci�n[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tama�o.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tama�o.podado )
set.seed(700) # ----- PREGUNTA 1 -----
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

# ----- PREGUNTA 2 -----

# Se estandariza la distribuci�n normal (media 0, desviaci�n 1).
ingreso.estandar <- (ingreso.normal - media.ingreso) / sd.ingreso

# Se muestran el promedio y la desviaci�n est�ndar por consola.
mean(ingreso.estandar)
sqrt (sum((ingreso.estandar - mean(ingreso.estandar))^2) / 5000 )

# ----- PREGUNTA 3 -----

# Construcci�n de las dos distribuciones ??2, cada una con 4 y 7 grados de libertad.

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
#. Usando las dos distribuciones ??2 generadas en el punto anterior, construyan una distribuci�n F.
# Construcci�n de la distribuci�n F

distribuci�nF <- (Libertad5 / 5) / (Libertad7 / 7)
plot(density(distribuci�nF))

#para comprobar con metodo de R
plot(density(rf(5000, 5, 7)))







