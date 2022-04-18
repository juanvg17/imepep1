# Se cargan los paquetes necesario, en caso de no estar instalados se instalan.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

# Se carga el archivo de datos CSV.
población <- read.csv2(file.choose(new = FALSE), encoding = "UTF-8")

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


set.seed(844)
n.repeticiones <- 100
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)

vector <- 1:100

g <- factorial(n.repeticiones)/((factorial(vector))*(factorial(n.repeticiones - vector)))

plot(g)



points(x = sum(veinte.repeticiones == 1), y = factorial(n.repeticiones)/((factorial(sum(veinte.repeticiones == 1)))*(factorial(n.repeticiones - sum(veinte.repeticiones == 1)))))
