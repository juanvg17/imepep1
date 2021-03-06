# Se sabe que una m�quina que envasa detergentes industriales llena bidones con un volumen de producto que
# sigue una distribuci�n normal con desviaci�n est�ndar de 1 litro. Usando una muestra aleatoria de 100 botellas,
# el ingeniero a cargo de la planta requiere determinar si la m�quina est� llenando los bidones con una media de
# 10 litros

##################
#Error tipo I: rechazar H0 en favor de HA cuando H0 es en realidad verdadera.
#Error tipo II: no rechazar H0 en favor de HA cuando HA es en realidad verdadera
##################
library (ggpubr)
library (pwr)

# Definir paleta de colores para gráficos.
colores <- hcl(h = (seq(15, 255, length.out = 3)), c = 100, l = 65)
desv_estandar <- 1
mu_real <- 10
n <- 100


# 1. Si el ingeniero est� seguro de que el verdadero volumen medio no puede ser superior a 10 litros y
# piensa rechazar la hip�tesis nula cuando la muestra presente una media menor a 9,5 litros, �cu�l es la probabilidad
# de que cometa un error de tipo I?

# tenemos  una var=1 ; 

# exito = es HA

# H0: � = �0, esto es � = 10 litros
# HA: � < �0, esto es � < 10 litros

#funcion de distribuci�n acumulada (es decir, la probabilidad de que la variable tome valores menores 
#o iguales que un valor dado
alfa_izq <- pnorm(9.5, mean = 10, sd = 1, lower.tail = TRUE)

# 2. Si el verdadero volumen medio de los bidones fuera de 9,7 litros, �cu�l ser�a la probabilidad de que el
# ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?

#Error tipo II: no rechazar H0 en favor de HA cuando HA es en realidad verdadera
#se busca el "beta"


# H0 : � = 10 litros
# HA : � = 9.7 litros

# Calcular el error est�ndar .
SE <- desv_estandar/sqrt (n)

mu_verdadera <- 9.7

# Definir cantidad de puntos a usar en el eje x.
numero_puntos <- 100

# Gr�ficar la distribuci�n muestral de la media de las diferencias si  la hip�tesis nula fuera verdadera

# Generar una distribución t en torno al valor nulo.
x <- seq(mu_verdadera - 5.2*SE, mu_verdadera + 5.2*SE , length.out = numero_puntos)
y <- dnorm (x , mean = mu_verdadera, sd = SE)
distr1 <- data.frame(x, y)


# Graficar la distribuci�n
g <- ggplot (data = distr1, aes (x))

# - Agregar la distribuci�n normal.
g <- g + stat_function(
     fun = dnorm,
     args = list (mean = mu_verdadera, sd = SE) ,
     colour = "red", size = 1)

# - Quitar etiquetas del eje y.  
g <- g + ylab ("")

# - Quitar marcas del y.
g <- g + scale_y_continuous (breaks = NULL)

# - Agregar marcas y etiquetas al eje x.
g <- g + scale_x_continuous(name = "volumen [litros]",breaks = seq (5,15,2))
# - Dar formato con fondo blanco.
g <- g + theme_pubr()

# - Rotar etiquetas del eje x.
g <- g + theme(axis.text.x = element_text(angle = 30, size = 10))

# - Agregar la media bajo la hipotesis nula.
g <- g + geom_vline(xintercept = mu_verdadera,
                      colour = "red", linetype = "longdash")

# - Agregar titulo
g <- g + ggtitle("Distribuci�n de las medias muestrales bajo la hipotesis nula")

print(g)

# Colorear la regi�n de rechazo de la hip�tesis nula .
Z_critico <- pnorm (0.05 , mean = mu_verdadera , sd = SE , lower.tail = FALSE)
q_critico_inferior <- mu_verdadera - Z_critico


g2 <- g + geom_area(data = subset(distr1, x = mu_verdadera), aes(y = y),
                    colour = "red", fill = "red", alpha = 0.5)

g2 <- g2 + ggtitle("Pregunta 1")

print(g2)




# 3. Como no se conoce el verdadero volumen medio, genere un gr�fico del poder estad�stico con las condiciones
# anteriores, pero suponiendo que el verdadero volumen medio podr�a variar de 9,3 a 10 litros.


# 4. Considerando un volumen medio de 10 litros, �cu�ntos bidones deber�an revisarse para conseguir un poder
# estad�stico de 0,8 y un nivel de significaci�n de 0,05?

# Se busca el tama�o de la muestra para conseguir los valores estipulados para los factores de la prueba:
# alfa = 0,05 y poder = 0.8.

mu_0 <- 9.7

# Calcular tama�o del efecto (d de Cohen).
efecto <- (mu_real - mu_0) / desv_estandar

power.t.test(delta = efecto, sd = 1, sig.level = 0.05, power = 0.8, type = "one.sample", alternative = "one.sided")


#   5. �Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1%
# solamente?


# 4
# Considerando un volumen medio de 10 litros, �cu�ntos bidones deber�an revisarse para conseguir un poder
# estad�stico de 0,8 y un nivel de significaci�n de 0,05?

power.t.test(delta = 0.3, sd = 1, sig.level = 0.05,
             power =0.8, type = "one.sample",alternative = "one.sided")
# Respuesta: 


#5
# �Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de 
# tipo I a un 1% solamente?

power.t.test(delta = 0.3, sd = 1, sig.level = 0.01,
             power =0.8, type = "one.sample",alternative = "one.sided")
# Respuesta: 

