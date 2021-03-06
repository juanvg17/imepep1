if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

# 1. Estudios cient�ficos han descubierto que la inteligencia musical est� altamente relacionada con la inteligencia
# matem�tica. Pensando en mejorar la oferta de actividades culturales y recreativas, una Universidad ha
# examinado la preferencia de talleres de un grupo de 8 estudiantes de carreras cient�ficas y 11 de carreras
# humanistas, encontrando que 6 de los primeros y 2 de los segundos participaron de talleres musicales. �Existe
# relaci�n entre el tipo de carrera que cursan los estudiantes y su participaci�n en talleres musicales?


#   2. Un pol�mico estudio realizado con 12 pares de gemelos abandonados por sus padres, donde uno de los
# gemelos fue adoptado por una familia funcional y el otro creci� en un centro del estado, ha encontrado que,
# al llegar a la edad adulta:
#   ??? Solo en una pareja de gemelos, ambos presentaron trastornos psicol�gicos.
# ??? En dos casos, solo el gemelo adoptado present� trastornos psicol�gicos.
# ??? En 5 de los casos, solo el gemelo que creci� en el centro estatal present� trastornos psicol�gicos.
# ??? En los casos restantes, ambos gemelos est�n libres de problemas psicol�gicos.
# �Influye el entorno en que un ni�o crece en la aparici�n de trastornos psicol�gicos en la edad adulta?

#   3. El 21 de marzo de 2022 se realiz� un estudio acerca de la aprobaci�n al presidente Gabriel Boric entre los
# estudiantes de una prestigiosa universidad a fin de comparar los resultados con los obtenidos en la misma
# encuesta a nivel nacional, obteni�ndose los resultados que se muestran en la tabla. �Refleja la opini�n
# estudiantil la percepci�n del pa�s?



# -------------------- PREGUNTA 1 ----------------------------------

# H0 : Hay independencia tipo de carrera que cursan los estudiantes y su participaci�n en talleres musicales.
# H1 : Hay una relaci�n entre el tipo de carrera que cursan los estudiantes y su participaci�n en talleres musicales.

# Denotando como p como la proporci�n de autoras en las especialidad de oncolog�a:

# H0 : p = p0, esto es p =  30 % de autoras en la especialidad de oncolog�a.
# H1 : p != p0, esto es p != 30 % de autoras en la especialidad de oncolog�a.

# participa <- c(6,2)
# noParticipa <- c(2,9)
# 
# tabla <- as.table(rbind(participa, noParticipa))
# 
# dimnames(tabla) <- list(talleres = c("Participa", "No Participa"),
#                         carrera = c("Cientifica", "Humanista"))
# print(tabla)
# 
# prueba <- chisq.test(tabla)
# esperados <- round(prueba[["expected"]], 3)
# print(esperados)
# print(prueba)


# Construir la tabla de contingencia .
carrera <- c(rep ("Cientifica", 8), rep("Humanista", 11))
talleres <- c(rep("Participa", 8), rep("No Participa", 11))
datos <- data.frame(talleres, carrera)
tabla <- xtabs(~., datos)
print (tabla)

# Aplicar prueba exacta de Fisher .
alfa <- 0.05
prueba <- fisher.test(tabla, 1 - alfa)
print(prueba)

# -------------------- PREGUNTA 2 ----------------------------------

# H0 : El entorno en el que el ni�o crece no influye en los trastornos psicologicos que desarrolla en la adultez
# HA : El entorno en el que el ni�o crece influye en los trastornos psicologicos que desarrolla en la adultez

entorno <- c(rep("Estado", 12), rep("Familia", 12))
trastorno <- c(rep("Presenta", 9), rep("No Presenta", 15))

datos1 <- data.frame(entorno, trastorno)
tabla <- xtabs(~., datos1)
print(tabla)

alfa <- 0.05
prueba1 <- fisher.test(tabla, 1- alfa)
print(prueba1)

# -------------------- PREGUNTA 3 ----------------------------------

# Construir la tabla de contingencia .
Voto <- c(rep ("Aprueba", 5171), rep("Desaprueba", 3498), rep("Ninguna", 727))
Sujetos <- c(rep("Estudiantes", 223), rep("Nacional", 9173))
datos3 <- data.frame(Sujetos, Voto)
# print(table(datos$Sujetos, datos$Voto, dnn = c("Sujeto", "Voto")))
tabla3 <- xtabs(~., datos3)
print (tabla3)

# Aplicar prueba exacta de Fisher .
alfa <- 0.05
prueba3 <- fisher.test(tabla3, 1 - alfa, alternative = "two-sided")

# Respuesta: se rechaza h0

# -------------------- PREGUNTA 4 ----------------------------------
archivo <- read.csv2(file.choose(new = FALSE))
set.seed(453)
alpha <- 0.05
muestra <- sample_n(archivo, 50)