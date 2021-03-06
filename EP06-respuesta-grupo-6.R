

# datos <- data.frame(Especialidad, Mujeres, Hombres)
# print(datos)
# 
# filtro1 <- datos %>% filter(Especialidad == "Oncolog�a" | Especialidad == "Dermatolog�a")
# 
# filtro1$Total <- rowSums(filtro1[,2:3])
# suma <- c("Total",colSums(filtro1[,2:4]))
# filtro1 <- rbind(filtro1,suma)
# #print(filtro1)
# 
# print(as.table(filtro1[1:2,2:3]))
# 
# prueba <- chisq.test(as.matrix(filtro1[1:2,2:3]))
# print(prueba)

# 1. Estudios previos hab�an determinado que la proporci�n de autoras en la especialidad de oncolog�a era de
# 32%. �Respaldan estos datos tal estimaci�n?


# 2. Seg�n estos datos, �es igual la proporci�n de autoras en las �reas de oncolog�a y dermatolog�a?
# uwu <- tabla %>% filter(c("Oncolog�a","Dermatolog�a"))



# 3. Suponiendo que la diferencia en la proporci�n de autoras en la especialidad de psiquiatr�a y la de obstetricia
# es de 0,18. �A cu�ntos autores deber�amos monitorear para obtener un intervalo de confianza del 99% y poder
# estad�stico de 90%, si se intenta mantener aproximadamente la misma proporci�n de gente estudiada en cada
# caso?

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}


Mujeres <- c(54, 71, 35, 30, 45, 44, 56, 21, 17)
Hombres <- c(52, 66, 41, 42, 65, 62, 88, 40, 35)
Especialidad <- c("Pediatr�a", "Obstetricia","Dermatolog�a",
                  "Psiquiatr�a", "Medicina Interna", "Oncolog�a",
                  "Neurolog�a", "Anestesiolog�a", "Radiolog�a")



datos <- data.frame(rbind(Mujeres, Hombres))
colnames(datos) <- Especialidad

print(datos)

datosO <- c(datos$Oncolog�a)
datosD <- c(datos$Dermatolog�a)

filtro1 <- data.frame(datosO, datosD)

print(filtro1)

prueba <- chisq.test(filtro1)

print(prueba)
