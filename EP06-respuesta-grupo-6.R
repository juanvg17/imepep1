

# datos <- data.frame(Especialidad, Mujeres, Hombres)
# print(datos)
# 
# filtro1 <- datos %>% filter(Especialidad == "Oncología" | Especialidad == "Dermatología")
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

# 1. Estudios previos habían determinado que la proporción de autoras en la especialidad de oncología era de
# 32%. ¿Respaldan estos datos tal estimación?


# 2. Según estos datos, ¿es igual la proporción de autoras en las áreas de oncología y dermatología?
# uwu <- tabla %>% filter(c("Oncología","Dermatología"))



# 3. Suponiendo que la diferencia en la proporción de autoras en la especialidad de psiquiatría y la de obstetricia
# es de 0,18. ¿A cuántos autores deberíamos monitorear para obtener un intervalo de confianza del 99% y poder
# estadístico de 90%, si se intenta mantener aproximadamente la misma proporción de gente estudiada en cada
# caso?

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}


Mujeres <- c(54, 71, 35, 30, 45, 44, 56, 21, 17)
Hombres <- c(52, 66, 41, 42, 65, 62, 88, 40, 35)
Especialidad <- c("Pediatría", "Obstetricia","Dermatología",
                  "Psiquiatría", "Medicina Interna", "Oncología",
                  "Neurología", "Anestesiología", "Radiología")



datos <- data.frame(rbind(Mujeres, Hombres))
colnames(datos) <- Especialidad

print(datos)

datosO <- c(datos$Oncología)
datosD <- c(datos$Dermatología)

filtro1 <- data.frame(datosO, datosD)

print(filtro1)

prueba <- chisq.test(filtro1)

print(prueba)
