# 1.	Realice una función personalizada llamada “ValidaMorosidad” la cual reciba por parámetro un vector de 12 # datos numéricos que representen días de atraso en 7 operaciones financieras y con base en el promedio de esos # # valores, califique al cliente de la siguiente forma: 
# a.	Si el promedio es de 30 días o menos, indique “Morosidad Baja”.
# b.	Si esta entre 31 y 60 días, indique “Morosidad Media”.
# c.	Si esta entre 61 y 90 días, indique “Morosidad Alta”.
# d.	Si esta entre 91 y 120 días, indique “Morosidad muy Alta”.
# e.	Más de 120 días, indique “Morosidad Crítica”.
operacion1 <- c(1,1,1,1,1,1,1,1,1,1,1,1) 
operacion2 <- c(1,1,1,1,1,1,1,1,1,1,1,30)   
operacion3 <- c(1,1,1,1,1,1,1,1,1,1,1,60) 
operacion4 <- c(1,1,1,1,1,1,1,1,1,1,1,90)
operacion5 <- c(1,1,1,1,1,1,1,1,1,1,1,120) 
operacion6 <- c(1,1,1,1,1,1,1,1,1,1,1,120) 
operacion7 <- c(1,1,1,1,1,1,1,1,1,1,1,120) 

ValidaMorosidad <- function(dias) {
  sumaDias = sum(dias)
  mensaje = "";
  if(sumaDias <= 30 ){
    mensaje  = "Morosidad Baja"
  }else if(sumaDias >= 31 & sumaDias  <= 60 ){
    mensaje  = "Morosidad Media"
  }else if(sumaDias >= 61 & sumaDias <= 90 ){
    mensaje  = "Morosidad Alta"
  }
  else if(sumaDias >= 91 & sumaDias <= 120){
    mensaje  = "Morosidad muy Alta"
  }
  else{
    mensaje  = "Morosidad Critica"
  }
  return(mensaje)
}

ValidaMorosidad(operacion1)
ValidaMorosidad(operacion2)
ValidaMorosidad(operacion3)
ValidaMorosidad(operacion4)
ValidaMorosidad(operacion5)
ValidaMorosidad(operacion6)
ValidaMorosidad(operacion7)

# 2.	Se dispone de los siguientes datos: 1,3,4,2,3,2,4,4,5,3,2,1,2 y 5 que representa la consulta a varias    
# personas sobre su grado académico, donde 1=primaria, 2=secundaria, 3=técnico, 4=diplomado 5=Universitario. Cree # en R un vector de nombre grados con los valores anteriores. A partir de dicho vector, cree una variable llamada # “No Profesional” donde indique “Si” cuando el valor en el vector creado sea diferente de 5 y “Profesional” para # todos los demás. Para ello debe usar la función ifelse de R.
datos <- c(1,3,4,2,3,2,4,4,5,3,2,1,2,5) 
grados <- c("primaria" = 1,"secundaria" = 2,"tecnico" = 3,"diplomado" = 4,"universitario" = 5) 

consultaPersonas <- function(datos) {
  grados <- c("primaria" = 1,"secundaria" = 2,"tecnico" = 3,"diplomado" = 4,"universitario" = 5) 
  variableoNProfecional <- "No Profesional"
  variableProfecional <- "Profesional"
  for (i in datos) {
    if(i == grados['universitario']){
      print(variableProfecional) 
    }else{
      print(variableoNProfecional ) 
    }
  }
}
consultaPersonas(datos)

# 4 Realice un procedimiento que evalué un vector numérico del 1 al 150 y si el dato es múltiplo de 11 muestre el número, de lo contrario no muestre nada.
x <- 1:150  
ValidarNumeros <- function(parametro) {
  for(elemento in parametro){
     if(elemento %% 11 ==0){ 
      print(elemento)
     }
  }
}
ValidarNumeros(x)

#5/6.Desarrolle una función personalizada para que tome dos valores numéricos cualesquiera y obre ellos se puede #hacer las 4 operaciones básicas: a) suma, b) resta, c) división y d) multiplicación. La función debe contener 3 #argumentos para indicar: valor1, valor2 y operación
fnOperacion <- function(valor1, valor2, operacion) {
  switch(operacion, 
        "*" = valor1 * valor2, 
        "+" = valor1 + valor2, 
        "-" = valor1 - valor2, 
        "/" = valor1 / valor2) # se podria validar división por cero
}
fnOperacion (5,2, "*")
fnOperacion (5,2, "+")
fnOperacion (5,2, "-")
fnOperacion (5,2, "/")

# 7.	Desarrolle una función personalizada que reciba tres valores numéricos como parámetro y determine cual de 
# los tres números es menor, en caso de que se presenten dos números iguales menores, debe mostrar un mensaje de # indicando que hay números iguales.
fnDeterminarMayor<- function(valor1, valor2, valor3) {
  # asigno los valores que paso por parametro a un vector
  X <- c(valor1,valor2,valor3) 
  # Con la funcion sort ordeno, para asi no crear estructuras como if, ifelse.
  X  = sort(X)
  # el primer elemento es el menor.
  if(X[1] == X[2]){
    print("indicando que hay numeros iguales.")
  } 
  X[1]
}
fnDeterminarMayor(5,4,9)
fnDeterminarMayor(1,2,2)
fnDeterminarMayor(1,2,1)

# 8.	Desarrollo una función en R que reciba por parámetro el salario de un empleado, con base en el mismo 
# retorne el monto de aumento que se le debe aplicar tomando en consideración lo siguiente:
# a.	Salarios iguales o inferiores a 250.000 se les aplica un 15% de aumento.
# b.	Salarios superiores a 250.000 pero iguales o inferiores a 500.000 se les aplica un 10% de aumento.
# c.	Salarios superiores a 500.000 se les aplica un 5% de aumento.
fnDeterminarSalarios <- function(salario) {
  if(salario <= 250000){
    salario = salario  + (salario * 15) / 100
  } else if(salario > 250000 & salario < 500000){
    salario = salario  + (salario * 10) / 100   
  } else if(salario >= 500000 ){
    salario = salario  + (salario * 5) / 100   
  }
    return(salario)
}

fnDeterminarSalarios (250000) #287500
fnDeterminarSalarios (450000) #495000
fnDeterminarSalarios (550000) #577500