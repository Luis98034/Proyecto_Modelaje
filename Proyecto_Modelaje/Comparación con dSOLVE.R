# usamos el script que ya estaba hecho de base para un modelo SIR cuando vimos deSolve, solo
# que lo ajustamos para un modelo SEIR, por lo tanto este código de qui, es exactamente igual 
# al de la clase de cuando vimos dSolve, solo está en forMa de función para que uno escoja los
# parametros y sea más fácil esto para comparar nadamás con los de nuesta función.  
library(deSolve)
  library(dplyr)

simular_SEIR <- function(beta, sigma, gamma, poblacion, infectados_iniciales, tiempo_simulacion = 100) {
  
  
  # Modelo SEIR
  SEIR <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), { 
      dS <- -beta * S * I
      dE <- beta * S * I - sigma * E # Definimos las ecuaciones diferenciales para cada estado de cada individuo
                                     # dentro de luna población  
      dI <- -gamma * I + sigma * E
      dR <- gamma * I
      list(c(dS, dE, dI, dR))
    })
  }
  
  # En los parametros de la función se pone cuanto es de población
  # y cuantos infectados hay, entonces, como los que no están infectados
  # son susceptibles, solo se resta
  susceptibles_iniciales <- poblacion - infectados_iniciales
  condiciones_iniciales <- c(S = susceptibles_iniciales, E = 0, 
                             I = infectados_iniciales, R = 0)
  pars <- c(beta = beta, sigma = sigma, gamma = gamma)
  tiempo <- seq(0, tiempo_simulacion, by = 1)
  
  # Todo esto es lo mismo del script de la clase 
  out <- ode(condiciones_iniciales, tiempo, SEIR, pars)
  
 
  matplot(out[, 1], out[, 2:5], type = "l", xlab = "Tiempo", ylab = "Población",
          main = "Modelo SEIR", lwd = 2, col = 1:4, lty = 1:4)
  legend("topright", legend = c("Susceptibles", "Expuestos", "Infectados", "Recuperados"),
         col = 1:4, lty = 1:4, cex = 0.8)
  
  # Esto ya es agregado, solo le acomodamos esto para ver cuantos infectados 
  # como maximo se ven
  datos_estados <- as.data.frame(out)
  
  # Con los pipes filtramos
  max_infectados <- datos_estados %>%
    select(t = time, I) %>%
    slice(which.max(I))
  
  # Imprimimos la fila del valor maximo
  cat("Máximo número de infectados:\n")
  print(max_infectados)
  return(list(simulacion = datos_estados, max_infectados = max_infectados))
}

# Ejemplo


#---Con la de karate
SEIR_ID(Karate, 0.2, 0.05, 0.5, 1)
SEIR_IE(Karate, 0.2, 0.05, 0.5, 1)
# La red de karate tiene 34 nodos, que son nuestra población
# Consideramos un infectado
# Beta = 0.2
# sigma  = 0.5
# gamma = 0.05
resultados <- simular_SEIR(
  beta = 0.2, 
  sigma = 0.5, 
  gamma = 0.05, 
  poblacion = 34, 
  infectados_iniciales = 1, 
  tiempo_simulacion = 100
)  #se ven medio similares
