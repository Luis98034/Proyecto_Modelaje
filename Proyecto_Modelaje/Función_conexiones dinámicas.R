

# Este código es EXACTAMENTE IGUAL que el otro, a diferencia de una cosa
# para considerar que las conexiones no se mantienen estaticas con respecto
# al tiempo, lo que hacemos es que cada iteración, las conexiones cambien
# de manera aleatoria
SEIR_ID <- function(red, Beta, Gamma, Sigma, Infectados) {
  # SEIR_ID es por Interacciones Dinámicas
  # Función para calcular R0
  Calcular_Ro <- function(beta, gamma) {
    Ro <- beta / gamma
    return(Ro)
  }
  
 
  Ro <- Calcular_Ro(Beta, Gamma)
  cat("R0 =", Ro, "\n")
  

  if (Ro <= 1) {
    cat("R0 es menor o igual a 1. La enfermedad no se propagará.\n")
    return(NULL) # Detenemos la ejecución de la función
  } else {
    cat("La simulación continuará.\n")
  }
  
  # Función para imprimir el estado de la red
  Imprimir_red <- function(red) {
    colores <- c("s" = "green", "E" = "yellow", "I" = "red", "R" = "blue")
    colores_nodos <- colores[V(red)$estado]
    plot(red, vertex.color = colores_nodos, main = "Estado de la red", vertex.size = 10)
  }
  
  # Configuración inicial
  V(red)$estado <- "s"
  Primeros_infectados <- sample(V(red), Infectados)
  V(red)[Primeros_infectados]$estado <- "I"
  
  Transicion_de_estados <- data.frame(tiempo = integer(), estado = character(), cantidad = integer())
  
  # Simulación y generación del GIF
  saveGIF({
    Imprimir_red(red)
    
    Primeros_estados <- table(V(red)$estado)
    data_primeros_estados <- as.data.frame(Primeros_estados) %>%
      rename(estado = Var1, cantidad = Freq) %>%
      mutate(tiempo = 0)
    
    Transicion_de_estados <- bind_rows(Transicion_de_estados, data_primeros_estados)
    
    for (t in 1:100) {
      red <- rewire(red, each_edge(p = 0.1)) 
      # esto es lo unico nuevo que se le añade
      # a la función para la reestructuración dinámica de la red
      estado_anterior <- V(red)$estado
      
      for (nodo in 1:vcount(red)) {
        estado_actual <- estado_anterior[nodo]
        
        if (estado_actual == "s") {
          vecinos_infectados <- sum(estado_anterior[neighbors(red, nodo)] == "I")
          prob_infeccion <- 1 - (1 - Beta)^vecinos_infectados
          if (vecinos_infectados > 0 & runif(1) < prob_infeccion) {
            V(red)[nodo]$estado <- "E"
          }
        } else if (estado_actual == "E") {
          if (runif(1) < Sigma) {
            V(red)[nodo]$estado <- "I"
          }
        } else if (estado_actual == "I") {
          if (runif(1) < Gamma) {
            V(red)[nodo]$estado <- "R"
          }
        }
      }
      
      Imprimir_red(red)
      
      conteo <- table(V(red)$estado)
      conteo_data <- as.data.frame(conteo) %>%
        rename(estado = Var1, cantidad = Freq) %>%
        mutate(tiempo = t)
      Transicion_de_estados <- bind_rows(Transicion_de_estados, conteo_data)
    }
    
  }, movie.name = "simulacion_seir.gif", interval = 0.2, ani.width = 600, ani.height = 600)
  
  # Graficar la transición de los estados a lo largo del tiempo
  ggplot(Transicion_de_estados, aes(x = tiempo, y = cantidad, color = estado, group = estado)) +
    geom_line(size = 1) + 
    scale_color_manual(values = c("s" = "green", "E" = "yellow", "I" = "red", "R" = "blue")) + 
    labs(title = "Modelo SEIR con conexiones dinámicas",
         x = "Tiempo", y = "Población") +
    theme_minimal()  + transition_reveal(tiempo)
 
}


# Mismos ejemplos con las mismas redes


plot(red_23)
SEIR_ID(red_23, Beta=0.4, Gamma=0.2, Sigma=0.2, Infectados=50)


# Ejemplo con la red del club Zachary

plot(Karate)

SEIR_ID(Karate, 0.2, 0.05, 0.5, 1)


# Ejemplo con una red barabasi

plot(red44)

SEIR_ID(red44, 0.1, 0.05, 0.5, 5)




