DeterminaImax <- function(beta, gamma, poblacion){
  Ro <- beta/gamma
  fraccion <- 1- ((1 + (log(Ro)))/Ro)
  infectados <- fraccion * poblacion
  cat("La cantidad de infectados Max es", infectados, "aproximadamente")
}   # Función ajustada a la formula para determinar la cantidad maxima de 
    # infectados posible



SEIR_IE <- function(red, Beta, Gamma, Sigma, Infectados) {
  #SEIR IE es por Interacciones estáticas
  # Función para calcular R0
  Calcular_Ro <- function(beta, gamma) {
    Ro <- beta / gamma
    return(Ro)
  }
  
  # Cálculo de R0
  Ro <- Calcular_Ro(Beta, Gamma)
  cat("R0 =", Ro, "\n")
  
  # Validación de R0
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
  
  # Definimos nnuestros estados iniciales
  V(red)$estado <- "s"
  Primeros_infectados <- sample(V(red), Infectados)
  V(red)[Primeros_infectados]$estado <- "I"
  
  Transicion_de_estados <- data.frame(tiempo = integer(), estado = character(), cantidad = integer())
  
  # Comenzamos con la siulación
  saveGIF({
    Imprimir_red(red)
    
    Primeros_estados <- table(V(red)$estado)
    data_primeros_estados <- as.data.frame(Primeros_estados) %>%
      rename(estado = Var1, cantidad = Freq) %>%
      mutate(tiempo = 0) #Para guardar los datos iniciales
    
    Transicion_de_estados <- bind_rows(Transicion_de_estados, data_primeros_estados)
    
    for (t in 1:100) {
      estado_anterior <- V(red)$estado
      
      for (nodo in 1:vcount(red)) {
        estado_actual <- estado_anterior[nodo]
        
        if (estado_actual == "s") {
          vecinos_infectados <- sum(estado_anterior[neighbors(red, nodo)] == "I")
          prob_infeccion <- 1 - (1 - Beta)^vecinos_infectados
          if (vecinos_infectados > 0 & runif(1) < prob_infeccion) {
            V(red)[nodo]$estado <- "E"
          }
        } else if (estado_actual == "E") {  # Transición entre los
          if (runif(1) < Sigma) {           # diferentes estados
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
        rename(estado = Var1, cantidad = Freq) %>% # Guardamos los datos de las
                                                   # transiciones de estados
        mutate(tiempo = t)
      Transicion_de_estados <- bind_rows(Transicion_de_estados, conteo_data)
    }
    
  }, movie.name = "simulacion_seir.gif", interval = 0.2, ani.width = 600, ani.height = 600)
  
  #Con esto vemos la cantidad máxima de infectados
  # en el modelo
  
  
  # Graficamos a la población y como cambia de compartimento con respecto
  # al tiempo
  ggplot(Transicion_de_estados, aes(x = tiempo, y = cantidad, color = estado, group = estado)) +
    geom_line(linewidth = 1) + 
    scale_color_manual(values = c("s" = "green", "E" = "yellow", "I" = "red", "R" = "blue")) + 
    labs(title = "Modelo SEIR con conexiones estáticas",
         x = "Tiempo", y = "Población") +
    theme_minimal()   + transition_reveal(tiempo)
                  # Esto ocupamucha memoria
 
}                         

##-------------Ejemplos con la función-------------------
red_23 <- random.graph.game(100, 0.15)
plot(red_23)
SEIR_IE(red_23, Beta=0.4, Gamma=0.2, Sigma=0.2, Infectados=50)


# Ejemplo con la red del club Zachary
Karate<-make_graph("Zachary")
plot(Karate)
vcount(Karate
       )
SEIR_IE(Karate, 0.2, 0.05, 0.5, 1)
DeterminaImax(0.2, 0.05, 34)  #Si varia

# Ejemplo con una red barabasi
red44 <- sample_pa(100, directed = F)
plot(red44)

SEIR_IE(red44, 0.1, 0.05, 0.5, 5)
DeterminaImax(0.1, 0.05, 100)
