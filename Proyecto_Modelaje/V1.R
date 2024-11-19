writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron") 
# Esto yo lo cargo porque mi computadora es rara y no me deja cargar Rtools

library(igraph)
library(animation)
library(tidyverse)
library(dplyr)
library(gganimate)
library(ggplot2)
library(devtools)

#  https://cienciadedatos.net/documentos/57_gif_con_r.html
# https://www.youtube.com/watch?v=ikS1lhAzKWo

# Para comenzar, primero creamos una red aleatoria de 100 nodos con conexiones 
# de 0.15, aunque la red puede ser cualquiera, siempre y cuando esta no sea 
# dirigida
red <- random.graph.game(100, 0.15)

# Antes de comenzar con los cambios en los estados de los nodos, primeoro 
# definimos una función para asignarle un color aespecifico a cada estado posible
# dentro de la enfermedad
Imprimir_red <- function() {
  colores <- c("s" = "green", "E" = "yellow", "I" = "red", "R" = "blue")
  colores_nodos <- colores[V(red)$estado] 
  plot(red, vertex.color = colores_nodos, main = "Estado de la red", vertex.size = 10)
}

# Asignamos los valores de las tasas en un vector
Tasas <- c(Beta = 0.5, Sigma = 0.1, Gamma = 0.2)

# Primero le asignamos el estado de susceptibles a todos los nodos, y luego 
# sampleamos 5 nodos al azar para definirlos como los primeros infectados en la 
# población
V(red)$estado <- "s"
Primeros_infectados <- sample(V(red), 5)
V(red)[Primeros_infectados]$estado <- "I"
V(red)$estado

# Como queriamos ver como es la dinamica de transmisión de la enfermedad, usamos
# la función GIFT, del paquete animate 
# https://cienciadedatos.net/documentos/57_gif_con_r.html

Transicion_de_estados <- data.frame(tiempo = integer(), estado = character(), cantidad = integer())

saveGIF({
#  Al definir las reglas del modelo, el cambio entre estados era muy rapido, 
#  entonces por eso mejor primero imprimimos la red original, antes de que se vean
# los cambios en el sistema
  
Imprimir_red()  # Esto muestra el estado inicial
  
# Antes de comenzar con nuestro ciclo for, primero tenemos que definir como queremos
# que se vayan guardando las cantidaades de los estados de cada compartimento,
# para esto necesitamos las libreria de tidyverse y de dplyr.
  
  # Dado que no podemos usar el pipes %>%, en un objeto igraph directamente, lo que se tiene
  # que hacer es pasar dicho objeto a un dataset. Para esto primero usamos la función table,
  # para que nos desglose la cantidad de nodos que presentan cierto estado, y luego
  # ya se le pone como as.data.frame. Cuando eso se imprime, salen columnas con los
  # nombres var1 y freq, entonces se los cambiamos, y ya solo agregamos una nueva
  # columna para que nos digan en que tiempo es que está esa cantidad de nodos con 
  # su respectivo estado
  
  Primeros_estados <- table(V(red)$estado)
  data_primeros_estados <- as.data.frame(Primeros_estados) %>% 
    rename(estado = Var1, cantidad = Freq) %>% 
    mutate(tiempo = 0)
  # Con el bind_rows, vamos juntando nuevas filas de los diferentes estados de los
  # nodos en los demás tiempos, pero como aun no se ejecuta el ciclo, 
  # solo va a tener los del tiempo 0
  Transicion_de_estados <- bind_rows(Transicion_de_estados, data_primeros_estados)
  
  
# Para empezar con la simulación, vamos a generar nuestros ciclos for, para
# comenzar a ver los estados de cada uno de nuestros nodos presentes
  for (t in 1:100) {
    estado_anterior <- V(red)$estado
    
    # Dinámica del modelo SEIR
    for (nodo in 1:vcount(red)) {
      estado_actual <- estado_anterior[nodo]
# En dado caso de que el estado actual del nodo se encuentre en el estado
# de susceptible, se van a generar dos objetos, uno llamado
# vecinos infectados, que guarda la cantidad de nodos vecinos a los cuales 
# nuestro nodo está conectado. El otro objeto es el de la probabilidad de infección
# que toma en cuenta la tasa Beta y los vecinos infectados, para definir la probabilidad
# de que un nodo susceptible pase al estado de expuesto
      if (estado_actual == "s") {
        vecinos_infectados <- sum(estado_anterior[neighbors(red, nodo)] == "I")
        prob_infeccion <- 1 - (1 - Tasas["Beta"])^vecinos_infectados
        if (vecinos_infectados > 0 && runif(1) < prob_infeccion) {
# En caso de que la probabilidad de infección sea mayor al runif, y que los vecinos
# infectados sean más que 0, entonces el estado de nuestro nodo pasa a expuesto
          V(red)[nodo]$estado <- "E"
        }
# En dado caso de que nuestro nodo se encuentre como Expuesto, si la tasa sigma
# es mayor al runif, entonces este pasa al estado de Infectado
      } else if (estado_actual == "E") {
        if (runif(1) < Tasas["Sigma"]) {
          V(red)[nodo]$estado <- "I"
        }
# Si el nodo se encuentra infectado, si la tasa Gamma es mayor al runif, entonces
# este se recupera
      } else if (estado_actual == "I") {
        if (runif(1) < Tasas["Gamma"]) {
          V(red)[nodo]$estado <- "R"
        }
      }
    }
    
    # Al final, como se pone esta función, después del todo el ciclo for, te va 
    # a imprimir todos los estados de la red, pero al estar todo dentro de la 
    # función de saveGIFT, se van a guardar todos esos estados en el gift
    Imprimir_red()
    
    
    # Esto es para recuperar los conteos de la cantidad de nodos con diferentes
    # estados en cada unno de los diferentes tiempos, de igual manera, se usa
    # la función table y se guarda como un dataset, luego eso datos se van juntando 
    # en el objeto de Transicion de estados, de tal manera que vamos a tener muchas
    # filas, donde en la columna de tiempo, nos va a decir que cantidad y estado de cada
    # nodo corresponde a cada tiempo
    conteo <- table(V(red)$estado)
    conteo_data <- as.data.frame(conteo) %>% 
      rename(estado = Var1, cantidad = Freq) %>% 
      mutate(tiempo = t)
    Transicion_de_estados <- bind_rows(Transicion_de_estados, conteo_data)
  }
}, movie.name = "simulacion_seir.gif", interval = 0.2, ani.width = 600, ani.height = 600)

#Al final, igual dentro de la función del gift, solo ponemos más parametros

# Al final, solo usamos ggplot2, para poder graficar los estados de cada nodo 
# en fucnión al tiempo, de manera parecida que con deSolve

ggplot(Transicion_de_estados, aes(x = tiempo, y = cantidad, color = estado, group = estado)) +
  geom_line(size = 1) +  # Dibuja las líneas
  scale_color_manual(values = c("s" = "green", "E" = "yellow", "I" = "red", "R" = "blue")) +  # Asigna colores a los estados
  labs(title = "Modelo SEIR con conexiones estáticas",
       x = "Tiempo", y = "Cantidad de nodos") +
  theme_minimal() #+ transition_reveal(tiempo)





#------Esto es solo para ver si las cantidades de infectados en el
# pico maximo si coinciden
DeterminaImax <- function(beta, gamma, poblacion){
  Ro <- beta/gamma
  fraccion <- 1- ((1 + (log(Ro)))/Ro)
  infectados <- fraccion * poblacion
  cat("La cantidad de infectados Max es", infectados, "aproximadamente")
}

DeterminaImax(0.5, 0.2, 100)

      # Para ver si la determinación de infectados maxima
                                # coincide con la de la formula

Transicion_de_estados %>%
  filter(estado == "I") %>%
  slice(which.max(cantidad))


devtools::install_github("Nertekkad/mlBioNets") 

library(mlBioNets) 

ls(getNamespace("mlBioNets"), all.names = TRUE)
citation("mlBioNets")

