library(igraph)
library(ggplot2)
library(animation)
library(ranger)
library(viridis)
library(deSolve)


###############################################
#                                             #
# Red inicial                                 #
#                                             #
 Adj <- barabasi.game(500, directed = FALSE)  #                                         #
#                                             #
###############################################


Infectious.Network <- function(x, StateConfig, StateDays, StartNode, R0, Colors, Days) {  
  # x objeto listo para plotear de igraph (la red)
  # StateConfig: c(S, I, R) o  c(S, E, I, R)
  # Colors: c(Color1, Color2, Color3) o c(Color1, Color2, Color3, Color4)
  # StateDays: ¿Cuantos días permanece una persona en ese estado?
  # c(0, #, #, #)
  # StartNode: ¡Quien(es) va(n) a ser el primer infeccioso?
  # ¡Cuantos días quieres que itere? ###
  # ¿Quieres que haga los plots? T o F
  # length(StateConfig) == length(Colors) == length(StateDays)
  
  
  # Hace runa base de datos que tenga el conteo de los estados por dia
  if (length(StateConfig) == 3){
    Summary <- data.frame(0, 0, 0, 0)
    colnames(Summary) <- c("S", "I", "R", "Day")
  } else if (length(StateConfig) == 4) {
    Summary <- data.frame(0, 0, 0, 0, 0 )
    colnames(Summary) <- c("S", "E", "I", "R", "Day")
  }
  
  
  
  # Para los nombres de los plots
  if (length(StateConfig) == 3) {PlotName <- "SIR"
  Lol <- c("Susceptible", "Infected", "Recovered")
  } else if (length(StateConfig) == 4) {PlotName <- "SEIR"
  Lol <- c("Susceptible", "Exposed", "Infected", "Recovered")}
  d <- 1
  
  
  
  # Asignar a objetos la cantidad de dias que pasa una persona en un estado determinado
  if(length(StateDays) == 3) {S<-StateDays[1]
  I<-StateDays[2]
  R<-StateDays[3]} else if (length(StateDays) == 4){
    S<-StateDays[1]
    E<-StateDays[2]
    I<-StateDays[3]
    R<-StateDays[4]
  }
  
  
  
  # Para que quede fija la red
  LO <- layout_nicely(Adj)
  angle <- 7*100 * 20
  RotMat <- matrix(c(cos(angle),sin(angle),-sin(angle), cos(angle)), ncol=2)
  LO2 <- LO %*% RotMat
  
  
  
  
  Net <- x # Asignar a otro objeto
  AdjNet <- as_adj_list(Net) # Para obtener adyacencias
  
  
  # Añadir atributos a los vertices
  V(Net)$State <- c(rep("A", length(Net)))
  V(Net)$Days <- c(rep(0, length(Net)))  
  
  
  
  
  # Asociar colores a los estados en los plots
  Colors <- Colors
  my_color <- Colors[as.numeric(as.factor(V(Net)$State))]
  
  
  
  
  
  
  # Asignar estados a un orden alfabetico (porque igraph asigna los colores en orden alfabetico)
  for (i in 1:length(StateConfig)) {
    StateConfig[i] <- LETTERS[i]
  }
  
  
  
  ####################
  # HAZ UN PLOT AQUI #
  ####################
  my_color <- Colors[as.numeric(as.factor(V(Net)$State))]
  
  plot(Net, layout = LO2, vertex.color = my_color, vertex.size = degree(Adj, V(Adj), "in")*3, 
       vertex.label = "", edge.arrow.size = 0.2, edge.size = 19, 
       vertex.frame.color="white", edge.color = "#E5E3C9")
  
  legend(x=-1.5, y=-1.1, paste(Lol), pch=21, 
         col="lightgray", pt.bg = Colors, pt.cex=2, cex=1, bty="n", ncol=1, title = paste(PlotName))
  
  #######################################
  # Actualizar la base de datos por dia #
  #######################################
  if (length(StateConfig) == 3){
    new_row <- c(as.numeric(table(V(Net)$State == "A")["TRUE"]),
                 as.numeric(table(V(Net)$State == "B")["TRUE"]),
                 as.numeric(table(V(Net)$State == "C")["TRUE"]),
                 0)
    
    Summary <- rbind(Summary, new_row)
  } else if (length(StateConfig) == 4) {
    new_row <- c(as.numeric(table(V(Net)$State == "A")["TRUE"]),
                 as.numeric(table(V(Net)$State == "B")["TRUE"]),
                 as.numeric(table(V(Net)$State == "C")["TRUE"]),
                 as.numeric(table(V(Net)$State == "D")["TRUE"]),
                 0)
    
    Summary <- rbind(Summary, new_row)
  }
  
  
  
  # Asignar el primer infectado a la red
  V(Net)$Days[StartNode] <- 1
  if(length(StateConfig) == 3) {V(Net)$State[StartNode] <- "B"} else if (length(StateConfig) == 4) {V(Net)$State[StartNode] <- "C"}
  
  
  
  
  
  ####################
  # HAZ UN PLOT AQUI #
  ####################
  my_color <- Colors[as.numeric(as.factor(V(Net)$State))]
  
  plot(Net, layout = LO2, vertex.color = my_color, vertex.size = degree(Adj, V(Adj), "in")*3, 
       vertex.label = "", edge.arrow.size = 0.2, edge.size = 19, 
       vertex.frame.color="white", edge.color = "#E5E3C9")
  
  legend(x=-1.5, y=-1.1, paste(Lol), pch=21, 
         col="lightgray", pt.bg = Colors, pt.cex=2, cex=1, bty="n", ncol=1, title = paste(PlotName, "Day", 0))
  
  #######################################
  # Actualizar la base de datos por dia #
  #######################################
  if (length(StateConfig) == 3){
    new_row <- c(as.numeric(table(V(Net)$State == "A")["TRUE"]),
                 as.numeric(table(V(Net)$State == "B")["TRUE"]),
                 as.numeric(table(V(Net)$State == "C")["TRUE"]),
                 0)
    
    Summary <- rbind(Summary, new_row)
  } else if (length(StateConfig) == 4) {
    new_row <- c(as.numeric(table(V(Net)$State == "A")["TRUE"]),
                 as.numeric(table(V(Net)$State == "B")["TRUE"]),
                 as.numeric(table(V(Net)$State == "C")["TRUE"]),
                 as.numeric(table(V(Net)$State == "D")["TRUE"]),
                 0)
    
    Summary <- rbind(Summary, new_row)
  }
  
  
  
  
  for (i in 1:Days) {
    
    
    # Asignar los dias de los estados a nuevos objetos
    if(length(StateConfig) == 3) {S <- StateDays[1]
    I <- StateDays[2]
    R <- StateDays[3]
    } else if (length(StateConfig) == 4) {
      
      S <- StateDays[1]
      E <- StateDays[2]
      I <- StateDays[3]
      R <- StateDays[4]}
    
    
    
    
    
    # Transmision de la enfermedad
    if (length(StateConfig) == 3){ # primer if
      
      for (i in 1:length(V(Net)$State)) {
        if(V(Net)$State[i] == "B") {new_infected <- sample(as.numeric(AdjNet[[i]]), R0, replace = T)
        for (i in 1:length(new_infected)) {
          if (V(Net)$Days[new_infected[i]] == 0) {V(Net)$Days[new_infected[i]] <- 1}
        }
        }
      } 
      
      
    } else if (length(StateConfig) == 4) { # Segundo if 
      
      for (i in 1:length(V(Net)$State)) {
        if(V(Net)$State[i] == "C") {new_infected <- sample(as.numeric(AdjNet[[i]]), R0, replace = T)
        for (i in 1:length(new_infected)) {
          if (V(Net)$Days[new_infected[i]] == 0) {V(Net)$Days[new_infected[i]] <- 1}
        }
        }
      } 
      
      
    } # Aqui termina el segundo if
    
    
    
    
    
    
    # Cambiar el estado basado en los dias que paso en ese mismo
    for (i in 1:length(V(Net)$Days)) {
      if(length(StateConfig) == 3) { # Si es un modelo SIR
        if(V(Net)$Days[i] == S+1) {V(Net)$State[i] <- "B"} else if (V(Net)$Days[i] == I+1) {V(Net)$State[i] <- "C"}
        
        
      } else if(length(StateConfig) == 4) { # Si es un modelo SEIR
        if(V(Net)$Days[i] == S+1) {V(Net)$State[i] <- "B"} else if (V(Net)$Days[i] == E+1) {V(Net)$State[i] <- "C"} else if (V(Net)$Days[i] == I+1) {V(Net)$State[i] <- "D"}
      }
    }
    
    
    
    
    
    # Añadir un dia a todos aquellos que iniciaron el proceso infeccioso
    for (i in 1:length(V(Net)$Days)) {
      if(V(Net)$Days[i] > 0) {V(Net)$Days[i] <- V(Net)$Days[i] + 1}
    }
    
    
    
    ####################
    # HAZ UN PLOT AQUI #
    ####################
    my_color <- Colors[as.numeric(as.factor(V(Net)$State))]
    
    plot(Net, layout = LO2, vertex.color = my_color, vertex.size = degree(Adj, V(Adj), "in")*3, 
         vertex.label = "", edge.arrow.size = 0.2, edge.size = 19, 
         vertex.frame.color="white", edge.color = "#E5E3C9")
    
    legend(x=-1.5, y=-1.1, paste(Lol), pch=21, 
           col="lightgray", pt.bg = Colors, pt.cex=2, cex=1, bty="n", ncol=1, title = paste(PlotName, "Day", d))
  
  
  #######################################
  # Actualizar la base de datos por dia #
  #######################################
  
  if (length(StateConfig) == 3){
    new_row <- c(as.numeric(table(V(Net)$State == "A")["TRUE"]),
                 as.numeric(table(V(Net)$State == "B")["TRUE"]),
                 as.numeric(table(V(Net)$State == "C")["TRUE"]),
                 d)
    
    Summary <- rbind(Summary, new_row)
  } else if (length(StateConfig) == 4) {
    new_row <- c(as.numeric(table(V(Net)$State == "A")["TRUE"]),
                 as.numeric(table(V(Net)$State == "B")["TRUE"]),
                 as.numeric(table(V(Net)$State == "C")["TRUE"]),
                 as.numeric(table(V(Net)$State == "D")["TRUE"]),
                 d)
    
    Summary <- rbind(Summary, new_row)
  }
  
  d <- d+1 
  
}
return(Summary)
}




#######
# SIR #
#######

# Plots de uno por uno
a <- Infectious.Network(Adj, c("S", "I", "R"), c(0, 5, 0), 24, 2, c("#9EA1D4", "#A8D1D1", "#F1F7B5"), 40)



# Para hacer un gift
saveGIF(Infectious.Network(Adj, c("S", "I", "R"), c(0, 5, 0), 24, 2, c("#9EA1D4", "#A8D1D1", "#F1F7B5"), 40) ,
        # Nombre del gif
        movie.name = "SIR.gif",
        # Dimensiones
        ani.width  = 350,
        ani.height = 350,
        # Tiempo de duración de cada frame (segundos)
        interval = 0.2
)



########
# SEIR #
########


# Plots de uno por uno
b <- Infectious.Network(Adj, c("S", "E", "I", "R"), c(0, 2, 5, 0), 24, 2, c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5"), 60)


# Para hacer un gift
saveGIF(Infectious.Network(Adj, c("S", "E", "I", "R"), c(0, 2, 5, 0), 24, 2, c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5"), 60),
        # Nombre del gif
        movie.name = "SEIR.gif",
        # Dimensiones
        ani.width  = 1600,
        ani.height = 900,
        # Tiempo de duración de cada frame (segundos)
        interval = 0.2
)



##############
# Modelo SIR #
#############


SIR <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), { 
    
    dS <- -beta*S*I/(S+I+R) 
    dI <- beta*S*I/(S+I+R) -gama*I
    dR <- gama*I 
    
    list(c(dS, dI, dR))
  })
}

###  Condiciones iniciales:

pars <- c(beta = 6, gama = 3)  
condiciones_iniciales <- c(S = 499, I = 1, R = 0)
tiempo <- seq(0, 10, by = 0.0001)
out1 <- ode(condiciones_iniciales, tiempo, SIR, pars)


### Grafica:

matplot(out1[ , 1], out1[ , 2:4], type = "l", xlab = "Time", ylab = "Population", main = "SIR", lwd = 3, col = c("#9EA1D4", "#A8D1D1", "#F1F7B5"))
legend("topright", c("Suceptible", "Infectado / Infected","Recuperado / Recovered"), col = c("#9EA1D4", "#A8D1D1", "#F1F7B5"), lty = 1:4, cex = 0.5)






###############
# Modelo SEIR #
###############


SEIR <- function (t, state, parameters) {  
  with (as.list (c (state, parameters)), { 
    dS <- - beta * S * I / (S + E + I + R)  
    dE <- beta * S * I / (S + E + I + R) - delta * E   
    dI <- delta * E - gama * I  
    dR <- gama * I   
    list (c (dS, dE, dI, dR))  
  })
} 

pars <- c (beta = 10, delta = 4, gama = 1) 
condiciones_iniciales <- c (S = 497, E = 2, I = 1, R = 0)  
tiempo <- seq (0, 10, by = 0.001)  
out2 <- ode (condiciones_iniciales, tiempo, SEIR, pars) 

matplot (out2[ , 1], out2[ , 2 : 5], type = "l", xlab = "Time", ylab = "Population", main = "SEIR", lwd = 3, col = c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5")) 
legend ("topright", c ("Susceptible", "Expuesto / Exposed", "Infectado / Infected", "Recuperado / Recovered"), lty = 1 : 4, cex = 0.5, col = c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5")) 




#################################
# Comparacion entre los metodos #
#################################


# Tablas del resultado de la funcion de redes

# Quitar las NA's de las tablas (NA=0)
a[is.na(a)] <- 0
a

b[is.na(b)] <- 0
b


## Graficos de SIR


# Funcion de redes 

matplot(a[ -1, 4], a[ -1, 1:3], type = "l", xlab = "Time", ylab = "Population", main = "SIR con función de redes", lwd = 3, col = c("#9EA1D4", "#A8D1D1", "#F1F7B5"))
legend("topright", c("Suceptible", "Infectado / Infected","Recuperado / Recovered"), col = c("#9EA1D4", "#A8D1D1", "#F1F7B5"), lty = 1:4, cex = 0.5)


# Desolve

matplot(out1[ , 1], out1[ , 2:4], type = "l", xlab = "Time", ylab = "Population", main = "SIR con desolve", lwd = 3, col = c("#9EA1D4", "#A8D1D1", "#F1F7B5"))
legend("topright", c("Suceptible", "Infectado / Infected","Recuperado / Recovered"), col = c("#9EA1D4", "#A8D1D1", "#F1F7B5"), lty = 1:4, cex = 0.5)





## Graficos de SEIR


# Funcion de redes

matplot (b[ -1, 5], b[ -1, 1:4], type = "l", xlab = "Time", ylab = "Population", main = "SEIR con función de redes", lwd = 3, col = c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5")) 
legend ("topright", c ("Susceptible", "Expuesto / Exposed", "Infectado / Infected", "Recuperado / Recovered"), col = c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5"), lty = 1 : 4, cex = 0.5) 


# Desolve 

matplot (out2[ , 1], out2[ , 2 : 5], type = "l", xlab = "Time", ylab = "Population", main = "SEIR con desolve", lwd = 3, col = c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5")) 
legend ("topright", c ("Susceptible", "Expuesto / Exposed", "Infectado / Infected", "Recuperado / Recovered"), col = c("#9EA1D4", "#FD8A8A", "#A8D1D1", "#F1F7B5"), lty = 1 : 4, cex = 0.5) 


