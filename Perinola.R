
## Simulacion de una ronda 
jugar_ronda <- function(vec_ronda, wallet , pozo, players){
  for(i in 1:players){
    if(wallet[i] > 0){
      ######## PON 1 ##################
      if(vec_ronda[i] == perinola[1]){
        wallet[i] <- wallet[i]-1
        pozo <- pozo + 1
      ####### PON 2 ####################  
      } else if(vec_ronda[i] == perinola[2]){
        if(wallet[i] > 2){
          wallet[i] <- wallet[i]-2
          pozo <- pozo + 2
        } else if (wallet[i]==1){
          wallet[i] <- wallet[i]-1
          pozo <- pozo + 1
        }
        
        
      ###### TOMA 1 ####################  
      } else if(vec_ronda[i] == perinola[3]){
        if(pozo != 0){
          wallet[i] <- wallet[i]+1
          pozo <- pozo - 1
        }
        
        ###### TOMA 2 ####################  
      } else if(vec_ronda[i] == perinola[4]){
        if(pozo >= 2){
          wallet[i] <- wallet[i]+2
          pozo <- pozo - 2
        } else if (pozo == 1){
          wallet[i] <- wallet[i]+1
          pozo <- pozo - 1
        }
        
        ###### TOMA TODO ####################  
      } else if(vec_ronda[i] == perinola[5]){
        wallet[i] <- wallet[i]+pozo
        pozo <- 0
      
        ###### TODOS PONEN ####################
        } else{
        for(j in 1:players){
          if(wallet[j]>0){
            wallet[j] <- wallet[j]-1
            pozo <- pozo + 1
          }
        }
      }
    }
   
  }
  return(c(wallet,pozo))
}

## función utilizada para nombar el dataframe final de la simulación
names_dataframe <- function(x){
  colnames(x)[1:(ncol(x)-1)]<-paste0("jugador_",1:(ncol(x)-1))
  colnames(x)[ncol(x)] <- "pozo" 
  return(x)
}


#simulación de N rodas
sim <- function(players = 4, rondas = 10, cash = 100){
  perinola <<- c("Pon 1","Pon 2","Toma 1", "Toma 2", "Toma todo", "Todos ponen")
  wallet<-rep(cash,players)
  pozo <- 0
  game <- data.frame()
  for(i in 1:rondas){
    ronda<- jugar_ronda(vec_ronda = sample(perinola,size = players, replace = TRUE), 
                        wallet = wallet, 
                        pozo = pozo, 
                        players = players)
    wallet <- ronda[1:players]
    pozo <- ronda[length(ronda)]
    game <- rbind(game,ronda)
  }
return(names_dataframe(game))
}

## simulacion para encontrar cuantas rondas son necesarias para que haya un perdedor.
primer_perdedor <-function(cash=10, players=4){
  perinola <<- c("Pon 1","Pon 2","Toma 1", "Toma 2", "Toma todo", "Todos ponen")
  wallet<-rep(cash,players)
  pozo <- 0
  i <- 0
  game <- data.frame()
  ronda<-wallet
  while( sum(ronda[1:players]==0)==0 ){
    i=i+1
    ronda<- jugar_ronda(vec_ronda = sample(perinola,size = players, replace = TRUE), 
                        wallet = wallet, 
                        pozo = pozo, 
                        players = players)
    wallet <- ronda[1:players]
    pozo <- ronda[length(ronda)]
    game <- rbind(game,ronda)
    
  }
  return(i)
}

# simulacion para saber cuantas rondas son necesarias para que haya un ganador.
ganador <- function(cash=20, players=4){
  vector_juegos <- vector()
  for(j in 1:100){
    perinola <<- c("Pon 1","Pon 2","Toma 1", "Toma 2", "Toma todo", "Todos ponen")
    wallet<-rep(cash,players)
    pozo <- 0
    i <- 0
    game <- data.frame()
    ronda<-wallet
    while( sum(ronda[1:players]==0)<3 ){
      i=i+1
      ronda<- jugar_ronda(vec_ronda = sample(perinola,size = players, replace = TRUE), 
                          wallet = wallet, 
                          pozo = pozo, 
                          players = players)
      wallet <- ronda[1:players]
      pozo <- ronda[length(ronda)]
      game <- rbind(game,ronda)
      
    } 
    vector_juegos<-c(vector_juegos,i)
  }
  return(vector_juegos)
}
  
  


