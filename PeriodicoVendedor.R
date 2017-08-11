diaP<-c(0.35,0.45,0.2)
demandas<-c(40,50,60,70,80,90,100)
demandaG<-c(0.03,0.05,0.15,0.20,0.35,0.15,0.07)
demandaF<-c(0.1,0.18,0.4,0.20,0.08,0.04,0)
demandaP<-c(0.44,0.22,0.16,0.12,0.06,0,0)
dia<- sample(0:2,1,prob=diaP)

Demanda <- function(day=sample(0:2,1,prob=c(0.35,0.45,0.2))){
	if(day==0){
		demanda <- sample(demandas,1,prob=demandaG)
	} else if(day==1){
		demanda <- sample(demandas,1,prob=demandaF)
	} else {
		demanda <- sample(demandas,1,prob=demandaP)
	}
return(demanda)
}


economia <- function(demanda,oferta){
	if(demanda<oferta){
		DineroScrap<- -0.05*(demanda-oferta)
		venta<-demanda*0.5
		ganancia=venta+DineroScrap - 0.33*oferta		
	} else if(demanda>oferta){
		venta<-oferta*0.5
		ganancia <- venta - 0.17*(demanda-oferta)-0.33*oferta
		if (abs(ganancia)<0.001){ganancia <- 0}
	} else{
		venta=demanda*0.5
		ganancia=venta-0.33*oferta
	}
return(ganancia)
}

simulacion <- function(dias=20, periodicos=7,show=T){
periodicos=periodicos*10
profit<-1:dias
bandera=F
	for(i in 1:dias){
		dia<- sample(0:2,1,prob=diaP)
		demanda=Demanda(dia)
		profit[i]<-economia(demanda,periodicos);
		if (bandera==F){
			DataSim=c(dia,demanda,profit[i])
			bandera=T
		} else {
			DataSim=rbind(DataSim,c(dia,demanda,profit[i]))
		}
	}
labels<-c('Tipo de dia','Demanda','Ganancia')
colnames(DataSim)=labels
rownames(DataSim)=1:dias
if(show==T){
	return(DataSim)
	}
return(mean(DataSim[,3]))	
}



Grafica <- function(dias=20){
y=10*(4:10)	
	for (i in 4:10){
		y[i-3]=simulacion(periodicos=i,show=F,dias)
	}
x<-10*(4:10)
plot(x,y,type='l',xlab='Oferta (Numero de periodicos)',ylab='Ganancia Promedio ($)',main='Problema del vendedor de periodicos')
}

