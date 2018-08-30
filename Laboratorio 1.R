rm(list=ls())

#Los 0s aceptados antes de expresas una cifra en notación cientifica
#option("scipen"=100, "digits"=4)

####Cargas librerias a utilizar 
suppressMessages(library(plotly)) #Graficas interactivas
suppressMessages(library(Quandl)) #Descargas precios
suppressMessages(library(PortfolioAnalytics)) # Teoria moderna 
suppressMessages(library(ROI)) #Optimización para portafolios
suppressMessages(library(kableExtra)) #Tablas en HTML

Quandl.api_key("dN9QssXxzTxndaqKUQ_i")

Bajar_Precios<- function(Columns, Tickers, Fecha_In, FEcha_Fn){
  
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers, date.gte=Fecha_In, date.lte=Fecha_In)
  return(Datos)
  
}

#Tickers de acciones 
tk <- c("APPL", "TSLA", "HD")
cs <- c("date", "adj_close")

#fecha inicial y final 
fs <- c("2015-08-01", "2016-08-01")
#decargar precios y calc rend
Datos <- list()
for (i in 1:length(tk)){
  Datos[[i]] <- Bajar_Precios(cs,  tk[i],  fs[1], fs[2])
}

names(Datos) <- tk

for(i in 1:length(tk)) {
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))
}

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk


Port1<- portfolio.spec(assets = tk)

#Especificar restricciones del portafolio 

Port1 <- add.constraint(portfolio = Port1, type = "full_investment")

#type = "fullinvestment" es cuando quieres invertir todo el capital

#restriccion 2: limites superior e inferior para el valor de los pesos individuales 
Port <- add.constraint(portfolio = Port1, type = "box", min=c(0.01,0.01,0.01), max=c(0.7,0.7,0.7))

Port1<- add.objective(portfolio = Port1, type = "return", name = "mean" )

Port1<- optimize.portfolio(R=Rends, portfolio = Port1, optimize_method = "random", trace = TRUE, search_size = 500)

