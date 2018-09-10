#Remover todos los objetos del "Enviorment"
rm(list = ls())

#los 0s aceptados antes de expresar una cifra en notacion cientifica
options("scipen"=100, "digits"=4)

### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teoria Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(kableExtra)) # Tablas en HTML para interactuar

# Cargar el token de QUANDL
Quandl.api_key("KAxj_3rAYHS5kZnBoSf2")

Capital_Inicial <- 10000
# Funcion para descagar precios (aquí pones dentro de las llaves la función)
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}


tk <- c("DIS", "GS","PFE") #Disney, Goldman Sachs, Pfizer
cs <- c("date","adj_close") 
fs <- c("2015-08-01","2016-08-01") 

#Descargamos precios
Datos <- list() #primero creamos el vector 'Datos' vacio

#Creamos un for que va desde el 1 hasta la longitud de tk, o sea 3.
for(i in 1:length(tk)) {
  Datos[[i]] <- Bajar_Precios(cs,tk[i],fs[1],fs[2])
  }

#Recordamos que para los for y funciones, aqui van entre llaves, como en python es
#gracias a la identacion

names(Datos) <- tk
#con 'names' cambiamos los nombres de las columnas por lo que quieras, en este caso
#por los tickers.

for(i in 1:length(tk)){
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))
  }

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1] #el -1 quita el ultimo valor

names(Rends) <- tk

Port1<- portfolio.spec(assets = tk)

#Especificar restricciones del portafolio 

Port1 <- add.constraint(portfolio = Port1, type = "full_investment")

#type = "fullinvestment" es cuando quieres invertir todo el capital

#restriccion 2: limites superior e inferior para el valor de los pesos individuales 
Port <- add.constraint(portfolio = Port1, type = "box", min=c(0.01,0.01,0.01), max=c(0.7,0.7,0.7))

Port1<- add.objective(portfolio = Port1, type = "return", name = "mean" )

Port1<- optimize.portfolio(R=Rends, portfolio = Port1, optimize_method = "random", trace = TRUE, search_size = 500)

#Portafolios < - vector("list", length = length(Port1$random_portfolio_objective_results))

Portafolios <- vector("list", length = length(Port1$random_portfolio_objectives_results))

for (i in 1:length(Port1$random_portfolio_objective_results)) {
  
  Portafolios[[i]]$Pesos <- Port1$random_portfolio_objectives_results[[i]]$weights
  Portafolios[[i]]$Medias <- Port1$random_portfolio_objectives_results[[i]]$objectives_measures$mean
  Portafolios[[i]]$Vars <- var.portfolio(R= Port1$R, weights = Portafolios[[i]]$pesos)
  names(Portafolios[[i]]$Medias) <- NULL
  
  
}



df_Portafolios <- data.frame(matrix(nrow = length(Port1$random_portfolios_objectives_results),
                                    ncol = 3,
                                    data = 0))
colnames(df_Portafolios) <- c("Rends","Var", "Clase")
