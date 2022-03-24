### 
# Model fitting -----
###

## Funcion para RMSE
# m modelo
# o observado
RMSE <- function(m, o){
  sqrt(mean((m - o)^2))
}


## Estimador directo----
data <- as.data.table(readRDS(file = paste0(wd_data,'MicroEnsin.rds')))

# Crea las variables de edad en la base de datos
data <- cbind(data, dummy(data$gedad, sep = "_"))
colnames(data) <- gsub('data','gedad',colnames(data))

data[,gedad_1524:=gedad_15+gedad_20]
data[,gedad_2534:=gedad_25+gedad_30]
data[,gedad_3544:=gedad_35+gedad_40]
data[,gedad_4554:=gedad_45+gedad_50]
data[,gedad_5564:=gedad_55+gedad_60]


# Disenio de la encuesta----
design <- 
  svydesign( 
    id= ~ cod_UPM_rev, 
    data = data, 
    weights = ~FactorExpansionPer
  )

# Este estimador directo a nivel de departamento con datos anoniizados
# debe realizarce a nivel de municipio con los datos desanonimizados

povinc.dir <- as.data.frame(svyby( ~ obeso ,~ DEPTO , design , svymean , na.rm = TRUE, vartype='var'))
hist(povinc.dir$obeso,prob=TRUE,main="",xlab="HT estimators pov.incidence")

# Auxiliar divipola
dt_aux_divi <- fread(file = paste0(wd_data,'AuxDivipolaDeptoEnsin.csv'))

povinc.dir <- as.data.table(merge(povinc.dir,dt_aux_divi))
povinc.dir[,id:=str_pad(U_DPTO, 2, pad = "0")]

g <- colmap(departamentos, povinc.dir, var = "obeso")
g
ggsave(file = paste0(wd_data,"MapaDepto",".jpg"), plot = g, device = "jpg",height = 20, width = 15)


## Modelo Fay-herriot----

dt_ensin_aux <- fread(file = paste0(wd_data,'AuxEnsin.csv'))

dt_ensin_aux <- dt_ensin_aux[order(dt_ensin_aux$DEPTO),]

## Borrar variables de base
#dt_ensin_aux[,c('etnia_ninguna','gedad_15'):=NULL]

# recodifica edades

dt_ensin_aux[,gedad_1524:=gedad_15+gedad_20]
dt_ensin_aux[,gedad_2534:=gedad_25+gedad_30]
dt_ensin_aux[,gedad_3544:=gedad_35+gedad_40]
dt_ensin_aux[,gedad_4554:=gedad_45+gedad_50]
dt_ensin_aux[,gedad_5564:=gedad_55+gedad_60]

#dt_ensin_aux <- dt_ensin_aux[,c('DEPTO','rural','mujer',"etnia_negra","etnia_indigena"),with=FALSE]
dt_ensin_aux <- dt_ensin_aux[,c('DEPTO','rural','mujer',"etnia_negra","etnia_indigena",
                                'gedad_2534','gedad_3544','gedad_4554','gedad_5564'),with=FALSE]

D <- nrow(dt_ensin_aux) 
X <- as.matrix(cbind(const=rep(1,D),dt_ensin_aux[,-1]))

colnames(X)
summary(X)
povinc.FH.res<-eblupFH(povinc.dir$obeso~X-1,vardir=povinc.dir$var, method = 'FH')
povinc.FH<-povinc.FH.res$eblup

povinc.rsyn1<-X%*%povinc.FH.res$fit$estcoef[,1]

# Finalmente, vamos a comparar los CVs estimados para los estimadores HT, GREG y FH para las 5 
# provincias con menores tamanos muestrales: 
compardirFH<-data.frame(dt_ensin_aux$DEPTO,povinc.dir,povinc.FH,povinc.rsyn1)


## Modelo con errores anidados ----
prov     <- unique(dt_ensin_aux$DEPTO)
meanxpop <- dt_ensin_aux[order(DEPTO),]
#popnsize <- data[,list(popnsize=sum(FactorExpansionPer)),by=list(DEPTO)]
popnsize <- data[,list(popnsize=.N),by=list(DEPTO)]
popnsize <- popnsize[order(DEPTO),]

data <- as.data.frame(data)

# povinc.BHF.res<-eblupBHF(obeso ~ rural + mujer + etnia_negra + etnia_indigena,
#                          dom=DEPTO,meanxpop=meanxpop,popnsize=popnsize,data=data)

povinc.BHF.res<-eblupBHF(obeso ~ rural + mujer + etnia_negra + etnia_indigena + gedad_2534 + gedad_3544
                         + gedad_4554 + gedad_5564,
                         dom=DEPTO,meanxpop=meanxpop,popnsize=popnsize,data=data)

povinc.BHF<-numeric(D)

# Consultamos los resultados del ajuste del modelo con errores anidados y calculamos el estimador 
# sintético de regresión basado en el modelo a nivel de individuo: 
betaest<-povinc.BHF.res$fit$fixed # Coeficientes de regresión
upred<-povinc.BHF.res$fit$random # Efectos predichos de prov.
sigmae2est<-povinc.BHF.res$fit$errorvar # Var. estimada del error
sigmau2est<-povinc.BHF.res$fit$refvar # Varianza estimada de los efectos de las provincias

X <- as.matrix(cbind(1,meanxpop[,-1]))
povinc.rsyn2 <-X%*%betaest
povinc.rsyn2 <-povinc.rsyn2+upred
setnames(povinc.rsyn2,'(Intercept)','povinc.rsyn2')

#povinc.rsyn2 <- povinc.BHF.res$eblup$eblup 

## Comparar todas las estimaciones hasta ahora
compardirFHEBLUP <- cbind(compardirFH,povinc.rsyn2)

rmse.FH   <- RMSE(m=compardirFHEBLUP$povinc.FH,o=compardirFHEBLUP$obeso)
rmse.syn1 <- RMSE(m=compardirFHEBLUP$povinc.rsyn1,o=compardirFHEBLUP$obeso)
rmse.syn2 <- RMSE(m=compardirFHEBLUP$povinc.rsyn2,o=compardirFHEBLUP$obeso)
rmse.out  <- data.frame(cbind(rmse.FH,rmse.syn1,rmse.syn2)) 
rmse.out

### 
# Model prediction -----
###

dt_censo <- fread(file=paste0(wd_data,'AuxCenso.csv'))
dt_censo$gedad_1524 <- NULL
## Modelo Fay-herriot----

# Se deja el modelo FH

## Guarda modelo
modelo <-povinc.FH.res$fit$estcoef

X <- as.matrix(cbind(const=rep(1,nrow(dt_censo)),dt_censo[,-1]))

colnames(X)
summary(X)

sae_estimate <- X%*%povinc.FH.res$fit$estcoef[,1]

dt_censo <- cbind(dt_censo,sae_estimate)

setnames(dt_censo,'V1','indicador')

dt_censo[,id:=str_pad(U_MPIO, 5, pad = "0")]

#dt_censo <- dt_censo[substr(U_MPIO,1,2)=='76',]

## Mapa del indicador
g <- colmap(municipios, dt_censo, var = "indicador")
g

ggsave(file = paste0(wd_data,"MapaMpioFH",".jpg"), plot = g, device = "jpg",height = 20, width = 15)

## Create new workbooks
wb <- createWorkbook() 
## Create the worksheets
addWorksheet(wb, sheetName = "Modelo")
addWorksheet(wb, sheetName = "EstDepto")
addWorksheet(wb, sheetName = "EstMpio")

## Write the data
writeData(wb, "Modelo", modelo)
writeData(wb, "EstDepto", povinc.dir)
writeData(wb, "EstMpio", dt_censo)

## Save workbook to working directory 
saveWorkbook(wb, file = paste0(wd_data,"SalidasSAEEnsinFH",".xlsx"), overwrite = TRUE)


## Modelo Con errores anidados----

povinc.rsyn2 <-X%*%betaest

povinc.rsyn2 <- as.data.table(cbind(dt_censo,povinc.rsyn2))

upred$DEPTO <- rownames(upred)

povinc.rsyn2[,U_DPTO:=as.numeric(substr(str_pad(as.character(U_MPIO), 5, pad = "0"),1,2))]

povinc.rsyn2 <- merge(povinc.rsyn2,dt_aux_divi,by='U_DPTO',all.x = T)
povinc.rsyn2 <- merge(povinc.rsyn2,upred,by='DEPTO',all.x = T)

setnames(povinc.rsyn2,'(Intercept)','upred')
povinc.rsyn2[,povinc.rsyn2:=V1+upred]


## Mapa del indicador
g <- colmap(municipios, povinc.rsyn2, var = "povinc.rsyn2")
g

ggsave(file = paste0(wd_data,"MapaMpioEBLUP",".jpg"), plot = g, device = "jpg",height = 20, width = 15)


## Create new workbooks
wb <- createWorkbook() 
## Create the worksheets
addWorksheet(wb, sheetName = "Modelo")
addWorksheet(wb, sheetName = "EstDepto")
addWorksheet(wb, sheetName = "EstMpio")

## Write the data
writeData(wb, "Modelo", c(betaest,upred))
writeData(wb, "EstDepto", povinc.dir)
writeData(wb, "EstMpio", povinc.rsyn2)

## Save workbook to working directory 
saveWorkbook(wb, file = paste0(wd_data,"SalidasSAEEnsinEBLUP",".xlsx"), overwrite = TRUE)


