
no_cores <- detectCores() - 1  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores, type="PSOCK")

### Datos Censo 2018
prepareCensus <- function(i,path.in,path.out){

  require(data.table) 
  require(foreign)
  require(dummies)
  require(readstata13)
  
  data <- as.data.table(read.spss(paste0(path.in,'CNPV2018_5PER_A2_',i,'.sav'), use.value.labels = F, to.data.frame = T))
  #data <- as.data.table(read.dta13(paste0(path.in,'CNPV2018_5PER_A2_',i,'.dta')))
  
  # Dummy mujeres
  data[,mujer:=as.numeric(P_SEXO==2)]
  
  # Dummies grupos de edad
  gecenso <- 1:21
  gedad5   <- seq(0,100,by=5)
  
  data[,P_EDADR:=as.factor(P_EDADR)]
  data[,levels(P_EDADR)]
  data[,gedad:=`levels<-`(P_EDADR, gedad5)]
  
  # Borrar menores de 15 y mayores de 64
  data <- data[!(as.character(gedad) %in% c('0','5','10','65','70','75','80','85','90','95','100')),]
  
  # dummy rural
  data[,rural:=as.numeric(as.numeric(UA_CLASE)!=1)]
  
  # Etnia
  data[,etnia_negra:=as.numeric(PA1_GRP_ETNIC %in% c(3,4,5))]
  data[,etnia_indigena:=as.numeric(PA1_GRP_ETNIC %in% c(1))]
  data[,etnia_ninguna:=as.numeric(PA1_GRP_ETNIC %in% c(2,6,9))]
  
  # dejar variables
  data <- data[,c('U_DPTO','U_MPIO','rural','mujer','gedad',
                          'etnia_negra','etnia_indigena','etnia_ninguna'),with=FALSE]  
  
  data <- cbind(data, dummy(data$gedad, sep = "_"))
  colnames(data) <- gsub('data','gedad',colnames(data))
  
  # variables por municipio
  data[,U_MPIO:=paste0(U_DPTO,U_MPIO)]
  data[,U_MPIO:=gsub(' ','',U_MPIO)]
  
  data[,c('gedad','U_DPTO'):=NULL]
  
  # Muestra para modelos individuales
  data.s <- data[sample(1:nrow(data), 0.01*nrow(data)), ] 
  saveRDS(data.s,file = paste0(path.out,'MicroCenso_',i,'.rds'))
  
  # Medias auxiliares
  data <- data[, lapply(.SD, mean), by='U_MPIO']
  
  fwrite(data,file = paste0(path.out,'AuxCenso_',i,'.csv'))
  
  
  #return(data)
}

#p.deptos <- c(97,99)

p.deptos <- c('05','08',11,13,15,17,18,19,20,23,25,27,41,
              44,47,50,52,54,63,66,68,70,73,76,81,85,86,
              88,91,94,95,97,99)

path.in  <- paste0(wd_data,'Censo/')
path.out <- wd_data

foreach(i=p.deptos) %dopar% prepareCensus(i=i,path.in = path.in,path.out = path.out)

stopCluster(cl) 


# Consolida tabla auxiliar

# Datos auxiliares del censo

dt_censo <- c()
for(ii in 1:length(p.deptos)){
  dt_aux       <- fread(file = paste0(wd_data,'AuxCenso_',p.deptos[ii],'.csv'))
  dt_censo <- rbind(dt_censo,dt_aux)
  rm(dt_aux)
}

dt_censo <- dt_censo[order(dt_censo$U_MPIO),]

# Codifica las variables de edad
colnames(dt_censo) <- gsub('NA','gedad',colnames(dt_censo))
dt_censo[,gedad_1524:=gedad_15+gedad_20]
dt_censo[,gedad_2534:=gedad_25+gedad_30]
dt_censo[,gedad_3544:=gedad_35+gedad_40]
dt_censo[,gedad_4554:=gedad_45+gedad_50]
dt_censo[,gedad_5564:=gedad_55+gedad_60]

## Borrar variables de base
dt_censo <- dt_censo[,c('U_MPIO','rural','mujer',"etnia_negra","etnia_indigena",'gedad_1524',
                        'gedad_2534','gedad_3544','gedad_4554','gedad_5564'),with=FALSE]

fwrite(dt_censo,file = paste0(wd_data,'AuxCenso,csv'))