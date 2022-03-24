# Datos ENSIN-----

# Se cargan los archivos de caracteristicas de las personas y antropometria

dt_pts   <- as.data.table(read.dta13(paste0(wd_data,'PTS.dta')))
dt_ensin <- as.data.table(read.dta13(paste0(wd_data,'ANTROPOMETRIA.dta')))

dt_pts <- unique(dt_pts[,c('LLAVE_HOGAR','DEPTO'),with=FALSE],by='LLAVE_HOGAR')

dt_ensin <- merge(dt_ensin,dt_pts,by='LLAVE_HOGAR',all.x = T)

# Variables auxiliares para modelos----

# Variable dependiente
table(dt_ensin$sexo)
dt_ensin[,obeso:=as.numeric(estadoImc9=='Obesidad')]
table(dt_ensin$adulto18a64)
table(dt_ensin$AN_EDAD) 

# Dummy mujeres
dt_ensin[,mujer:=as.numeric(as.character(sexo)==' Mujeres')]
table(dt_ensin$mujer)
# Dummies grupos de edad
dt_ensin[,gedad:=cut(AN_EDAD,              
                 breaks =seq(0,65,5),                  
                 labels =seq(0,60,5))]

table(dt_ensin$gedad)
# dummy rural
table(dt_ensin$cabecera)
dt_ensin[,rural:=as.numeric(as.character(cabecera)==' Resto')]
table(dt_ensin$rural)

# Etnia
table(dt_ensin$etnia)
dt_ensin[,etnia_indigena:=as.numeric(as.character(etnia)==' Indígena')]
dt_ensin[,etnia_ninguna:=as.numeric(as.character(etnia)==' Sin pertenecia etnica')]
dt_ensin[,etnia_negra:=as.numeric(etnia_ninguna==0 & etnia_indigena==0)]
table(dt_ensin$etnia_negra)

# Base para regresiones----

# Se guardan microdatos para modelos que requieran de estaestructura de datos

dt_ensin <- dt_ensin[,c('LLAVE_HOGAR','LLAVE_PERSONA','obeso','DEPTO',
                        'Subregion','rural','mujer','gedad','cod_UPM_rev',
                        'Estrato_UPM','fpc_upm','fpc_USM','USM','fpc_utm','UTM',
                        'etnia_negra','etnia_indigena','etnia_ninguna',
                        'FactorExpansionPer'),with=FALSE]  

dt_ensin <- dt_ensin[!(as.character(gedad) %in% c('0','5','10','65',NA)),]
saveRDS(dt_ensin,file = paste0(wd_data,'MicroEnsin.rds'))

# dejar variables
dt_ensin_aux <- dt_ensin[,c('DEPTO','rural','mujer','gedad',
                        'etnia_negra','etnia_indigena','etnia_ninguna','FactorExpansionPer'),with=FALSE]  

dt_ensin_aux <- dt_ensin_aux[!(as.character(gedad) %in% c('0','5','10','65',NA)),]
dt_ensin_aux <- cbind(dt_ensin_aux, dummy(dt_ensin_aux$gedad, sep = "_"))
colnames(dt_ensin_aux) <- gsub('dt_ensin_aux','gedad',colnames(dt_ensin_aux))

dt_ensin_aux[,c('gedad'):=NULL]

# Medias auxiliares----

# Se calculan medias poblaciones a nivel de departamento. Con datos no anonimizados
# estos se deben calcular a nivel de municipio

dt_ensin_aux <- dt_ensin_aux[, lapply(.SD, function(x){weighted.mean(x,w=FactorExpansionPer,na.rm=T)}), by="DEPTO"]

dt_ensin_aux[,c('FactorExpansionPer'):=NULL]

fwrite(dt_ensin_aux,file = paste0(wd_data,'AuxEnsin.csv'))


