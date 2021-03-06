rm(list=ls())

# Para plotear los mapas instalar:
#install.packages("devtools")
#devtools::install_github("nebulae-co/colmaps")

library(data.table)
library(readstata13)
library(sae)
library(foreign)
library(dummies)
library(survey)

library(colmaps)
library(ggplot2)
library(openxlsx)
library(stringr)
## Pararllelization
library(doParallel)

### Datos ENSIN
wd <- 'C:/Users/wrgar/OneDrive/Documents/SAEObesity/'

wd_code  <- paste0(wd,'Code/')
wd_data  <- 'G:/My Drive/11Cloud/SAE/SAEEnsin/Data/'
wd_resu  <- paste0(wd,'Resu/')

#+++++++++++++++++++++++++++++++++++++++++++
##            Ensin 2005----
#+++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++
##            Ensin 2015----
#+++++++++++++++++++++++++++++++++++++++++++

# 1DataEnsin----
source('1DSAEEnsin15.R')

# 1DataAuxCenso----
source('1DSAECenso15.R')

# 2Estimaciones----
source('2ESAESurvey15.R')