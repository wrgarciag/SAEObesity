rm(list=ls())

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
#wd       <- 'C:/Users/wrgar/Google Drive/11Cloud/SAE/SAEEnsin/' 
wd <- 'G:/My Drive/11Cloud/SAE/SAEEnsin/Data/' 
#wd_data  <- 'C:/Users/wrgar/Downloads/Ensin/' 
wd_data  <- 'G:/My Drive/11Cloud/SAE/SAEEnsin/Data/' 

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