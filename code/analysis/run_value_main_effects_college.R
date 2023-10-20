#' ---
#' title: "Analysis for values main effects for college educated"
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#' date: '`r Sys.Date()`'
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Packages
#' 
## -----------------------------------------------------------------------

library(lme4)
library(lmerTest)
#library(emmeans)
library(r2mlm)
library(rio)
library(dplyr)
library(vjihelpers)
#library(psych)
library(tibble)

#' 
#' ## Functions
#' 
## -----------------------------------------------------------------------

source("../custom_functions.R")

#' 
#' ## Data
#' 
## -----------------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")
fdat<-fdat[fdat$edu.c==0.5,]

#' 
#' ## Variables
#' 
## -----------------------------------------------------------------------

value.vars<-
  c("con","tra","ben","uni","sdi",
        "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]


#' 
#' # Analysis call
#' 
## -----------------------------------------------------------------------
length(DV.vars)

for (k in 1:length(DV.vars)){
  DV_by_values_pipe(DV=DV.vars[k],
                  IV=value.vars,
                  IV.c=value.vars.c,
                  data=fdat,
                  directory="../../results/college")
  
}



#' 
#' # Session info
#' 
## -----------------------------------------------------------------------
sinf<-sessionInfo()
print(sinf,locale=F)

