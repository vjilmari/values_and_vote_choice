#' ---
#' title: "Analysis for values by education interactions"
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#' date: '`r Sys.Date()`'
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' RQ5. If, as we expected (H1-H3), some personal values are associated with the ideological position of the party for which one has voted, are these associations stronger among the more highly educated (i.e., is there a boundary condition to these associations, such that they only exists among the more highly educated)?
#' H7. We expect the associations between personal values and the ideology of the party for which one has voted to be stronger among the more highly educated.
#' 
#' ERQ5. Given that some personal values are associated the the salience of anti-elitism (H4) or (anti-)corruption (ERQ1), or some of the 15 Chapel Hill rated  policy positions (ERQ2) of the party for which one has voted, are these associations stronger among the more highly educated (i.e., is there a boundary condition to these associations, such that they only exist among the more highly educated)?
#' 
#' # Preparations
#' 
#' ## Packages
#' 
## ---------------------------------------------------------------------------------------

library(lme4)
library(lmerTest)
library(emmeans)
library(r2mlm)
library(rio)
library(dplyr)
library(vjihelpers)
library(psych)
library(tibble)

#' 
#' ## Functions
#' 
## ---------------------------------------------------------------------------------------

source("../custom_functions.R")

#' 
#' ## Data
#' 
## ---------------------------------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")

#' 
#' ## Variables
#' 
## ---------------------------------------------------------------------------------------

value.vars<-
  c("con","tra","ben","uni","sdi",
        "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]

table(fdat$edu.c,useNA="always")

table(fdat$cntry)

fdat$West_vs_post_comm<-
  case_when(fdat$cntry == "AT" |
              fdat$cntry == "BE" |
              fdat$cntry == "CH" |
              fdat$cntry == "DE" |
              fdat$cntry == "DK" |
              fdat$cntry == "ES" |
              fdat$cntry == "FI" |
              fdat$cntry == "FR" |
              fdat$cntry == "GB" |
              fdat$cntry == "IE" |
              fdat$cntry == "IL" |
              fdat$cntry == "NL" |
              fdat$cntry == "NO" |
              fdat$cntry == "PT" |
              fdat$cntry == "SE" ~ -0.5,
            TRUE~0.5)

table(fdat$West_vs_post_comm,useNA="always")


#' 
#' 
#' # Analysis call
#' 
## ---------------------------------------------------------------------------------------
length(DV.vars)

for (k in 1:length(DV.vars)){
  DV_by_values_lvl1_pipe(
    DV=DV.vars[k],
    data=fdat,
    IV=value.vars,
    IV.c=value.vars.c,
    moderator="edu.c",
    directory="../../results/education_interaction")
  
}



#' 
#' # Session info
#' 
## ---------------------------------------------------------------------------------------
sinf<-sessionInfo()
print(sinf,locale=F)

