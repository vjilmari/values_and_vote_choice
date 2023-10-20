#' ---
#' title: "ERQ4: Analysis for values by West vs. post-Communist interactions"
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#' date: '`r Sys.Date()`'
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' ERQ4. Given that some personal values are associated the the salience of anti-elitism (H4) or (anti-)corruption (ERQ1), or some of the 15 Chapel Hill rated  policy positions (ERQ2) of the party for which one has voted, are these associations different in Western Europe and in post-communist countries?
#' 
#' 
#' # Preparations
#' 
#' ## Packages
#' 
## -----------------------------------------------------------------

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
## -----------------------------------------------------------------

source("../custom_functions.R")

#' 
#' ## Data
#' 
## -----------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")

#' 
#' ## Variables
#' 
## -----------------------------------------------------------------

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
## -----------------------------------------------------------------
length(DV.vars)

for (k in 1:length(DV.vars)){
  DV_by_values_CLI_pipe(DV=DV.vars[k],
                  IV=value.vars,
                  IV.c=value.vars.c,
                  data=fdat,
                  moderator="West_vs_post_comm",
                  directory="../../results/ERQ4")
  
}



#' 
#' # Session info
#' 
## -----------------------------------------------------------------
sinf<-sessionInfo()
print(sinf,locale=F)

