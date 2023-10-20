#' ---
#' title: "Descriptive statistics"
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#' date: '`r Sys.Date()`'
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Packages
#' 
## ------------------------------------------------------------------------------------------------------
library(psych)
library(dplyr)
library(rio)

#' 
#' ## Functions
#' 
## ------------------------------------------------------------------------------------------------------
source("../custom_functions.R")

#' 
#' ## Data
#' 
## ------------------------------------------------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")


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
#' ## Variables
#' 
## ------------------------------------------------------------------------------------------------------

value.vars<-
  c("con","tra","ben","uni","sdi",
        "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]


DV.vars<-DV.vars[-c(which(DV.vars=="lrecon_salience.z"),
  which(DV.vars=="galtan_salience.z"))]
DV.vars

#' 
#' ## Exclude Israeli participants
#' 
## ------------------------------------------------------------------------------------------------------
fdat<-fdat[fdat$cntry!="IL",]
table(fdat$cntry)

#' 
#' # Missing values
#' 
#' ## Political/Voting variables
#' 
## ------------------------------------------------------------------------------------------------------

fdat$sum_miss_DV.vars<-rowSums(is.na(fdat[,DV.vars]))
table(fdat$sum_miss_DV.vars,useNA="always")
fdat$miss_DV.vars<-ifelse(fdat$sum_miss_DV.vars!=0,1,0)
table(fdat$miss_DV.vars)


#' 
#' ## Values
#' 
## ------------------------------------------------------------------------------------------------------

fdat$sum_miss_value.vars<-rowSums(is.na(fdat[,value.vars]))
table(fdat$sum_miss_value.vars,useNA="always")
fdat$miss_value.vars<-ifelse(fdat$sum_miss_value.vars!=0,1,0)
table(fdat$miss_value.vars)


#' 
#' ## Gender
#' 
## ------------------------------------------------------------------------------------------------------

table(fdat$gndr.c,useNA="always")
fdat$miss_gndr<-ifelse(is.na(fdat$gndr.c),1,0)
table(fdat$miss_gndr)


#' 
#' ## Age
#' 
## ------------------------------------------------------------------------------------------------------

table(is.na(fdat$agea),useNA="always")
fdat$miss_agea<-ifelse(is.na(fdat$agea),1,0)
table(fdat$miss_agea)


#' 
#' ## Education
#' 
## ------------------------------------------------------------------------------------------------------

table(fdat$edu,useNA="always")
table(fdat$edu.f,useNA="always")
table(fdat$edu.c,useNA="always")

fdat$miss_edu<-ifelse(is.na(fdat$edu.c),1,0)
table(fdat$miss_edu)

#' 
#' ## Vote
#' 
## ------------------------------------------------------------------------------------------------------
table(fdat$vote,useNA="always")
table(fdat$vote.c,useNA="always")
prop.table(table(fdat$vote.c,useNA="always"))
prop.table(table(fdat$vote.c))

fdat$voted<-ifelse(fdat$vote==1,1,0)
table(fdat$voted,useNA="always")

table(fdat$voted,fdat$miss_DV.vars,useNA="always")

#' 
#' ## Make data frames without missing values
#' 
## ------------------------------------------------------------------------------------------------------
main.dat<-
  fdat[fdat$miss_DV.vars==0 &
         fdat$miss_value.vars==0 &
         fdat$miss_agea==0 &
         fdat$miss_gndr==0,]

edu.dat<-
  fdat[fdat$miss_DV.vars==0 &
         fdat$miss_value.vars==0 &
         fdat$miss_agea==0 &
         fdat$miss_gndr==0 &
         fdat$miss_edu==0,]

#' 
#' # Descriptive statistics
#' 
#' ## Voters of total
#' 
## ------------------------------------------------------------------------------------------------------
rbind(table(fdat$voted,useNA="always"),
      100*prop.table(table(fdat$voted,useNA="always")))


#' 
#' ## Matchable vote choice of voters
#' 
## ------------------------------------------------------------------------------------------------------
table(fdat$voted,fdat$miss_DV.vars,
      useNA="always")

prop.table(table(fdat$voted,fdat$miss_DV.vars,
      useNA="always"),1)


#' 
#' ## Excluded because of missingness in other variables
#' 
## ------------------------------------------------------------------------------------------------------
table(rowSums(fdat[fdat$miss_DV.vars==0,
                   c("miss_agea","miss_gndr","miss_value.vars")]))


#' 
#' ## Gender ratio in the final sample
#' 
## ------------------------------------------------------------------------------------------------------
rbind(table(main.dat$gndr.f,useNA="always"),
      100*prop.table(table(main.dat$gndr.f,useNA="always")))


#' 
#' ## Age
#' 
## ------------------------------------------------------------------------------------------------------
describe(main.dat$agea,fast=T)

#' 
#' ## Country-specific sample sizes
#' 
## ------------------------------------------------------------------------------------------------------
table(main.dat$cntry)
min(table(main.dat$cntry))
mean(table(main.dat$cntry))
median(table(main.dat$cntry))
max(table(main.dat$cntry))


#' 
#' ## Education
#' 
## ------------------------------------------------------------------------------------------------------
rbind(table(edu.dat$edu.c,useNA="always"),
      100*prop.table(table(edu.dat$edu.c,useNA="always")))

#' 
#' ## West Europe vs. Post-communist
#' 
## ------------------------------------------------------------------------------------------------------
main.dat$West_vs_post_comm<-
  case_when(main.dat$cntry == "AT" |
              main.dat$cntry == "BE" |
              main.dat$cntry == "CH" |
              main.dat$cntry == "DE" |
              main.dat$cntry == "DK" |
              main.dat$cntry == "ES" |
              main.dat$cntry == "FI" |
              main.dat$cntry == "FR" |
              main.dat$cntry == "GB" |
              main.dat$cntry == "IE" |
              main.dat$cntry == "IL" |
              main.dat$cntry == "NL" |
              main.dat$cntry == "NO" |
              main.dat$cntry == "PT" |
              main.dat$cntry == "SE" ~ -0.5,
            TRUE~0.5)

table(main.dat$cntry,
      main.dat$West_vs_post_comm)

rbind(table(main.dat$West_vs_post_comm,
            useNA="always"),
      100*prop.table(table(main.dat$West_vs_post_comm,
                           useNA="always")))

