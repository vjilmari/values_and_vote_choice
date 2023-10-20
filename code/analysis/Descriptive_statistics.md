---
title: "Descriptive statistics"
output: 
  html_document: 
    toc: yes
    keep_md: yes
date: '2022-08-31'
---



# Preparations

## Packages


```r
library(psych)
library(dplyr)
library(rio)
```

## Functions


```r
source("../custom_functions.R")
```

## Data


```r
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
```

```
## 
##  -0.5   0.5  <NA> 
## 29199 10986     0
```

## Variables


```r
value.vars<-
  c("con","tra","ben","uni","sdi",
        "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]


DV.vars<-DV.vars[-c(which(DV.vars=="lrecon_salience.z"),
  which(DV.vars=="galtan_salience.z"))]
DV.vars
```

```
##  [1] "lrgen.z"                  "lrecon.z"                 "galtan.z"                
##  [4] "spendvtax.z"              "deregulation.z"           "redistribution.z"        
##  [7] "econ_interven.z"          "civlib_laworder.z"        "sociallifestyle.z"       
## [10] "religious_principle.z"    "immigrate_policy.z"       "multiculturalism.z"      
## [13] "urban_rural.z"            "environment.z"            "regions.z"               
## [16] "international_security.z" "ethnic_minorities.z"      "nationalism.z"           
## [19] "antielite_salience.z"     "corrupt_salience.z"
```

## Exclude Israeli participants


```r
fdat<-fdat[fdat$cntry!="IL",]
table(fdat$cntry)
```

```
## 
##   AT   BE   CH   CZ   DE   DK   EE   ES   FI   FR   GB   HU   IE   LT   NL   NO   PL   PT   SE   SI 
## 1795 1769 1532 2148 3045 1502 2051 1925 2087 1917 2264 1698 2390 2250 1919 1436 1615 1265 1791 1224
```

# Missing values

## Political/Voting variables


```r
fdat$sum_miss_DV.vars<-rowSums(is.na(fdat[,DV.vars]))
table(fdat$sum_miss_DV.vars,useNA="always")
```

```
## 
##     0     1    20  <NA> 
## 21644    40 15939     0
```

```r
fdat$miss_DV.vars<-ifelse(fdat$sum_miss_DV.vars!=0,1,0)
table(fdat$miss_DV.vars)
```

```
## 
##     0     1 
## 21644 15979
```

## Values


```r
fdat$sum_miss_value.vars<-rowSums(is.na(fdat[,value.vars]))
table(fdat$sum_miss_value.vars,useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10  <NA> 
## 35059  1225   295   132    50    42    20    11    17    16   756     0
```

```r
fdat$miss_value.vars<-ifelse(fdat$sum_miss_value.vars!=0,1,0)
table(fdat$miss_value.vars)
```

```
## 
##     0     1 
## 35059  2564
```

## Gender


```r
table(fdat$gndr.c,useNA="always")
```

```
## 
##  -0.5   0.5  <NA> 
## 17707 19894    22
```

```r
fdat$miss_gndr<-ifelse(is.na(fdat$gndr.c),1,0)
table(fdat$miss_gndr)
```

```
## 
##     0     1 
## 37601    22
```

## Age


```r
table(is.na(fdat$agea),useNA="always")
```

```
## 
## FALSE  TRUE  <NA> 
## 37548    75     0
```

```r
fdat$miss_agea<-ifelse(is.na(fdat$agea),1,0)
table(fdat$miss_agea)
```

```
## 
##     0     1 
## 37548    75
```

## Education


```r
table(fdat$edu,useNA="always")
```

```
## 
## 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA  7. MA   <NA> 
##   3824   6595   6777   6585   5244   3920   4440    238
```

```r
table(fdat$edu.f,useNA="always")
```

```
## 
## 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA  7. MA   <NA> 
##   3824   6595   6777   6585   5244   3920   4440    238
```

```r
table(fdat$edu.c,useNA="always")
```

```
## 
##  -0.5   0.5  <NA> 
## 29025  8360   238
```

```r
fdat$miss_edu<-ifelse(is.na(fdat$edu.c),1,0)
table(fdat$miss_edu)
```

```
## 
##     0     1 
## 37385   238
```

## Vote


```r
table(fdat$vote,useNA="always")
```

```
## 
##     1     2     3  <NA> 
## 25738  8187  3394   304
```

```r
table(fdat$vote.c,useNA="always")
```

```
## 
##  -0.5   0.5  <NA> 
##  8187 25738  3698
```

```r
prop.table(table(fdat$vote.c,useNA="always"))
```

```
## 
##       -0.5        0.5       <NA> 
## 0.21760625 0.68410281 0.09829094
```

```r
prop.table(table(fdat$vote.c))
```

```
## 
##      -0.5       0.5 
## 0.2413265 0.7586735
```

```r
fdat$voted<-ifelse(fdat$vote==1,1,0)
table(fdat$voted,useNA="always")
```

```
## 
##     0     1  <NA> 
## 11581 25738   304
```

```r
table(fdat$voted,fdat$miss_DV.vars,useNA="always")
```

```
##       
##            0     1  <NA>
##   0        0 11581     0
##   1    21644  4094     0
##   <NA>     0   304     0
```

## Make data frames without missing values


```r
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
```

# Descriptive statistics

## Voters of total


```r
rbind(table(fdat$voted,useNA="always"),
      100*prop.table(table(fdat$voted,useNA="always")))
```

```
##               0           1        <NA>
## [1,] 11581.0000 25738.00000 304.0000000
## [2,]    30.7817    68.41028   0.8080164
```

## Matchable vote choice of voters


```r
table(fdat$voted,fdat$miss_DV.vars,
      useNA="always")
```

```
##       
##            0     1  <NA>
##   0        0 11581     0
##   1    21644  4094     0
##   <NA>     0   304     0
```

```r
prop.table(table(fdat$voted,fdat$miss_DV.vars,
      useNA="always"),1)
```

```
##       
##                0         1      <NA>
##   0    0.0000000 1.0000000 0.0000000
##   1    0.8409356 0.1590644 0.0000000
##   <NA> 0.0000000 1.0000000 0.0000000
```

## Excluded because of missingness in other variables


```r
table(rowSums(fdat[fdat$miss_DV.vars==0,
                   c("miss_agea","miss_gndr","miss_value.vars")]))
```

```
## 
##     0     1     2     3 
## 20464  1165    14     1
```

## Gender ratio in the final sample


```r
rbind(table(main.dat$gndr.f,useNA="always"),
      100*prop.table(table(main.dat$gndr.f,useNA="always")))
```

```
##           Female       Male <NA>
## [1,] 10619.00000 9845.00000    0
## [2,]    51.89113   48.10887    0
```

## Age


```r
describe(main.dat$agea,fast=T)
```

```
##    vars     n  mean   sd min max range   se
## X1    1 20464 52.85 16.9  15 101    86 0.12
```

## Country-specific sample sizes


```r
table(main.dat$cntry)
```

```
## 
##   AT   BE   CH   CZ   DE   DK   EE   ES   FI   FR   GB   HU   IE   LT   NL   NO   PL   PT   SE   SI 
## 1060 1235  625  883 1974 1103  915  900 1262  901 1290  753 1235  970 1239 1012  712  521 1361  513
```

```r
min(table(main.dat$cntry))
```

```
## [1] 513
```

```r
mean(table(main.dat$cntry))
```

```
## [1] 1023.2
```

```r
median(table(main.dat$cntry))
```

```
## [1] 991
```

```r
max(table(main.dat$cntry))
```

```
## [1] 1974
```

## Education


```r
rbind(table(edu.dat$edu.c,useNA="always"),
      100*prop.table(table(edu.dat$edu.c,useNA="always")))
```

```
##             -0.5        0.5 <NA>
## [1,] 14795.00000 5587.00000    0
## [2,]    72.58856   27.41144    0
```

## West Europe vs. Post-communist


```r
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
```

```
##     
##      -0.5  0.5
##   AT 1060    0
##   BE 1235    0
##   CH  625    0
##   CZ    0  883
##   DE 1974    0
##   DK 1103    0
##   EE    0  915
##   ES  900    0
##   FI 1262    0
##   FR  901    0
##   GB 1290    0
##   HU    0  753
##   IE 1235    0
##   LT    0  970
##   NL 1239    0
##   NO 1012    0
##   PL    0  712
##   PT  521    0
##   SE 1361    0
##   SI    0  513
```

```r
rbind(table(main.dat$West_vs_post_comm,
            useNA="always"),
      100*prop.table(table(main.dat$West_vs_post_comm,
                           useNA="always")))
```

```
##             -0.5        0.5 <NA>
## [1,] 15718.00000 4746.00000    0
## [2,]    76.80805   23.19195    0
```
