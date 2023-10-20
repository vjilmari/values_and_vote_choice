---
title: "RQ3. Economic Left-Right Saliency Moderation"
output: 
  html_document: 
    toc: yes
    keep_md: yes
date: '2022-09-01'
---



RQ3. If, as expected (H2), some personal values are associated with the Economic Left-Right position of the party for which one has voted, are those associations stronger if this dimension is salient in the party’s public stance (i.e., is there a boundary condition to these associations, such that the ideology needs to be salient in order for the associations to exist)?
H5. We expect the associations between personal values and Economic Left-Right to be stronger if this dimension is salient in the party’s public stance.

H2 confirmed association for universalism and power values.


# Preparations

## Packages


```r
library(lme4)
```

```
## Loading required package: Matrix
```

```r
library(lmerTest)
```

```
## 
## Attaching package: 'lmerTest'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmer
```

```
## The following object is masked from 'package:stats':
## 
##     step
```

```r
library(emmeans)
library(r2mlm)
```

```
## Loading required package: nlme
```

```
## 
## Attaching package: 'nlme'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmList
```

```
## Registered S3 method overwritten by 'parameters':
##   method                         from      
##   format.parameters_distribution datawizard
```

```r
library(rio)
```

```
## The following rio suggested packages are not installed: 'arrow', 'feather', 'fst', 'hexView', 'pzfx', 'readODS', 'rmatio'
## Use 'install_formats()' to install them
```

```
## 
## Attaching package: 'rio'
```

```
## The following object is masked from 'package:lme4':
## 
##     factorize
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:nlme':
## 
##     collapse
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(vjihelpers)
library(psych)
library(tibble)
```

## Functions


```r
source("../custom_functions.R")
```

## Data


```r
fdat<-import("../../data/processed/fdat.xlsx")
```

## Variables


```r
value.vars<-
  c("con","tra","ben","uni","sdi",
        "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]
```

## Data filtering and centering predictors


```r
temp.fdat<- fdat %>% 
    dplyr::select(all_of(value.vars),
                  all_of(value.vars.c),
                  "lrecon.z",
                  "lrecon_salience.z",
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()

temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(value.vars,
                               value.vars.c,
                               "lrecon_salience.z",
                               "gndr.c",
                               "age10.c"))
```

# Analysis

## uni

### fixed effect model


```r
mod2.uni<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+uni.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.uni)
```

```
##              Est.   SE       df      t     p    LL    UL
## (Intercept)  0.14 0.05    18.06   2.87 0.010  0.04  0.24
## gndr.c.gmc  -0.02 0.01 20480.81  -1.26 0.208 -0.04  0.01
## age10.c.gmc  0.03 0.00 20482.31   8.90 0.000  0.02  0.04
## uni.c.gmc   -0.26 0.01 20479.33 -26.60 0.000 -0.27 -0.24
```

### salience main effect


```r
mod3.uni<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+uni.c.gmc+
                 lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.uni)
```

```
##                        Est.   SE       df      t     p    LL    UL
## (Intercept)            0.14 0.05    18.13   2.89 0.010  0.04  0.24
## gndr.c.gmc            -0.02 0.01 20479.78  -1.58 0.115 -0.04  0.00
## age10.c.gmc            0.02 0.00 20481.23   5.80 0.000  0.01  0.03
## uni.c.gmc             -0.24 0.01 20478.39 -26.31 0.000 -0.26 -0.23
## lrecon_salience.z.gmc  0.36 0.01 20478.69  38.17 0.000  0.34  0.37
```

### salience moderation


```r
mod4.uni<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+uni.c.gmc+
                 lrecon_salience.z.gmc+
             uni.c.gmc:lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.uni)
```

```
##                                  Est.   SE       df      t     p
## (Intercept)                      0.14 0.05    18.14   2.91 0.009
## gndr.c.gmc                      -0.02 0.01 20478.81  -1.72 0.085
## age10.c.gmc                      0.02 0.00 20480.26   5.69 0.000
## uni.c.gmc                       -0.24 0.01 20477.40 -25.96 0.000
## lrecon_salience.z.gmc            0.36 0.01 20477.83  38.57 0.000
## uni.c.gmc:lrecon_salience.z.gmc  0.08 0.01 20480.64   5.53 0.000
##                                    LL    UL
## (Intercept)                      0.04  0.24
## gndr.c.gmc                      -0.04  0.00
## age10.c.gmc                      0.01  0.03
## uni.c.gmc                       -0.26 -0.22
## lrecon_salience.z.gmc            0.34  0.38
## uni.c.gmc:lrecon_salience.z.gmc  0.05  0.11
```

```r
emtrends(mod4.uni,
         var="uni.c.gmc",
         specs="lrecon_salience.z.gmc",
         at=list(lrecon_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  lrecon_salience.z.gmc uni.c.gmc.trend      SE    df lower.CL
##                     -1          -0.319 0.01641 20480   -0.351
##                      0          -0.241 0.00929 20477   -0.259
##                      1          -0.163 0.01733 20480   -0.197
##  upper.CL t.ratio p.value
##    -0.287 -19.441  <.0001
##    -0.223 -25.958  <.0001
##    -0.129  -9.413  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```

### figure data


```r
by=0.05

p.uni<-
  emmip(mod4.uni,
        lrecon_salience.z.gmc ~ uni.c.gmc,
        at=list(lrecon_salience.z.gmc = c(-1,0,1),
                uni.c.gmc=
                  seq(from=
                        round(range(temp.fdat$uni.c.gmc)[1],2),
                      to=round(range(temp.fdat$uni.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.uni$lrecon_salience<-p.uni$tvar

levels(p.uni$lrecon_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "uni.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "uni.c.gmc"])

p.uni$filter.low<-
  ifelse(p.uni$lrecon_salience.z.gmc==(-1) &
           (p.uni$uni.c.gmc<min.low | 
              p.uni$uni.c.gmc>max.low),0,1)

table(p.uni$filter.low)
```

```
## 
##   0   1 
##  27 354
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "uni.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "uni.c.gmc"])

p.uni$filter.mid<-
  ifelse(p.uni$lrecon_salience.z.gmc==(0) &
           (p.uni$uni.c.gmc<min.mid | 
              p.uni$uni.c.gmc>max.mid),0,1)

table(p.uni$filter.mid)
```

```
## 
##   0   1 
##   1 380
```

```r
min.high<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "uni.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "uni.c.gmc"])

p.uni$filter.high<-
  ifelse(p.uni$lrecon_salience.z.gmc==(1) &
           (p.uni$uni.c.gmc<min.high | 
              p.uni$uni.c.gmc>max.high),0,1)

table(p.uni$filter.high)
```

```
## 
##   0   1 
##  54 327
```

```r
export(p.uni,overwrite=T,
       "../../results/figures/figdata/uni_ELR_salience.xlsx")
```

## pow

### fixed effect model


```r
mod2.pow<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+pow.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.pow)
```

```
##              Est.   SE       df     t     p    LL    UL
## (Intercept)  0.14 0.05    18.06  2.89 0.010  0.04  0.24
## gndr.c.gmc  -0.04 0.01 20480.77 -2.89 0.004 -0.06 -0.01
## age10.c.gmc  0.03 0.00 20482.43  7.13 0.000  0.02  0.03
## pow.c.gmc    0.06 0.01 20479.81  7.89 0.000  0.04  0.07
```

### salience main effect


```r
mod3.pow<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+pow.c.gmc+
                 lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.pow)
```

```
##                        Est.   SE       df     t     p    LL    UL
## (Intercept)            0.14 0.05    18.13  2.91 0.009  0.04  0.24
## gndr.c.gmc            -0.04 0.01 20479.74 -3.24 0.001 -0.06 -0.02
## age10.c.gmc            0.01 0.00 20481.34  4.02 0.000  0.01  0.02
## pow.c.gmc              0.05 0.01 20478.84  7.46 0.000  0.04  0.07
## lrecon_salience.z.gmc  0.36 0.01 20478.71 38.29 0.000  0.34  0.38
```

### salience moderation


```r
mod4.pow<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+pow.c.gmc+
                 lrecon_salience.z.gmc+
             pow.c.gmc:lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.pow)
```

```
##                                  Est.   SE       df     t     p
## (Intercept)                      0.14 0.05    18.12  2.90 0.009
## gndr.c.gmc                      -0.04 0.01 20478.77 -3.22 0.001
## age10.c.gmc                      0.01 0.00 20480.36  4.03 0.000
## pow.c.gmc                        0.05 0.01 20477.84  7.46 0.000
## lrecon_salience.z.gmc            0.36 0.01 20477.72 38.24 0.000
## pow.c.gmc:lrecon_salience.z.gmc  0.01 0.01 20479.67  1.15 0.249
##                                    LL    UL
## (Intercept)                      0.04  0.24
## gndr.c.gmc                      -0.06 -0.02
## age10.c.gmc                      0.01  0.02
## pow.c.gmc                        0.04  0.07
## lrecon_salience.z.gmc            0.34  0.38
## pow.c.gmc:lrecon_salience.z.gmc -0.01  0.04
```

```r
emtrends(mod4.pow,
         var="pow.c.gmc",
         specs="lrecon_salience.z.gmc",
         at=list(lrecon_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  lrecon_salience.z.gmc pow.c.gmc.trend      SE    df lower.CL
##                     -1          0.0406 0.01342 20479   0.0143
##                      0          0.0536 0.00719 20478   0.0396
##                      1          0.0667 0.01345 20479   0.0404
##  upper.CL t.ratio p.value
##    0.0669   3.022  0.0025
##    0.0677   7.460  <.0001
##    0.0931   4.962  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```

### figure data


```r
by=0.05

p.pow<-
  emmip(mod4.pow,
        lrecon_salience.z.gmc ~ pow.c.gmc,
        at=list(lrecon_salience.z.gmc = c(-1,0,1),
                pow.c.gmc=
                  seq(from=
                        round(range(temp.fdat$pow.c.gmc)[1],2),
                      to=round(range(temp.fdat$pow.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.pow$lrecon_salience<-p.pow$tvar

levels(p.pow$lrecon_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "pow.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "pow.c.gmc"])

p.pow$filter.low<-
  ifelse(p.pow$lrecon_salience.z.gmc==(-1) &
           (p.pow$pow.c.gmc<min.low | 
              p.pow$pow.c.gmc>max.low),0,1)

table(p.pow$filter.low)
```

```
## 
##   0   1 
##  39 414
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "pow.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "pow.c.gmc"])

p.pow$filter.mid<-
  ifelse(p.pow$lrecon_salience.z.gmc==(0) &
           (p.pow$pow.c.gmc<min.mid | 
              p.pow$pow.c.gmc>max.mid),0,1)

table(p.pow$filter.mid)
```

```
## 
##   1 
## 453
```

```r
min.high<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "pow.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "pow.c.gmc"])

p.pow$filter.high<-
  ifelse(p.pow$lrecon_salience.z.gmc==(1) &
           (p.pow$pow.c.gmc<min.high | 
              p.pow$pow.c.gmc>max.high),0,1)

table(p.pow$filter.high)
```

```
## 
##   0   1 
##  82 371
```

```r
export(p.pow,overwrite=T,
       "../../results/figures/figdata/pow_ELR_salience.xlsx")
```

## ben

### fixed effect model


```r
mod2.ben<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+ben.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.ben)
```

```
##              Est.   SE       df     t     p    LL    UL
## (Intercept)  0.14 0.05    18.06  2.89 0.010  0.04  0.24
## gndr.c.gmc  -0.04 0.01 20480.88 -2.85 0.004 -0.06 -0.01
## age10.c.gmc  0.02 0.00 20482.43  6.82 0.000  0.02  0.03
## ben.c.gmc   -0.06 0.01 20479.34 -5.86 0.000 -0.08 -0.04
```

### salience main effect


```r
mod3.ben<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+ben.c.gmc+
                 lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.ben)
```

```
##                        Est.   SE       df     t     p    LL    UL
## (Intercept)            0.14 0.05    18.13  2.90 0.009  0.04  0.24
## gndr.c.gmc            -0.04 0.01 20479.84 -3.10 0.002 -0.06 -0.01
## age10.c.gmc            0.01 0.00 20481.34  3.74 0.000  0.01  0.02
## ben.c.gmc             -0.06 0.01 20478.39 -6.02 0.000 -0.08 -0.04
## lrecon_salience.z.gmc  0.36 0.01 20478.71 38.40 0.000  0.34  0.38
```

### salience moderation


```r
mod4.ben<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+ben.c.gmc+
                 lrecon_salience.z.gmc+
             ben.c.gmc:lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.ben)
```

```
##                                  Est.   SE       df     t     p
## (Intercept)                      0.14 0.05    18.13  2.91 0.009
## gndr.c.gmc                      -0.04 0.01 20478.83 -3.11 0.002
## age10.c.gmc                      0.01 0.00 20480.35  3.76 0.000
## ben.c.gmc                       -0.06 0.01 20477.39 -6.02 0.000
## lrecon_salience.z.gmc            0.36 0.01 20477.71 38.34 0.000
## ben.c.gmc:lrecon_salience.z.gmc -0.02 0.02 20478.34 -1.28 0.201
##                                    LL    UL
## (Intercept)                      0.04  0.24
## gndr.c.gmc                      -0.06 -0.01
## age10.c.gmc                      0.01  0.02
## ben.c.gmc                       -0.08 -0.04
## lrecon_salience.z.gmc            0.34  0.38
## ben.c.gmc:lrecon_salience.z.gmc -0.05  0.01
```

```r
emtrends(mod4.ben,
         var="ben.c.gmc",
         specs="lrecon_salience.z.gmc",
         at=list(lrecon_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  lrecon_salience.z.gmc ben.c.gmc.trend      SE    df lower.CL
##                     -1         -0.0400 0.01851 20478  -0.0763
##                      0         -0.0599 0.00995 20477  -0.0795
##                      1         -0.0799 0.01849 20478  -0.1161
##  upper.CL t.ratio p.value
##  -0.00373  -2.162  0.0307
##  -0.04044  -6.022  <.0001
##  -0.04364  -4.320  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```

### figure data


```r
by=0.05

p.ben<-
  emmip(mod4.ben,
        lrecon_salience.z.gmc ~ ben.c.gmc,
        at=list(lrecon_salience.z.gmc = c(-1,0,1),
                ben.c.gmc=
                  seq(from=
                        round(range(temp.fdat$ben.c.gmc)[1],2),
                      to=round(range(temp.fdat$ben.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.ben$lrecon_salience<-p.ben$tvar

levels(p.ben$lrecon_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "ben.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "ben.c.gmc"])

p.ben$filter.low<-
  ifelse(p.ben$lrecon_salience.z.gmc==(-1) &
           (p.ben$ben.c.gmc<min.low | 
              p.ben$ben.c.gmc>max.low),0,1)

table(p.ben$filter.low)
```

```
## 
##   0   1 
##   3 342
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "ben.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "ben.c.gmc"])

p.ben$filter.mid<-
  ifelse(p.ben$lrecon_salience.z.gmc==(0) &
           (p.ben$ben.c.gmc<min.mid | 
              p.ben$ben.c.gmc>max.mid),0,1)

table(p.ben$filter.mid)
```

```
## 
##   1 
## 345
```

```r
min.high<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "ben.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "ben.c.gmc"])

p.ben$filter.high<-
  ifelse(p.ben$lrecon_salience.z.gmc==(1) &
           (p.ben$ben.c.gmc<min.high | 
              p.ben$ben.c.gmc>max.high),0,1)

table(p.ben$filter.high)
```

```
## 
##   0   1 
##  25 320
```

```r
export(p.ben,overwrite=T,
       "../../results/figures/figdata/ben_ELR_salience.xlsx")
```

## ach

### fixed effect model


```r
mod2.ach<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+ach.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.ach)
```

```
##              Est.   SE       df     t     p    LL    UL
## (Intercept)  0.14 0.05    18.05  2.88 0.010  0.04  0.24
## gndr.c.gmc  -0.04 0.01 20480.84 -3.48 0.001 -0.07 -0.02
## age10.c.gmc  0.03 0.00 20482.24  7.52 0.000  0.02  0.03
## ach.c.gmc    0.03 0.01 20479.36  4.68 0.000  0.02  0.04
```

### salience main effect


```r
mod3.ach<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+ach.c.gmc+
                 lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.ach)
```

```
##                        Est.   SE       df     t     p    LL    UL
## (Intercept)            0.14 0.05    18.12  2.90 0.009  0.04  0.24
## gndr.c.gmc            -0.05 0.01 20479.81 -3.87 0.000 -0.07 -0.02
## age10.c.gmc            0.02 0.00 20481.17  4.32 0.000  0.01  0.02
## ach.c.gmc              0.02 0.01 20478.42  3.83 0.000  0.01  0.04
## lrecon_salience.z.gmc  0.36 0.01 20478.71 38.28 0.000  0.34  0.38
```

### salience moderation


```r
mod4.ach<-lmer(lrecon.z~gndr.c.gmc+age10.c.gmc+ach.c.gmc+
                 lrecon_salience.z.gmc+
             ach.c.gmc:lrecon_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.ach)
```

```
##                                  Est.   SE       df     t     p
## (Intercept)                      0.14 0.05    18.11  2.90 0.010
## gndr.c.gmc                      -0.05 0.01 20478.80 -3.84 0.000
## age10.c.gmc                      0.02 0.00 20480.16  4.39 0.000
## ach.c.gmc                        0.02 0.01 20477.41  3.83 0.000
## lrecon_salience.z.gmc            0.36 0.01 20477.71 38.20 0.000
## ach.c.gmc:lrecon_salience.z.gmc  0.03 0.01 20478.85  3.07 0.002
##                                    LL    UL
## (Intercept)                      0.04  0.24
## gndr.c.gmc                      -0.07 -0.02
## age10.c.gmc                      0.01  0.02
## ach.c.gmc                        0.01  0.04
## lrecon_salience.z.gmc            0.34  0.38
## ach.c.gmc:lrecon_salience.z.gmc  0.01  0.05
```

```r
emtrends(mod4.ach,
         var="ach.c.gmc",
         specs="lrecon_salience.z.gmc",
         at=list(lrecon_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  lrecon_salience.z.gmc ach.c.gmc.trend      SE    df lower.CL
##                     -1         -0.0062 0.01195 20478  -0.0296
##                      0          0.0247 0.00644 20477   0.0121
##                      1          0.0556 0.01196 20478   0.0322
##  upper.CL t.ratio p.value
##    0.0172  -0.519  0.6039
##    0.0373   3.835  0.0001
##    0.0790   4.648  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```

### figure data


```r
by=0.05

p.ach<-
  emmip(mod4.ach,
        lrecon_salience.z.gmc ~ ach.c.gmc,
        at=list(lrecon_salience.z.gmc = c(-1,0,1),
                ach.c.gmc=
                  seq(from=
                        round(range(temp.fdat$ach.c.gmc)[1],2),
                      to=round(range(temp.fdat$ach.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.ach$lrecon_salience<-p.ach$tvar

levels(p.ach$lrecon_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "ach.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc<=(-1),
                "ach.c.gmc"])

p.ach$filter.low<-
  ifelse(p.ach$lrecon_salience.z.gmc==(-1) &
           (p.ach$ach.c.gmc<min.low | 
              p.ach$ach.c.gmc>max.low),0,1)

table(p.ach$filter.low)
```

```
## 
##   0   1 
##  25 389
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "ach.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>(-1) |
                  temp.fdat$lrecon_salience.z.gmc<(1),
                "ach.c.gmc"])

p.ach$filter.mid<-
  ifelse(p.ach$lrecon_salience.z.gmc==(0) &
           (p.ach$ach.c.gmc<min.mid | 
              p.ach$ach.c.gmc>max.mid),0,1)

table(p.ach$filter.mid)
```

```
## 
##   0   1 
##   1 413
```

```r
min.high<-
  min(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "ach.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$lrecon_salience.z.gmc>=(1) ,
                "ach.c.gmc"])

p.ach$filter.high<-
  ifelse(p.ach$lrecon_salience.z.gmc==(1) &
           (p.ach$ach.c.gmc<min.high | 
              p.ach$ach.c.gmc>max.high),0,1)

table(p.ach$filter.high)
```

```
## 
##   0   1 
##  46 368
```

```r
export(p.ach,overwrite=T,
       "../../results/figures/figdata/ach_ELR_salience.xlsx")
```

# Session info


```r
sinf<-sessionInfo()
print(sinf,locale=F)
```

```
## R version 4.2.1 (2022-06-23 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods  
## [7] base     
## 
## other attached packages:
##  [1] tibble_3.1.8          psych_2.2.3          
##  [3] vjihelpers_0.0.0.9000 dplyr_1.0.9          
##  [5] rio_0.5.29            r2mlm_0.3.1          
##  [7] nlme_3.1-157          emmeans_1.8.0        
##  [9] lmerTest_3.1-3        lme4_1.1-30          
## [11] Matrix_1.4-1          knitr_1.39           
## [13] rmarkdown_2.15       
## 
## loaded via a namespace (and not attached):
##  [1] sass_0.4.1          jsonlite_1.8.0      splines_4.2.1      
##  [4] carData_3.0-5       bslib_0.3.1         datawizard_0.4.0   
##  [7] assertthat_0.2.1    cellranger_1.1.0    yaml_2.3.5         
## [10] bayestestR_0.11.5   numDeriv_2016.8-1.1 pillar_1.8.0       
## [13] lattice_0.20-45     glue_1.6.2          digest_0.6.29      
## [16] minqa_1.2.4         colorspace_2.0-3    htmltools_0.5.2    
## [19] plyr_1.8.7          pkgconfig_2.0.3     haven_2.5.0        
## [22] purrr_0.3.4         xtable_1.8-4        mvtnorm_1.1-3      
## [25] scales_1.2.0        openxlsx_4.2.5      rockchalk_1.8.151  
## [28] generics_0.1.3      ggplot2_3.3.5       ellipsis_0.3.2     
## [31] broomExtra_4.3.2    mnormt_2.1.0        cli_3.3.0          
## [34] magrittr_2.0.3      readxl_1.4.0        estimability_1.4.1 
## [37] kutils_1.70         evaluate_0.16       fansi_1.0.3        
## [40] MASS_7.3-57         forcats_0.5.1       foreign_0.8-82     
## [43] tools_4.2.1         data.table_1.14.2   hms_1.1.1          
## [46] lifecycle_1.0.1     stringr_1.4.0       munsell_0.5.0      
## [49] zip_2.2.0           compiler_4.2.1      jquerylib_0.1.4    
## [52] rlang_1.0.4         grid_4.2.1          nloptr_2.0.3       
## [55] parameters_0.17.0   rstudioapi_0.13     boot_1.3-28        
## [58] gtable_0.3.0        DBI_1.1.2           curl_4.3.2         
## [61] R6_2.5.1            performance_0.9.0   fastmap_1.1.0      
## [64] utf8_1.2.2          insight_0.17.0      stringi_1.7.6      
## [67] parallel_4.2.1      Rcpp_1.0.9          vctrs_0.4.1        
## [70] tidyselect_1.1.2    xfun_0.30           coda_0.19-4
```
