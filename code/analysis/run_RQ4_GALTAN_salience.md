---
title: "RQ4. GAL-TAN Saliency Moderation"
output: 
  html_document: 
    toc: yes
    keep_md: yes
date: '2022-09-01'
---



RQ4. If, as expected (H3), some personal values are associated with the GAL-TAN position of the party for which one has voted, are those associations stronger if this dimension is salient in the party’s public stance (i.e., is there a boundary condition to these associations, such that the ideology needs to be salient in order for the associations to exist)?
H6. We expect the associations between personal values and GAL-TAN to be stronger if this dimension is salient in the party’s public stance.

H3 confirmed association for conformity, tradition, and security. (Not confirmed for self-direction)


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
                  "galtan.z",
                  "galtan_salience.z",
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()

temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(value.vars,
                               value.vars.c,
                               "galtan_salience.z",
                               "gndr.c",
                               "age10.c"))
```

# Analysis

## con

### fixed effect model


```r
mod2.con<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+con.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.con)
```

```
##              Est.   SE       df     t     p    LL    UL
## (Intercept) -0.01 0.06    19.19 -0.20 0.844 -0.14  0.12
## gndr.c.gmc  -0.05 0.01 20481.11 -4.02 0.000 -0.07 -0.02
## age10.c.gmc  0.03 0.00 20481.83  7.36 0.000  0.02  0.03
## con.c.gmc    0.11 0.01 20480.45 17.66 0.000  0.10  0.12
```

### salience main effect


```r
mod3.con<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+con.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.con)
```

```
##                        Est.   SE       df     t     p    LL    UL
## (Intercept)           -0.01 0.06    19.18 -0.21 0.833 -0.14  0.12
## gndr.c.gmc            -0.05 0.01 20480.08 -4.33 0.000 -0.07 -0.03
## age10.c.gmc            0.03 0.00 20480.78  9.20 0.000  0.02  0.04
## con.c.gmc              0.11 0.01 20479.44 17.60 0.000  0.10  0.12
## galtan_salience.z.gmc  0.23 0.01 20479.39 26.24 0.000  0.22  0.25
```

### salience moderation


```r
mod4.con<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+con.c.gmc+
                 galtan_salience.z.gmc+
             con.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.con)
```

```
##                                  Est.   SE       df     t     p
## (Intercept)                     -0.01 0.06    19.19 -0.14 0.888
## gndr.c.gmc                      -0.05 0.01 20479.07 -4.18 0.000
## age10.c.gmc                      0.03 0.00 20479.77  9.25 0.000
## con.c.gmc                        0.10 0.01 20478.43 16.84 0.000
## galtan_salience.z.gmc            0.24 0.01 20478.34 27.55 0.000
## con.c.gmc:galtan_salience.z.gmc  0.18 0.01 20481.42 19.00 0.000
##                                    LL    UL
## (Intercept)                     -0.14  0.12
## gndr.c.gmc                      -0.07 -0.02
## age10.c.gmc                      0.02  0.04
## con.c.gmc                        0.09  0.12
## galtan_salience.z.gmc            0.23  0.26
## con.c.gmc:galtan_salience.z.gmc  0.16  0.20
```

```r
emtrends(mod4.con,
         var="con.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  galtan_salience.z.gmc con.c.gmc.trend      SE    df lower.CL
##                     -1         -0.0747 0.01144 20480  -0.0971
##                      0          0.1033 0.00614 20478   0.0913
##                      1          0.2813 0.01095 20481   0.2599
##  upper.CL t.ratio p.value
##   -0.0522  -6.527  <.0001
##    0.1154  16.841  <.0001
##    0.3028  25.680  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```


### figure data


```r
by=0.05

p.con<-
  emmip(mod4.con,
        galtan_salience.z.gmc ~ con.c.gmc,
        at=list(galtan_salience.z.gmc = c(-1,0,1),
                con.c.gmc=
                  seq(from=
                        round(range(temp.fdat$con.c.gmc)[1],2),
                      to=round(range(temp.fdat$con.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.con$galtan_salience<-p.con$tvar

levels(p.con$galtan_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "con.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "con.c.gmc"])

p.con$filter.low<-
  ifelse(p.con$galtan_salience.z.gmc==(-1) &
           (p.con$con.c.gmc<min.low | 
              p.con$con.c.gmc>max.low),0,1)

table(p.con$filter.low)
```

```
## 
##   0   1 
##  33 387
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "con.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "con.c.gmc"])

p.con$filter.mid<-
  ifelse(p.con$galtan_salience.z.gmc==(0) &
           (p.con$con.c.gmc<min.mid | 
              p.con$con.c.gmc>max.mid),0,1)

table(p.con$filter.mid)
```

```
## 
##   0   1 
##   1 419
```

```r
min.high<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "con.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "con.c.gmc"])

p.con$filter.high<-
  ifelse(p.con$galtan_salience.z.gmc==(1) &
           (p.con$con.c.gmc<min.high | 
              p.con$con.c.gmc>max.high),0,1)

table(p.con$filter.high)
```

```
## 
##   0   1 
##   7 413
```

```r
export(p.con,overwrite=T,
       "../../results/figures/figdata/con_GALTAN_salience.xlsx")
```

## tra

### fixed effect model


```r
mod2.tra<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+tra.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.tra)
```

```
##              Est.   SE       df     t    p    LL    UL
## (Intercept) -0.01 0.06    19.18 -0.20 0.84 -0.14  0.12
## gndr.c.gmc  -0.06 0.01 20481.08 -5.30 0.00 -0.08 -0.04
## age10.c.gmc  0.03 0.00 20481.77  7.62 0.00  0.02  0.03
## tra.c.gmc    0.11 0.01 20480.41 16.33 0.00  0.10  0.12
```

### salience main effect


```r
mod3.tra<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+tra.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.tra)
```

```
##                        Est.   SE       df     t     p    LL    UL
## (Intercept)           -0.01 0.06    19.18 -0.22 0.829 -0.15  0.12
## gndr.c.gmc            -0.06 0.01 20480.04 -5.58 0.000 -0.08 -0.04
## age10.c.gmc            0.03 0.00 20480.72  9.53 0.000  0.03  0.04
## tra.c.gmc              0.11 0.01 20479.40 15.92 0.000  0.09  0.12
## galtan_salience.z.gmc  0.23 0.01 20479.39 26.02 0.000  0.22  0.25
```

### salience moderation


```r
mod4.tra<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+tra.c.gmc+
                 galtan_salience.z.gmc+
             tra.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.tra)
```

```
##                                  Est.   SE       df     t     p
## (Intercept)                     -0.01 0.06    19.19 -0.20 0.844
## gndr.c.gmc                      -0.06 0.01 20479.05 -5.58 0.000
## age10.c.gmc                      0.03 0.00 20479.74  9.32 0.000
## tra.c.gmc                        0.11 0.01 20478.41 15.87 0.000
## galtan_salience.z.gmc            0.24 0.01 20478.36 26.96 0.000
## tra.c.gmc:galtan_salience.z.gmc  0.15 0.01 20480.69 14.91 0.000
##                                    LL    UL
## (Intercept)                     -0.14  0.12
## gndr.c.gmc                      -0.08 -0.04
## age10.c.gmc                      0.02  0.04
## tra.c.gmc                        0.09  0.12
## galtan_salience.z.gmc            0.22  0.26
## tra.c.gmc:galtan_salience.z.gmc  0.13  0.17
```

```r
emtrends(mod4.tra,
         var="tra.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  galtan_salience.z.gmc tra.c.gmc.trend      SE    df lower.CL
##                     -1         -0.0468 0.01221 20480  -0.0707
##                      0          0.1052 0.00663 20478   0.0922
##                      1          0.2572 0.01211 20480   0.2335
##  upper.CL t.ratio p.value
##   -0.0229  -3.831  0.0001
##    0.1182  15.865  <.0001
##    0.2810  21.243  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```

### figure data


```r
by=0.05

p.tra<-
  emmip(mod4.tra,
        galtan_salience.z.gmc ~ tra.c.gmc,
        at=list(galtan_salience.z.gmc = c(-1,0,1),
                tra.c.gmc=
                  seq(from=
                        round(range(temp.fdat$tra.c.gmc)[1],2),
                      to=round(range(temp.fdat$tra.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.tra$galtan_salience<-p.tra$tvar

levels(p.tra$galtan_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "tra.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "tra.c.gmc"])

p.tra$filter.low<-
  ifelse(p.tra$galtan_salience.z.gmc==(-1) &
           (p.tra$tra.c.gmc<min.low | 
              p.tra$tra.c.gmc>max.low),0,1)

table(p.tra$filter.low)
```

```
## 
##   0   1 
##  35 376
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "tra.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "tra.c.gmc"])

p.tra$filter.mid<-
  ifelse(p.tra$galtan_salience.z.gmc==(0) &
           (p.tra$tra.c.gmc<min.mid | 
              p.tra$tra.c.gmc>max.mid),0,1)

table(p.tra$filter.mid)
```

```
## 
##   1 
## 411
```

```r
min.high<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "tra.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "tra.c.gmc"])

p.tra$filter.high<-
  ifelse(p.tra$galtan_salience.z.gmc==(1) &
           (p.tra$tra.c.gmc<min.high | 
              p.tra$tra.c.gmc>max.high),0,1)

table(p.tra$filter.high)
```

```
## 
##   0   1 
##  13 398
```

```r
export(p.tra,overwrite=T,
       "../../results/figures/figdata/tra_GALTAN_salience.xlsx")
```

## sec

### fixed effect model


```r
mod2.sec<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sec.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.sec)
```

```
##              Est.   SE       df     t     p    LL    UL
## (Intercept) -0.01 0.06    19.19 -0.21 0.836 -0.15  0.12
## gndr.c.gmc  -0.08 0.01 20481.10 -6.90 0.000 -0.10 -0.06
## age10.c.gmc  0.02 0.00 20481.79  7.32 0.000  0.02  0.03
## sec.c.gmc    0.16 0.01 20480.47 22.61 0.000  0.14  0.17
```

### salience main effect


```r
mod3.sec<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sec.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.sec)
```

```
##                        Est.   SE       df     t     p    LL    UL
## (Intercept)           -0.01 0.06    19.18 -0.22 0.825 -0.15  0.12
## gndr.c.gmc            -0.08 0.01 20480.07 -7.24 0.000 -0.10 -0.06
## age10.c.gmc            0.03 0.00 20480.73  9.14 0.000  0.02  0.04
## sec.c.gmc              0.16 0.01 20479.46 22.83 0.000  0.14  0.17
## galtan_salience.z.gmc  0.24 0.01 20479.39 26.47 0.000  0.22  0.25
```

### salience moderation


```r
mod4.sec<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sec.c.gmc+
                 galtan_salience.z.gmc+
             sec.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.sec)
```

```
##                                  Est.   SE       df     t     p
## (Intercept)                     -0.01 0.06    19.20 -0.17 0.865
## gndr.c.gmc                      -0.08 0.01 20479.07 -7.21 0.000
## age10.c.gmc                      0.03 0.00 20479.74  9.18 0.000
## sec.c.gmc                        0.15 0.01 20478.46 21.71 0.000
## galtan_salience.z.gmc            0.25 0.01 20478.37 28.35 0.000
## sec.c.gmc:galtan_salience.z.gmc  0.24 0.01 20480.26 24.23 0.000
##                                    LL    UL
## (Intercept)                     -0.14  0.12
## gndr.c.gmc                      -0.10 -0.06
## age10.c.gmc                      0.02  0.04
## sec.c.gmc                        0.13  0.16
## galtan_salience.z.gmc            0.23  0.27
## sec.c.gmc:galtan_salience.z.gmc  0.22  0.26
```

```r
emtrends(mod4.sec,
         var="sec.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  galtan_salience.z.gmc sec.c.gmc.trend     SE    df lower.CL
##                     -1         -0.0928 0.0124 20480   -0.117
##                      0          0.1476 0.0068 20478    0.134
##                      1          0.3881 0.0117 20480    0.365
##  upper.CL t.ratio p.value
##   -0.0686  -7.515  <.0001
##    0.1610  21.708  <.0001
##    0.4110  33.168  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```

### figure data


```r
by=0.05

p.sec<-
  emmip(mod4.sec,
        galtan_salience.z.gmc ~ sec.c.gmc,
        at=list(galtan_salience.z.gmc = c(-1,0,1),
                sec.c.gmc=
                  seq(from=
                        round(range(temp.fdat$sec.c.gmc)[1],2),
                      to=round(range(temp.fdat$sec.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.sec$galtan_salience<-p.sec$tvar

levels(p.sec$galtan_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "sec.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "sec.c.gmc"])

p.sec$filter.low<-
  ifelse(p.sec$galtan_salience.z.gmc==(-1) &
           (p.sec$sec.c.gmc<min.low | 
              p.sec$sec.c.gmc>max.low),0,1)

table(p.sec$filter.low)
```

```
## 
##   0   1 
##  20 340
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "sec.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "sec.c.gmc"])

p.sec$filter.mid<-
  ifelse(p.sec$galtan_salience.z.gmc==(0) &
           (p.sec$sec.c.gmc<min.mid | 
              p.sec$sec.c.gmc>max.mid),0,1)

table(p.sec$filter.mid)
```

```
## 
##   1 
## 360
```

```r
min.high<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "sec.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "sec.c.gmc"])

p.sec$filter.high<-
  ifelse(p.sec$galtan_salience.z.gmc==(1) &
           (p.sec$sec.c.gmc<min.high | 
              p.sec$sec.c.gmc>max.high),0,1)

table(p.sec$filter.high)
```

```
## 
##   0   1 
##  14 346
```

```r
export(p.sec,overwrite=T,
       "../../results/figures/figdata/sec_GALTAN_salience.xlsx")
```

## sdi

### fixed effect model


```r
mod2.sdi<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sdi.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.sdi)
```

```
##              Est.   SE       df     t     p    LL    UL
## (Intercept) -0.01 0.06    19.19 -0.20 0.847 -0.14  0.12
## gndr.c.gmc  -0.05 0.01 20481.12 -4.29 0.000 -0.07 -0.03
## age10.c.gmc  0.04 0.00 20481.88 12.08 0.000  0.03  0.05
## sdi.c.gmc   -0.07 0.01 20480.52 -8.99 0.000 -0.08 -0.05
```

### salience main effect


```r
mod3.sdi<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sdi.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.sdi)
```

```
##                        Est.   SE       df     t     p    LL    UL
## (Intercept)           -0.01 0.06    19.18 -0.21 0.836 -0.14  0.12
## gndr.c.gmc            -0.05 0.01 20480.08 -4.61 0.000 -0.07 -0.03
## age10.c.gmc            0.05 0.00 20480.82 13.97 0.000  0.04  0.05
## sdi.c.gmc             -0.07 0.01 20479.51 -9.54 0.000 -0.08 -0.06
## galtan_salience.z.gmc  0.24 0.01 20479.39 26.47 0.000  0.22  0.26
```

### salience moderation


```r
mod4.sdi<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sdi.c.gmc+
                 galtan_salience.z.gmc+
             sdi.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.sdi)
```

```
##                                  Est.   SE       df     t     p
## (Intercept)                     -0.01 0.06    19.19 -0.19 0.854
## gndr.c.gmc                      -0.05 0.01 20479.09 -4.57 0.000
## age10.c.gmc                      0.05 0.00 20479.83 13.99 0.000
## sdi.c.gmc                       -0.07 0.01 20478.51 -9.47 0.000
## galtan_salience.z.gmc            0.24 0.01 20478.37 26.84 0.000
## sdi.c.gmc:galtan_salience.z.gmc -0.09 0.01 20479.45 -8.13 0.000
##                                    LL    UL
## (Intercept)                     -0.14  0.12
## gndr.c.gmc                      -0.07 -0.03
## age10.c.gmc                      0.04  0.05
## sdi.c.gmc                       -0.08 -0.05
## galtan_salience.z.gmc            0.22  0.26
## sdi.c.gmc:galtan_salience.z.gmc -0.12 -0.07
```

```r
emtrends(mod4.sdi,
         var="sdi.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))
```

```
##  galtan_salience.z.gmc sdi.c.gmc.trend      SE    df lower.CL
##                     -1          0.0257 0.01379 20479  -0.0013
##                      0         -0.0689 0.00728 20479  -0.0832
##                      1         -0.1635 0.01366 20479  -0.1903
##  upper.CL t.ratio p.value
##    0.0528   1.866  0.0620
##   -0.0546  -9.467  <.0001
##   -0.1367 -11.973  <.0001
## 
## Degrees-of-freedom method: satterthwaite 
## Confidence level used: 0.95
```

### figure data


```r
by=0.05

p.sdi<-
  emmip(mod4.sdi,
        galtan_salience.z.gmc ~ sdi.c.gmc,
        at=list(galtan_salience.z.gmc = c(-1,0,1),
                sdi.c.gmc=
                  seq(from=
                        round(range(temp.fdat$sdi.c.gmc)[1],2),
                      to=round(range(temp.fdat$sdi.c.gmc)[2],
                               2),by=by)),
        plotit=F,CIs=TRUE,type="response",
        disable.pbkrtest=T,lmerTest.limit=30000)

p.sdi$galtan_salience<-p.sdi$tvar

levels(p.sdi$galtan_salience)<-
  c("Low Salience","Average Salience","High Salience")

min.low<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "sdi.c.gmc"])
max.low<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc<=(-1),
                "sdi.c.gmc"])

p.sdi$filter.low<-
  ifelse(p.sdi$galtan_salience.z.gmc==(-1) &
           (p.sdi$sdi.c.gmc<min.low | 
              p.sdi$sdi.c.gmc>max.low),0,1)

table(p.sdi$filter.low)
```

```
## 
##   0   1 
##  20 370
```

```r
min.mid<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "sdi.c.gmc"])
max.mid<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>(-1) |
                  temp.fdat$galtan_salience.z.gmc<(1),
                "sdi.c.gmc"])

p.sdi$filter.mid<-
  ifelse(p.sdi$galtan_salience.z.gmc==(0) &
           (p.sdi$sdi.c.gmc<min.mid | 
              p.sdi$sdi.c.gmc>max.mid),0,1)

table(p.sdi$filter.mid)
```

```
## 
##   1 
## 390
```

```r
min.high<-
  min(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "sdi.c.gmc"])
max.high<-
  max(temp.fdat[temp.fdat$galtan_salience.z.gmc>=(1) ,
                "sdi.c.gmc"])

p.sdi$filter.high<-
  ifelse(p.sdi$galtan_salience.z.gmc==(1) &
           (p.sdi$sdi.c.gmc<min.high | 
              p.sdi$sdi.c.gmc>max.high),0,1)

table(p.sdi$filter.high)
```

```
## 
##   0   1 
##   4 386
```

```r
export(p.sdi,overwrite=T,
       "../../results/figures/figdata/sdi_GALTAN_salience.xlsx")
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
