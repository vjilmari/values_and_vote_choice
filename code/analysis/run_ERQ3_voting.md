---
title: "ERQ3. Are personal values associated with not having voted in the previous national elections, despite being eligible to vote?"
output: 
  html_document: 
    toc: yes
    keep_md: yes
---




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
#library(emmeans)
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
#library(psych)
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


table(fdat$vote)
```

```
## 
##     1     2     3 
## 27867  8492  3498
```

```r
table(fdat$vote.c)
```

```
## 
##  -0.5   0.5 
##  8492 27867
```

```r
fdat$vote.DV<-fdat$vote.c+0.5
table(fdat$vote.DV)
```

```
## 
##     0     1 
##  8492 27867
```

## Data filtering and centering predictors


```r
temp.fdat<- fdat %>% 
    dplyr::select(all_of(value.vars),
                  all_of(value.vars.c),
                  "vote.DV",
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()

temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(value.vars,
                               value.vars.c,
                               "gndr.c",
                               "age10.c"))
```

# Analysis

## Empty model


```r
mod0<-glmer(vote.DV~(1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod0)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  26634.4  26651.2 -13315.2  26630.4    33549 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.2173  0.1294  0.2291  0.4670  2.3902 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3158   0.5619  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   1.2195     0.1274   9.573   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Covariate model


```r
mod1<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25472.9  25506.6 -12732.5  25464.9    33547 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9797  0.0923  0.2174  0.4450  3.2122 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3364   0.58    
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.35652    0.13157  10.310  < 2e-16 ***
## gndr.c.gmc  -0.15137    0.03085  -4.907 9.23e-07 ***
## age10.c.gmc  0.30660    0.00939  32.652  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr..
## gndr.c.gmc  -0.002       
## age10.c.gmc  0.048 -0.047
```

## All values model


```r
mod.all<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  con.gmc+tra.gmc+ben.gmc+uni.gmc+sdi.gmc+
                 sti.gmc+hed.gmc+ach.gmc+pow.gmc+sec.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod.all)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + con.gmc + tra.gmc + ben.gmc +  
##     uni.gmc + sdi.gmc + sti.gmc + hed.gmc + ach.gmc + pow.gmc +  
##     sec.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25330.3  25448.2 -12651.2  25302.3    33537 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -10.8664   0.0900   0.2161   0.4391   3.4016 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3444   0.5869  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.367271   0.133061  10.276  < 2e-16 ***
## gndr.c.gmc  -0.169502   0.031977  -5.301 1.15e-07 ***
## age10.c.gmc  0.302637   0.010281  29.437  < 2e-16 ***
## con.gmc     -0.002751   0.017144  -0.160  0.87253    
## tra.gmc      0.015478   0.018440   0.839  0.40126    
## ben.gmc      0.067725   0.025008   2.708  0.00677 ** 
## uni.gmc      0.182842   0.024434   7.483 7.27e-14 ***
## sdi.gmc      0.059746   0.019202   3.111  0.00186 ** 
## sti.gmc     -0.042281   0.017048  -2.480  0.01313 *  
## hed.gmc     -0.007386   0.016737  -0.441  0.65898    
## ach.gmc     -0.018941   0.016783  -1.129  0.25909    
## pow.gmc     -0.008722   0.018306  -0.476  0.63373    
## sec.gmc     -0.133165   0.019054  -6.989 2.77e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 13 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
export(rownames_to_column(getFE_glmer(mod.all,p.round = 10,round = 10)),
       "../../results/ERQ3/all_FE.xlsx",overwrite=T)
```

## con

### fixed effect model


```r
mod2.con<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  con.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.con)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + con.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25470.7  25512.8 -12730.4  25460.7    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.2250  0.0923  0.2174  0.4444  3.1907 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.337    0.5805  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.35705    0.13169  10.305  < 2e-16 ***
## gndr.c.gmc  -0.15075    0.03085  -4.887 1.02e-06 ***
## age10.c.gmc  0.31153    0.00970  32.118  < 2e-16 ***
## con.c.gmc   -0.03475    0.01697  -2.047   0.0406 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.048 -0.043       
## con.c.gmc   -0.003 -0.010 -0.251
```

```r
export(rownames_to_column(getFE_glmer(mod2.con,p.round = 10,round = 10)),
       "../../results/ERQ3/con_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.con<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  con.c.gmc+
                  (con.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.con)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + con.c.gmc + (con.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25434.2  25493.2 -12710.1  25420.2    33544 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -10.2183   0.0926   0.2190   0.4426   3.2823 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr 
##  cntry  (Intercept) 0.30703  0.5541        
##         con.c.gmc   0.02111  0.1453   -0.97
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.35568    0.12589  10.769  < 2e-16 ***
## gndr.c.gmc  -0.15348    0.03092  -4.964  6.9e-07 ***
## age10.c.gmc  0.31237    0.00970  32.204  < 2e-16 ***
## con.c.gmc   -0.08840    0.03953  -2.236   0.0253 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.050 -0.042       
## con.c.gmc   -0.795 -0.005 -0.112
```

```r
anova(mod2.con,mod3.con)
```

```
## Data: temp.fdat
## Models:
## mod2.con: vote.DV ~ gndr.c.gmc + age10.c.gmc + con.c.gmc + (1 | cntry)
## mod3.con: vote.DV ~ gndr.c.gmc + age10.c.gmc + con.c.gmc + (con.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.con    5 25471 25513 -12730    25461                         
## mod3.con    7 25434 25493 -12710    25420 40.472  2  1.628e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## tra

### fixed effect model


```r
mod2.tra<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  tra.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.tra)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + tra.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25473.8  25515.9 -12731.9  25463.8    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9384  0.0922  0.2178  0.4454  3.2343 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3356   0.5793  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.356272   0.131406  10.321  < 2e-16 ***
## gndr.c.gmc  -0.153726   0.030928  -4.970 6.68e-07 ***
## age10.c.gmc  0.304019   0.009705  31.326  < 2e-16 ***
## tra.c.gmc    0.018819   0.018003   1.045    0.296    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.047 -0.027       
## tra.c.gmc   -0.001 -0.073 -0.253
```

```r
export(rownames_to_column(getFE_glmer(mod2.tra,p.round = 10,round = 10)),
       "../../results/ERQ3/tra_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.tra<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  tra.c.gmc+
                  (tra.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.tra)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + tra.c.gmc + (tra.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25465.5  25524.4 -12725.7  25451.5    33544 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.1002  0.0928  0.2181  0.4430  3.3397 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr 
##  cntry  (Intercept) 0.328379 0.57304       
##         tra.c.gmc   0.009054 0.09515  -0.90
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.359076   0.130023  10.453  < 2e-16 ***
## gndr.c.gmc  -0.152659   0.031003  -4.924 8.48e-07 ***
## age10.c.gmc  0.305635   0.009729  31.414  < 2e-16 ***
## tra.c.gmc   -0.029274   0.035949  -0.814    0.415    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.003              
## age10.c.gmc  0.047 -0.024       
## tra.c.gmc   -0.525 -0.059 -0.158
```

```r
anova(mod2.tra,mod3.tra)
```

```
## Data: temp.fdat
## Models:
## mod2.tra: vote.DV ~ gndr.c.gmc + age10.c.gmc + tra.c.gmc + (1 | cntry)
## mod3.tra: vote.DV ~ gndr.c.gmc + age10.c.gmc + tra.c.gmc + (tra.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
## mod2.tra    5 25474 25516 -12732    25464                        
## mod3.tra    7 25466 25524 -12726    25452 12.364  2   0.002066 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```




## ben

### fixed effect model


```r
mod2.ben<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  ben.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.ben)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + ben.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25448.1  25490.3 -12719.1  25438.1    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.8073  0.0925  0.2172  0.4437  3.2564 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3351   0.5789  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.356467   0.131333  10.328  < 2e-16 ***
## gndr.c.gmc  -0.179841   0.031356  -5.735 9.73e-09 ***
## age10.c.gmc  0.304084   0.009397  32.361  < 2e-16 ***
## ben.c.gmc    0.129834   0.025076   5.178 2.25e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.048 -0.039       
## ben.c.gmc    0.002 -0.177 -0.043
```

```r
export(rownames_to_column(getFE_glmer(mod2.ben,p.round = 10,round = 10)),
       "../../results/ERQ3/ben_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.ben<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  ben.c.gmc+
                  (ben.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.ben)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + ben.c.gmc + (ben.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25430.4  25489.4 -12708.2  25416.4    33544 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -10.1474   0.0920   0.2171   0.4446   3.1669 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr
##  cntry  (Intercept) 0.33621  0.5798       
##         ben.c.gmc   0.01397  0.1182   0.06
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.357503   0.131528  10.321  < 2e-16 ***
## gndr.c.gmc  -0.186687   0.031417  -5.942 2.81e-09 ***
## age10.c.gmc  0.304594   0.009407  32.380  < 2e-16 ***
## ben.c.gmc    0.116689   0.050851   2.295   0.0217 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.048 -0.039       
## ben.c.gmc    0.038 -0.091 -0.029
```

```r
anova(mod2.ben,mod3.ben)
```

```
## Data: temp.fdat
## Models:
## mod2.ben: vote.DV ~ gndr.c.gmc + age10.c.gmc + ben.c.gmc + (1 | cntry)
## mod3.ben: vote.DV ~ gndr.c.gmc + age10.c.gmc + ben.c.gmc + (ben.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.ben    5 25448 25490 -12719    25438                         
## mod3.ben    7 25430 25489 -12708    25416 21.737  2  1.905e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## uni

### fixed effect model


```r
mod2.uni<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  uni.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.uni)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25386.6  25428.7 -12688.3  25376.6    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.6135  0.0905  0.2163  0.4436  3.2068 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3413   0.5842  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.362782   0.132491  10.286  < 2e-16 ***
## gndr.c.gmc  -0.180091   0.031073  -5.796  6.8e-09 ***
## age10.c.gmc  0.296729   0.009421  31.497  < 2e-16 ***
## uni.c.gmc    0.227214   0.024207   9.386  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.047 -0.036       
## uni.c.gmc    0.009 -0.101 -0.098
```

```r
export(rownames_to_column(getFE_glmer(mod2.uni,p.round = 10,round = 10)),
       "../../results/ERQ3/uni_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.uni<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  uni.c.gmc+
                  (uni.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.uni)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + (uni.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25384.9  25443.8 -12685.4  25370.9    33544 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.7874  0.0912  0.2169  0.4434  3.2201 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr
##  cntry  (Intercept) 0.343203 0.58583      
##         uni.c.gmc   0.008702 0.09328  0.30
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.360382   0.132855  10.240  < 2e-16 ***
## gndr.c.gmc  -0.179669   0.031133  -5.771 7.88e-09 ***
## age10.c.gmc  0.297811   0.009437  31.559  < 2e-16 ***
## uni.c.gmc    0.182376   0.047203   3.864 0.000112 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.003              
## age10.c.gmc  0.046 -0.035       
## uni.c.gmc    0.143 -0.059 -0.072
```

```r
anova(mod2.uni,mod3.uni)
```

```
## Data: temp.fdat
## Models:
## mod2.uni: vote.DV ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + (1 | cntry)
## mod3.uni: vote.DV ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + (uni.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.uni    5 25387 25429 -12688    25377                       
## mod3.uni    7 25385 25444 -12685    25371 5.7035  2    0.05774 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## sdi

### fixed effect model


```r
mod2.sdi<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  sdi.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.sdi)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + sdi.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25456.9  25499.0 -12723.4  25446.9    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.3404  0.0920  0.2170  0.4423  3.2101 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3389   0.5822  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.35824    0.13201  10.289  < 2e-16 ***
## gndr.c.gmc  -0.14816    0.03087  -4.800 1.59e-06 ***
## age10.c.gmc  0.30743    0.00939  32.740  < 2e-16 ***
## sdi.c.gmc    0.08326    0.01959   4.251 2.13e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.048 -0.046       
## sdi.c.gmc    0.005  0.024  0.028
```

```r
export(rownames_to_column(getFE_glmer(mod2.sdi,p.round = 10,round = 10)),
       "../../results/ERQ3/sdi_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.sdi<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  sdi.c.gmc+
                  (sdi.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.sdi)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + sdi.c.gmc + (sdi.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25444.4  25503.4 -12715.2  25430.4    33544 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.4136  0.0916  0.2169  0.4403  3.2331 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr 
##  cntry  (Intercept) 0.34036  0.5834        
##         sdi.c.gmc   0.01013  0.1006   -0.37
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.360019   0.132311  10.279  < 2e-16 ***
## gndr.c.gmc  -0.142107   0.030922  -4.596 4.31e-06 ***
## age10.c.gmc  0.310033   0.009441  32.839  < 2e-16 ***
## sdi.c.gmc    0.126606   0.042357   2.989   0.0028 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.001              
## age10.c.gmc  0.048 -0.044       
## sdi.c.gmc   -0.179  0.026  0.032
```

```r
anova(mod2.sdi,mod3.sdi)
```

```
## Data: temp.fdat
## Models:
## mod2.sdi: vote.DV ~ gndr.c.gmc + age10.c.gmc + sdi.c.gmc + (1 | cntry)
## mod3.sdi: vote.DV ~ gndr.c.gmc + age10.c.gmc + sdi.c.gmc + (sdi.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.sdi    5 25457 25499 -12723    25447                         
## mod3.sdi    7 25444 25503 -12715    25430 16.445  2  0.0002685 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## sti

### fixed effect model


```r
mod2.sti<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  sti.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.sti)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + sti.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25474.3  25516.4 -12732.2  25464.3    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9730  0.0923  0.2175  0.4446  3.2135 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.336    0.5796  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.356344   0.131504  10.314  < 2e-16 ***
## gndr.c.gmc  -0.153627   0.030989  -4.957 7.14e-07 ***
## age10.c.gmc  0.304444   0.009803  31.056  < 2e-16 ***
## sti.c.gmc   -0.012661   0.016634  -0.761    0.447    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.047 -0.018       
## sti.c.gmc    0.001  0.096  0.288
```

```r
export(rownames_to_column(getFE_glmer(mod2.sti,p.round = 10,round = 10)),
       "../../results/ERQ3/sti_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.sti<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  sti.c.gmc+
                  (sti.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.sti)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + sti.c.gmc + (sti.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25454.9  25513.9 -12720.5  25440.9    33544 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.8522  0.0925  0.2181  0.4439  3.1811 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr
##  cntry  (Intercept) 0.332254 0.57641      
##         sti.c.gmc   0.005692 0.07545  0.46
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.354339   0.130773  10.356  < 2e-16 ***
## gndr.c.gmc  -0.152709   0.031043  -4.919 8.69e-07 ***
## age10.c.gmc  0.306290   0.009822  31.183  < 2e-16 ***
## sti.c.gmc    0.026536   0.031098   0.853    0.393    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.001              
## age10.c.gmc  0.047 -0.018       
## sti.c.gmc    0.239  0.051  0.161
```

```r
anova(mod2.sti,mod3.sti)
```

```
## Data: temp.fdat
## Models:
## mod2.sti: vote.DV ~ gndr.c.gmc + age10.c.gmc + sti.c.gmc + (1 | cntry)
## mod3.sti: vote.DV ~ gndr.c.gmc + age10.c.gmc + sti.c.gmc + (sti.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.sti    5 25474 25516 -12732    25464                         
## mod3.sti    7 25455 25514 -12720    25441 23.399  2  8.297e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## hed

### fixed effect model


```r
mod2.hed<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  hed.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.hed)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + hed.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25473.2  25515.3 -12731.6  25463.2    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9646  0.0921  0.2176  0.4450  3.2068 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3363   0.5799  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.356442   0.131565  10.310  < 2e-16 ***
## gndr.c.gmc  -0.155528   0.031008  -5.016 5.28e-07 ***
## age10.c.gmc  0.303779   0.009624  31.563  < 2e-16 ***
## hed.c.gmc   -0.022518   0.017084  -1.318    0.187    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.047 -0.023       
## hed.c.gmc    0.000  0.102  0.220
```

```r
export(rownames_to_column(getFE_glmer(mod2.hed,p.round = 10,round = 10)),
       "../../results/ERQ3/hed_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.hed<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  hed.c.gmc+
                  (hed.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
summary(mod3.hed)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + hed.c.gmc + (hed.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25471.3  25530.2 -12728.6  25457.3    33544 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.8975  0.0929  0.2176  0.4444  3.2182 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr
##  cntry  (Intercept) 0.331715 0.57595      
##         hed.c.gmc   0.004128 0.06425  1.00
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.355104   0.130622  10.374  < 2e-16 ***
## gndr.c.gmc  -0.158383   0.031038  -5.103 3.34e-07 ***
## age10.c.gmc  0.303891   0.009624  31.576  < 2e-16 ***
## hed.c.gmc    0.001214   0.024402   0.050     0.96    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.048 -0.024       
## hed.c.gmc    0.587  0.058  0.158
## optimizer (bobyqa) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
anova(mod2.hed,mod3.hed)
```

```
## Data: temp.fdat
## Models:
## mod2.hed: vote.DV ~ gndr.c.gmc + age10.c.gmc + hed.c.gmc + (1 | cntry)
## mod3.hed: vote.DV ~ gndr.c.gmc + age10.c.gmc + hed.c.gmc + (hed.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
## mod2.hed    5 25473 25515 -12732    25463                       
## mod3.hed    7 25471 25530 -12729    25457 5.8748  2      0.053 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## ach

### fixed effect model


```r
mod2.ach<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  ach.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.ach)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + ach.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25462.8  25504.9 -12726.4  25452.8    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.6711  0.0922  0.2172  0.4447  3.2925 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3359   0.5795  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.356301   0.131465  10.317  < 2e-16 ***
## gndr.c.gmc  -0.159846   0.030948  -5.165 2.41e-07 ***
## age10.c.gmc  0.298722   0.009643  30.977  < 2e-16 ***
## ach.c.gmc   -0.059139   0.017003  -3.478 0.000505 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.047 -0.026       
## ach.c.gmc   -0.001  0.079  0.229
```

```r
export(rownames_to_column(getFE_glmer(mod2.ach,p.round = 10,round = 10)),
       "../../results/ERQ3/ach_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.ach<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  ach.c.gmc+
                  (ach.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.ach)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + ach.c.gmc + (ach.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25444.0  25502.9 -12715.0  25430.0    33544 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9240  0.0933  0.2185  0.4431  3.4949 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr
##  cntry  (Intercept) 0.326422 0.57133      
##         ach.c.gmc   0.007185 0.08476  0.66
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.3552248  0.1296861  10.450  < 2e-16 ***
## gndr.c.gmc  -0.1580401  0.0309920  -5.099 3.41e-07 ***
## age10.c.gmc  0.3010313  0.0096857  31.080  < 2e-16 ***
## ach.c.gmc    0.0000916  0.0342369   0.003    0.998    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.047 -0.025       
## ach.c.gmc    0.355  0.049  0.140
```

```r
anova(mod2.ach,mod3.ach)
```

```
## Data: temp.fdat
## Models:
## mod2.ach: vote.DV ~ gndr.c.gmc + age10.c.gmc + ach.c.gmc + (1 | cntry)
## mod3.ach: vote.DV ~ gndr.c.gmc + age10.c.gmc + ach.c.gmc + (ach.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.ach    5 25463 25505 -12726    25453                         
## mod3.ach    7 25444 25503 -12715    25430 22.821  2  1.108e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## pow

### fixed effect model


```r
mod2.pow<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  pow.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.pow)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + pow.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25457.4  25499.5 -12723.7  25447.4    33546 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.1076  0.0914  0.2174  0.4445  3.2168 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3364   0.58    
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.357618   0.131584  10.318  < 2e-16 ***
## gndr.c.gmc  -0.169039   0.031149  -5.427 5.74e-08 ***
## age10.c.gmc  0.304674   0.009395  32.431  < 2e-16 ***
## pow.c.gmc   -0.076140   0.018183  -4.187 2.82e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.048 -0.042       
## pow.c.gmc   -0.004  0.136  0.043
```

```r
export(rownames_to_column(getFE_glmer(mod2.pow,p.round = 10,round = 10)),
       "../../results/ERQ3/pow_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.pow<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  pow.c.gmc+
                  (pow.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.pow)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + pow.c.gmc + (pow.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25436.2  25495.2 -12711.1  25422.2    33544 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.1765  0.0923  0.2178  0.4437  3.2329 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr
##  cntry  (Intercept) 0.32552  0.5705       
##         pow.c.gmc   0.01209  0.1100   0.89
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.35732    0.12950  10.482  < 2e-16 ***
## gndr.c.gmc  -0.16837    0.03118  -5.399 6.69e-08 ***
## age10.c.gmc  0.30660    0.00943  32.513  < 2e-16 ***
## pow.c.gmc   -0.04509    0.03699  -1.219    0.223    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc  -0.002              
## age10.c.gmc  0.050 -0.041       
## pow.c.gmc    0.579  0.066  0.024
```

```r
anova(mod2.pow,mod3.pow)
```

```
## Data: temp.fdat
## Models:
## mod2.pow: vote.DV ~ gndr.c.gmc + age10.c.gmc + pow.c.gmc + (1 | cntry)
## mod3.pow: vote.DV ~ gndr.c.gmc + age10.c.gmc + pow.c.gmc + (pow.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.pow    5 25457 25500 -12724    25447                         
## mod3.pow    7 25436 25495 -12711    25422 25.147  2  3.463e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## sec

### fixed effect model


```r
mod2.sec<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  sec.c.gmc+
                  (1|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod2.sec)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: vote.DV ~ gndr.c.gmc + age10.c.gmc + sec.c.gmc + (1 | cntry)
##    Data: temp.fdat
## Weights: anweight
## 
##      AIC      BIC   logLik deviance df.resid 
##  25420.8  25462.9 -12705.4  25410.8    33546 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -10.2525   0.0913   0.2172   0.4427   3.3286 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  cntry  (Intercept) 0.3418   0.5847  
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.362233   0.132547  10.277  < 2e-16 ***
## gndr.c.gmc  -0.119207   0.031186  -3.822 0.000132 ***
## age10.c.gmc  0.320812   0.009619  33.351  < 2e-16 ***
## sec.c.gmc   -0.141701   0.019378  -7.312 2.62e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc   0.000              
## age10.c.gmc  0.049 -0.016       
## sec.c.gmc   -0.009 -0.139 -0.212
```

```r
export(rownames_to_column(getFE_glmer(mod2.sec,p.round = 10,round = 10)),
       "../../results/ERQ3/sec_FE.xlsx",overwrite=T)
```

### random effect model


```r
mod3.sec<-glmer(vote.DV~gndr.c.gmc+age10.c.gmc+
                  sec.c.gmc+
                  (sec.c.gmc|cntry),
           weights = anweight,data=temp.fdat,
           family = binomial(link="logit"),
           control = glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e6)))
```

```
## Warning in eval(family$initialize, rho): non-integer #successes in a
## binomial glm!
```

```r
summary(mod3.sec)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## vote.DV ~ gndr.c.gmc + age10.c.gmc + sec.c.gmc + (sec.c.gmc |  
##     cntry)
##    Data: temp.fdat
## Weights: anweight
## Control: 
## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06))
## 
##      AIC      BIC   logLik deviance df.resid 
##  25398.9  25457.8 -12692.4  25384.9    33544 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -11.6110   0.0908   0.2167   0.4405   3.3235 
## 
## Random effects:
##  Groups Name        Variance Std.Dev. Corr 
##  cntry  (Intercept) 0.345749 0.58800       
##         sec.c.gmc   0.008972 0.09472  -0.26
## Number of obs: 33551, groups:  cntry, 21
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.365180   0.133275  10.243  < 2e-16 ***
## gndr.c.gmc  -0.121479   0.031222  -3.891 9.99e-05 ***
## age10.c.gmc  0.323700   0.009653  33.534  < 2e-16 ***
## sec.c.gmc   -0.161873   0.039354  -4.113 3.90e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) gndr.. ag10..
## gndr.c.gmc   0.000              
## age10.c.gmc  0.049 -0.017       
## sec.c.gmc   -0.147 -0.064 -0.109
```

```r
anova(mod2.sec,mod3.sec)
```

```
## Data: temp.fdat
## Models:
## mod2.sec: vote.DV ~ gndr.c.gmc + age10.c.gmc + sec.c.gmc + (1 | cntry)
## mod3.sec: vote.DV ~ gndr.c.gmc + age10.c.gmc + sec.c.gmc + (sec.c.gmc | cntry)
##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
## mod2.sec    5 25421 25463 -12705    25411                         
## mod3.sec    7 25399 25458 -12692    25385 25.895  2  2.383e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
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
##  [1] tibble_3.1.8          vjihelpers_0.0.0.9000 dplyr_1.0.9          
##  [4] rio_0.5.29            r2mlm_0.3.1           nlme_3.1-157         
##  [7] lmerTest_3.1-3        lme4_1.1-30           Matrix_1.4-1         
## [10] knitr_1.39            rmarkdown_2.15       
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
## [25] scales_1.2.0        openxlsx_4.2.5      emmeans_1.8.0      
## [28] rockchalk_1.8.151   generics_0.1.3      ggplot2_3.3.5      
## [31] ellipsis_0.3.2      broomExtra_4.3.2    cli_3.3.0          
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
## [67] Rcpp_1.0.9          vctrs_0.4.1         tidyselect_1.1.2   
## [70] xfun_0.30           coda_0.19-4
```
