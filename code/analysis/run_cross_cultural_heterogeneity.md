---
title: "Cross-cultural heterogeneity"
output: 
  html_document: 
    toc: yes
    keep_md: yes
---



# Preparations

## Packages


```r
library(devtools)
```

```
## Loading required package: usethis
```

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
library(r2mlm)
```

```
## Warning: package 'r2mlm' was built under R version 4.3.1
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
## 
## Please cite as:
```

```
##  Shaw, M., Rights, J.D., Sterba, S.S., Flake, J.K. (2023). r2mlm: An R package calculating R-squared measures for multilevel models. Behavior Research Methods, 55(4), 1942-1964. doi:10.3758/s13428-022-01841-4
```

```r
library(rio)
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
library(tibble)
library(ggplot2)
library(ggflags)
library(MetBrewer)
library(finalfit)
library(performance)
```

```
## Warning: package 'performance' was built under R version 4.3.1
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


DV.names<-
  c("Left-Right General",
    "Left-Right Economic",
    "GAL-TAN",
    "Public services vs. reducing taxes",
    "Deregulation of markets",
    "Redistribution of wealth",
    "State intervention in the economy",
    "Civil liberties vs. law and order",
    "Social lifestyle policies",
    "Religious principles",
    "Immigration policy",
    "Multiculturalism vs. assimilation",
    "Urban vs. rural interests",
    "Environmental protection vs. economic growth",
    "Decentralization to regions/localities",
    "International troop deployment",
    "Rights of ethnic minorities",
    "Cosmopolitanism vs. nationalism",
    "Left-Right Economic salience",
    "GAL-TAN salience",
    "Anti-elite",
    "Reducing corruption")


Value.names<-
  c("Conformity",
    "Tradition",
    "Benevolence",
    "Universalism",
    "Self-direction",
    "Stimulation",
    "Hedonism",
    "Achievement",
    "Power",
    "Security")
```

# Analysis call


```r
length(DV.vars)
```

```
## [1] 22
```

```r
t1<-Sys.time()

pdf("../../results/figures/Country_specific_figures.pdf",
    onefile=T,width=11.7,height=8.3)

for (k in 1:length(DV.vars)){
  Heterogeneity_plot_pipe(DV=DV.vars[k],
                  IV=value.vars,
                  data=fdat,
                  IV.c=value.vars.c,
                  directory="../../results/main")
  
}
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

```r
t2<-Sys.time()

dev.off()
```

```
## png 
##   2
```

```r
t2-t1
```

```
## Time difference of 14.88677 mins
```

# Session info


```r
sinf<-sessionInfo()
print(sinf,locale=F)
```

```
## R version 4.3.0 (2023-04-21 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19045)
## 
## Matrix products: default
## 
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] performance_0.10.4    finalfit_1.0.6        MetBrewer_0.2.0      
##  [4] ggflags_0.0.2         ggplot2_3.4.2         tibble_3.2.1         
##  [7] vjihelpers_0.0.0.9000 dplyr_1.1.2           rio_0.5.29           
## [10] r2mlm_0.3.5           nlme_3.1-162          lmerTest_3.1-3       
## [13] lme4_1.1-33           Matrix_1.5-4          devtools_2.4.5       
## [16] usethis_2.1.6        
## 
## loaded via a namespace (and not attached):
##  [1] remotes_2.4.2       readxl_1.4.2        rlang_1.1.1        
##  [4] magrittr_2.0.3      rockchalk_1.8.157   compiler_4.3.0     
##  [7] png_0.1-8           callr_3.7.3         vctrs_0.6.2        
## [10] stringr_1.5.0       profvis_0.3.8       pkgconfig_2.0.3    
## [13] crayon_1.5.2        fastmap_1.1.1       backports_1.4.1    
## [16] ellipsis_0.3.2      labeling_0.4.2      utf8_1.2.3         
## [19] promises_1.2.0.1    rmarkdown_2.21      sessioninfo_1.2.2  
## [22] haven_2.5.2         ps_1.7.5            nloptr_2.0.3       
## [25] purrr_1.0.1         xfun_0.39           cachem_1.0.8       
## [28] kutils_1.70         jsonlite_1.8.7      later_1.3.1        
## [31] jpeg_0.1-10         broom_1.0.4         prettyunits_1.1.1  
## [34] R6_2.5.1            bslib_0.4.2         stringi_1.7.12     
## [37] boot_1.3-28.1       pkgload_1.3.2       jquerylib_0.1.4    
## [40] cellranger_1.1.0    numDeriv_2016.8-1.1 Rcpp_1.0.10        
## [43] knitr_1.42          base64enc_0.1-3     httpuv_1.6.11      
## [46] splines_4.3.0       tidyselect_1.2.0    rstudioapi_0.14    
## [49] yaml_2.3.7          miniUI_0.1.1.1      curl_5.0.2         
## [52] processx_3.8.1      pkgbuild_1.4.0      lattice_0.21-8     
## [55] plyr_1.8.8          shiny_1.7.5         withr_2.5.0        
## [58] evaluate_0.21       foreign_0.8-84      survival_3.5-5     
## [61] urlchecker_1.0.1    zip_2.3.0           pillar_1.9.0       
## [64] carData_3.0-5       mice_3.15.0         insight_0.19.2     
## [67] generics_0.1.3      grImport2_0.2-0     hms_1.1.3          
## [70] munsell_0.5.0       scales_1.2.1        minqa_1.2.5        
## [73] xtable_1.8-4        glue_1.6.2          tools_4.3.0        
## [76] data.table_1.14.8   openxlsx_4.2.5.2    forcats_1.0.0      
## [79] fs_1.6.2            XML_3.99-0.14       grid_4.3.0         
## [82] tidyr_1.3.0         colorspace_2.1-0    cli_3.6.1          
## [85] fansi_1.0.4         gtable_0.3.3        sass_0.4.6         
## [88] digest_0.6.31       farver_2.1.1        htmlwidgets_1.6.2  
## [91] memoise_2.0.1       htmltools_0.5.5     lifecycle_1.0.3    
## [94] mime_0.12           MASS_7.3-58.4
```
