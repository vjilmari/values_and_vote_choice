#' ---
#' title: "RQ4. GAL-TAN Saliency Moderation"
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#' date: '`r Sys.Date()`'
#' ---
#' 
## ----setup, include=FALSE----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' RQ4. If, as expected (H3), some personal values are associated with the GAL-TAN position of the party for which one has voted, are those associations stronger if this dimension is salient in the party’s public stance (i.e., is there a boundary condition to these associations, such that the ideology needs to be salient in order for the associations to exist)?
#' H6. We expect the associations between personal values and GAL-TAN to be stronger if this dimension is salient in the party’s public stance.
#' 
#' H3 confirmed association for conformity, tradition, and security. (Not confirmed for self-direction)
#' 
#' 
#' # Preparations
#' 
#' ## Packages
#' 
## ----------------------------------------------------------------

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
## ----------------------------------------------------------------

source("../custom_functions.R")

#' 
#' ## Data
#' 
## ----------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")

#' 
#' ## Variables
#' 
## ----------------------------------------------------------------

value.vars<-
  c("con","tra","ben","uni","sdi",
        "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]


#' 
#' ## Data filtering and centering predictors
#' 
## ----------------------------------------------------------------

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

#' 
#' # Analysis
#' 
#' ## con
#' 
#' ### fixed effect model
#' 
## ----------------------------------------------------------------
mod2.con<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+con.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.con)

#' 
#' ### salience main effect
#' 
## ----------------------------------------------------------------
mod3.con<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+con.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.con)

#' 
#' ### salience moderation
#' 
## ----------------------------------------------------------------
mod4.con<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+con.c.gmc+
                 galtan_salience.z.gmc+
             con.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.con)

emtrends(mod4.con,
         var="con.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))



#' 
#' 
#' ### figure data
#' 
## ----------------------------------------------------------------
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

export(p.con,overwrite=T,
       "../../results/figures/figdata/con_GALTAN_salience.xlsx")


#' 
#' ## tra
#' 
#' ### fixed effect model
#' 
## ----------------------------------------------------------------
mod2.tra<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+tra.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.tra)

#' 
#' ### salience main effect
#' 
## ----------------------------------------------------------------
mod3.tra<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+tra.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.tra)

#' 
#' ### salience moderation
#' 
## ----------------------------------------------------------------
mod4.tra<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+tra.c.gmc+
                 galtan_salience.z.gmc+
             tra.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.tra)

emtrends(mod4.tra,
         var="tra.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))



#' 
#' ### figure data
#' 
## ----------------------------------------------------------------
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

export(p.tra,overwrite=T,
       "../../results/figures/figdata/tra_GALTAN_salience.xlsx")


#' 
#' ## sec
#' 
#' ### fixed effect model
#' 
## ----------------------------------------------------------------
mod2.sec<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sec.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.sec)

#' 
#' ### salience main effect
#' 
## ----------------------------------------------------------------
mod3.sec<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sec.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.sec)

#' 
#' ### salience moderation
#' 
## ----------------------------------------------------------------
mod4.sec<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sec.c.gmc+
                 galtan_salience.z.gmc+
             sec.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.sec)

emtrends(mod4.sec,
         var="sec.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))


#' 
#' ### figure data
#' 
## ----------------------------------------------------------------
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

export(p.sec,overwrite=T,
       "../../results/figures/figdata/sec_GALTAN_salience.xlsx")


#' 
#' ## sdi
#' 
#' ### fixed effect model
#' 
## ----------------------------------------------------------------
mod2.sdi<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sdi.c.gmc+(1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod2.sdi)

#' 
#' ### salience main effect
#' 
## ----------------------------------------------------------------
mod3.sdi<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sdi.c.gmc+
                 galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod3.sdi)

#' 
#' ### salience moderation
#' 
## ----------------------------------------------------------------
mod4.sdi<-lmer(galtan.z~gndr.c.gmc+age10.c.gmc+sdi.c.gmc+
                 galtan_salience.z.gmc+
             sdi.c.gmc:galtan_salience.z.gmc+
             (1|cntry),
           weights = anweight,data=temp.fdat)
getFE(mod4.sdi)

emtrends(mod4.sdi,
         var="sdi.c.gmc",
         specs="galtan_salience.z.gmc",
         at=list(galtan_salience.z.gmc=c(-1,0,1)),
         disable.pbkrtest=T,
         lmerTest.limit = 25000,
         infer=c(T,T))


#' 
#' ### figure data
#' 
## ----------------------------------------------------------------
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

export(p.sdi,overwrite=T,
       "../../results/figures/figdata/sdi_GALTAN_salience.xlsx")


#' 
#' 
#' # Session info
#' 
## ----------------------------------------------------------------
sinf<-sessionInfo()
print(sinf,locale=F)

