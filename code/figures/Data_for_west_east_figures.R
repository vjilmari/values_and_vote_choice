# Use the posterior mode slopes for plotting
# rerun the selected models
# confidence intervals for posterior modes through
# https://stackoverflow.com/questions/69805532/extract-the-confidence-intervals-of-lmer-random-effects-plotted-with-dotplotra

library(lme4)
library(lmerTest)
library(emmeans)
library(r2mlm)
library(rio)
library(dplyr)
library(vjihelpers)
library(psych)
library(tibble)
#library(parameters)
#library(ggplot2)
#library(MetBrewer)
#library(finalfit)
#library(ggpubr)

source("code/custom_functions.R")




fdat<-import("data/processed/fdat.xlsx")


value.vars<-
  c("con","tra","ben","uni","sdi",
    "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]


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

# lrgen

lrgen.fdat<- fdat %>% 
  dplyr::select(all_of(value.vars),
                all_of(value.vars.c),
                lrgen.z,
                West_vs_post_comm,
                "gndr.c",
                "age10.c",
                "cntry",
                "anweight") %>%
  na.omit()

# center all IVs within country

lrgen.fdat<-
  group_mean_center(data=lrgen.fdat,group.var = "cntry",
                    vars = c(value.vars,
                             value.vars.c,
                             "gndr.c",
                             "age10.c"))

# uni

## fit FE model
uni.lrgen.mod2<-
  lmer(lrgen.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (1 | cntry),
       weights = anweight,data=lrgen.fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))


## fit RE model
uni.lrgen.mod3<-
  lmer(lrgen.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (uni.c.gmc | cntry),
           weights = anweight,data=lrgen.fdat,
           control = lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=2e6)))

anova(uni.lrgen.mod2,uni.lrgen.mod3)
summary(uni.lrgen.mod3)



uni.lrgen.mod3.ranefs<-
  as.data.frame(ranef(uni.lrgen.mod3,condVar=T))

uni.lrgen.mod3.ranefs<-
  uni.lrgen.mod3.ranefs[uni.lrgen.mod3.ranefs$term=="uni.c.gmc",]

uni.lrgen.mod3.ranefs$Slope<-
  fixef(uni.lrgen.mod3)["uni.c.gmc"]+
  uni.lrgen.mod3.ranefs$condval

uni.lrgen.mod3.ranefs$LL<-
  fixef(uni.lrgen.mod3)["uni.c.gmc"]+
  uni.lrgen.mod3.ranefs$condval+
  qnorm(.025)*uni.lrgen.mod3.ranefs$condsd

uni.lrgen.mod3.ranefs$UL<-
  fixef(uni.lrgen.mod3)["uni.c.gmc"]+
  uni.lrgen.mod3.ranefs$condval+
  qnorm(.975)*uni.lrgen.mod3.ranefs$condsd

export(uni.lrgen.mod3.ranefs,overwrite=T,
       "results/figures/figdata/uni.lrgen.mod3.ranefs.xlsx")





# lrecon

lrecon.fdat<- fdat %>% 
  dplyr::select(all_of(value.vars),
                all_of(value.vars.c),
                lrecon.z,
                West_vs_post_comm,
                "gndr.c",
                "age10.c",
                "cntry",
                "anweight") %>%
  na.omit()

# center all IVs within country

lrecon.fdat<-
  group_mean_center(data=lrecon.fdat,group.var = "cntry",
                    vars = c(value.vars,
                             value.vars.c,
                             "gndr.c",
                             "age10.c"))

# uni

## fit FE model
uni.lrecon.mod2<-
  lmer(lrecon.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (1 | cntry),
       weights = anweight,data=lrecon.fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))


## fit RE model
uni.lrecon.mod3<-
  lmer(lrecon.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (uni.c.gmc | cntry),
       weights = anweight,data=lrecon.fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))

anova(uni.lrecon.mod2,uni.lrecon.mod3)
summary(uni.lrecon.mod3)

uni.lrecon.mod3.ranefs<-
  as.data.frame(ranef(uni.lrecon.mod3,condVar=T))

uni.lrecon.mod3.ranefs<-
  uni.lrecon.mod3.ranefs[uni.lrecon.mod3.ranefs$term=="uni.c.gmc",]

uni.lrecon.mod3.ranefs$Slope<-
  fixef(uni.lrecon.mod3)["uni.c.gmc"]+
  uni.lrecon.mod3.ranefs$condval

uni.lrecon.mod3.ranefs$LL<-
  fixef(uni.lrecon.mod3)["uni.c.gmc"]+
  uni.lrecon.mod3.ranefs$condval+
  qnorm(.025)*uni.lrecon.mod3.ranefs$condsd

uni.lrecon.mod3.ranefs$UL<-
  fixef(uni.lrecon.mod3)["uni.c.gmc"]+
  uni.lrecon.mod3.ranefs$condval+
  qnorm(.975)*uni.lrecon.mod3.ranefs$condsd

export(uni.lrecon.mod3.ranefs,overwrite=T,
       "results/figures/figdata/uni.lrecon.mod3.ranefs.xlsx")




# galtan

galtan.fdat<- fdat %>% 
  dplyr::select(all_of(value.vars),
                all_of(value.vars.c),
                galtan.z,
                West_vs_post_comm,
                "gndr.c",
                "age10.c",
                "cntry",
                "anweight") %>%
  na.omit()

# center all IVs within country

galtan.fdat<-
  group_mean_center(data=galtan.fdat,group.var = "cntry",
                    vars = c(value.vars,
                             value.vars.c,
                             "gndr.c",
                             "age10.c"))

# uni

## fit FE model
uni.galtan.mod2<-
  lmer(galtan.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (1 | cntry),
       weights = anweight,data=galtan.fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))


## fit RE model
uni.galtan.mod3<-
  lmer(galtan.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (uni.c.gmc | cntry),
       weights = anweight,data=galtan.fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))

anova(uni.galtan.mod2,uni.galtan.mod3)
summary(uni.galtan.mod3)

uni.galtan.mod3.ranefs<-
  as.data.frame(ranef(uni.galtan.mod3,condVar=T))

uni.galtan.mod3.ranefs<-
  uni.galtan.mod3.ranefs[uni.galtan.mod3.ranefs$term=="uni.c.gmc",]

uni.galtan.mod3.ranefs$Slope<-
  fixef(uni.galtan.mod3)["uni.c.gmc"]+
  uni.galtan.mod3.ranefs$condval

uni.galtan.mod3.ranefs$LL<-
  fixef(uni.galtan.mod3)["uni.c.gmc"]+
  uni.galtan.mod3.ranefs$condval+
  qnorm(.025)*uni.galtan.mod3.ranefs$condsd

uni.galtan.mod3.ranefs$UL<-
  fixef(uni.galtan.mod3)["uni.c.gmc"]+
  uni.galtan.mod3.ranefs$condval+
  qnorm(.975)*uni.galtan.mod3.ranefs$condsd

export(uni.galtan.mod3.ranefs,overwrite=T,
       "results/figures/figdata/uni.galtan.mod3.ranefs.xlsx")




# antielite_salience

antielite_salience.fdat<- fdat %>% 
  dplyr::select(all_of(value.vars),
                all_of(value.vars.c),
                antielite_salience.z,
                West_vs_post_comm,
                "gndr.c",
                "age10.c",
                "cntry",
                "anweight") %>%
  na.omit()

# center all IVs within country

antielite_salience.fdat<-
  group_mean_center(data=antielite_salience.fdat,group.var = "cntry",
                    vars = c(value.vars,
                             value.vars.c,
                             "gndr.c",
                             "age10.c"))

# uni

## fit FE model
uni.antielite_salience.mod2<-
  lmer(antielite_salience.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (1 | cntry),
       weights = anweight,data=antielite_salience.fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))


## fit RE model
uni.antielite_salience.mod3<-
  lmer(antielite_salience.z ~ gndr.c.gmc + age10.c.gmc + uni.c.gmc + 
         (uni.c.gmc | cntry),
       weights = anweight,data=antielite_salience.fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))

anova(uni.antielite_salience.mod2,uni.antielite_salience.mod3)
summary(uni.antielite_salience.mod3)

uni.antielite_salience.mod3.ranefs<-
  as.data.frame(ranef(uni.antielite_salience.mod3,condVar=T))

uni.antielite_salience.mod3.ranefs<-
  uni.antielite_salience.mod3.ranefs[uni.antielite_salience.mod3.ranefs$term=="uni.c.gmc",]

uni.antielite_salience.mod3.ranefs$Slope<-
  fixef(uni.antielite_salience.mod3)["uni.c.gmc"]+
  uni.antielite_salience.mod3.ranefs$condval

uni.antielite_salience.mod3.ranefs$LL<-
  fixef(uni.antielite_salience.mod3)["uni.c.gmc"]+
  uni.antielite_salience.mod3.ranefs$condval+
  qnorm(.025)*uni.antielite_salience.mod3.ranefs$condsd

uni.antielite_salience.mod3.ranefs$UL<-
  fixef(uni.antielite_salience.mod3)["uni.c.gmc"]+
  uni.antielite_salience.mod3.ranefs$condval+
  qnorm(.975)*uni.antielite_salience.mod3.ranefs$condsd

export(uni.antielite_salience.mod3.ranefs,overwrite=T,
       "results/figures/figdata/uni.antielite_salience.mod3.ranefs.xlsx")



