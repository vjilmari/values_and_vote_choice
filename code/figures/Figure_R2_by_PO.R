# Figure for variance explained by all values for each PO

source("code/custom_functions.R")
library(rio)
library(ggplot2)
library(MetBrewer)
library(finalfit)
library(ggpubr)
library(dplyr)

met.brewer("OKeeffe1")
met.brewer("Demuth")

value.vars<-
  c("con","tra","ben","uni","sdi",
    "sti","hed","ach","pow","sec")

value.names<-
  c("Conformity",
    "Tradition",
    "Benevolence",
    "Universalism",
    "Self-Direction",
    "Stimulation",
    "Hedonism",
    "Achievement",
    "Power",
    "Security")

value.vars.c<-paste0(value.vars,".c")
value.vars.c.gmc<-paste0(value.vars,".c.gmc")


# read data

d<-import("results/figures/figdata/R2longtab.xlsx")


# reorder the samples

table(d$Sample)
d$Sample<-factor(d$Sample,
                 levels = c("All", "College", "No college","Western Europe",
                            "Post-communist"))

# make a reversed version of the sample variable

d$Sample.rev<-factor(d$Sample,
                 levels = rev(c("All", "College", "No college","Western Europe",
                            "Post-communist")))

# make long d where covariates and values are stacked

head(d)

long.d<-data.frame(
  Sample=rep(d$Sample,times=2),
  Sample.rev=rep(d$Sample.rev,times=2),
  Variable=rep(d$Variable,times=2),
  Type=rep(c("Covariates","Values"),each=nrow(d)),
  R2=c(d$COVR2,d$VALUER2))

long.d$Type<-factor(long.d$Type,
                    levels=c("Values","Covariates"))

# exclude all but the main variables
table(d$Variable)
d.main<-
  d %>% filter(Variable=="Left-Right General" |
                 Variable=="Left-Right Economic" |
                 Variable=="GAL-TAN")

long.d.main<-
  long.d %>% filter(Variable=="Left-Right General" |
                 Variable=="Left-Right Economic" |
                 Variable=="GAL-TAN") 
str(long.d.main)

# order the Variables correctly

long.d.main$Variable<-factor(long.d.main$Variable,
                    levels=c("Left-Right General","Left-Right Economic","GAL-TAN"))

# try bar/col plot for values only

R2plot.main<-
  ggplot(long.d.main,
       aes(x=R2,y=Sample.rev,fill=Type))+
  geom_col(position=position_dodge2())+
  scale_fill_manual(values=met.brewer("Demuth")[c(7,4)])+
  #scale_alpha_manual(values = c(1,0.5))+
  xlab(paste0("\u0394","R\u00b2"))+
  ylab("")+
  theme(legend.position="top",
        legend.title=element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        #axis.text.y = element_text(angle = 45),
        text = element_text(size = 12),
        strip.text = element_text(
          size = 12, color = "black", face = "bold"
        ),
        strip.background = element_blank())+
  scale_x_continuous(limits=c(0,0.15),breaks=seq(0,.15,.05))+
  facet_wrap(~Variable,ncol=1)
R2plot.main

png("results/figures/R2_main_vars_plot.png",
    width = 21/2,height=(29.7*(3/4))/1,units = "cm",res = 600)
R2plot.main
dev.off()


# make even larger plot including all but salience moderators
table(long.d$Variable)

long.d.other<-
  long.d %>% filter(Variable!="Left-Right Economic salience" &
                      Variable!="GAL-TAN salience") 

table(long.d.other$Sample)
table(long.d.other$Sample.rev)

long.d.other[long.d.other$Variable=="Immigration policy",]



R2plot.other<-
  ggplot(long.d.other,
         aes(x=R2,y=Sample.rev,fill=Type))+
  geom_col()+
  scale_fill_manual(values=met.brewer("Demuth")[c(7,4)])+
  #scale_alpha_manual(values = c(1,0.5))+
  xlab(paste0("\u0394","R\u00b2"))+
  ylab("")+
  geom_text(color=met.brewer("Demuth")[7],
            size=4,aes(x=0.185,
                       label=ifelse(Type=="Values",
                                    substr(as.character(round_tidy(R2,2)),2,4),
                                    "")))+
  geom_text(color=met.brewer("Demuth")[4],
            size=4,aes(x=0.170,
                       label=ifelse(Type=="Covariates",
                                    substr(as.character(round_tidy(R2,2)),2,4),
                                    "")))+
  theme(legend.position="top",
        legend.title=element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        #axis.text.y = element_text(angle = 45),
        text = element_text(size = 12),
        strip.text = element_text(hjust = 0,
          size = 12, color = "black", face = "bold"
        ),
        strip.background = element_blank())+
  scale_x_continuous(limits=c(0,0.20),breaks=seq(0,.15,.05))+
  facet_wrap(~Variable,ncol=2)
R2plot.other

png("results/figures/R2_all_vars_plot.png",
    width = 21*1.25,height=(21*1.25*(1/(21.0/29.7)))*(3/4),units = "cm",res = 600)
R2plot.other
dev.off()


