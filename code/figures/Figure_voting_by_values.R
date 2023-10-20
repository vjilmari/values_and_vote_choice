# Prediction figures for all CHES variables

source("code/custom_functions.R")
library(rio)
library(ggplot2)
library(MetBrewer)
library(finalfit)
library(ggpubr)

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

# helper to match variable with longer names
DV.vars<-
  data.frame(DV.var.name=
               c("lrgen.z",
                 "lrecon.z",
                 "galtan.z",
                 "spendvtax.z",
                 "deregulation.z",
                 "redistribution.z",
                 "econ_interven.z",
                 "civlib_laworder.z",
                 "sociallifestyle.z",
                 "religious_principle.z",
                 "immigrate_policy.z",
                 "multiculturalism.z",
                 "urban_rural.z",
                 "environment.z",
                 "regions.z",
                 "international_security.z",
                 "ethnic_minorities.z",
                 "nationalism.z",
                 "antielite_salience.z",
                 "corrupt_salience.z"),
             DV.name=c(
               "Left-Right General",
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
               "Anti-elite",
               "Anti-corruption"
             ))

DV.vars



# lrgen.z
getwd()
lrgen.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  lrgen.z.list[[i]]<-
    for.main.figures(DV="lrgen.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


lrgen.z.df<-do.call(rbind,lrgen.z.list)
#lrgen.z.df$value<-substr(lrgen.z.df$rowname,1,3)
lrgen.z.df$value<-value.names
lrgen.z.df$r2<-round_tidy(lrgen.z.df$d.r2,2)
lrgen.z.df$r2.total<-round_tidy(lrgen.z.df$d.r2.multi,2)

lrgen.z.plot.df<-
  data.frame(value=rep(lrgen.z.df$value,times=2),
             b=c(lrgen.z.df$Est.,lrgen.z.df$Est..mv),
             LL=c(lrgen.z.df$LL,lrgen.z.df$LL.mv),
             UL=c(lrgen.z.df$UL,lrgen.z.df$UL.mv),
             r2=rep(lrgen.z.df$r2,2),
             r2.total=rep(lrgen.z.df$r2.total,2))



lrgen.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
lrgen.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

lrgen.plot<-
  ggplot(lrgen.z.plot.df,aes(x=b,
                           y=value.loc,
                           group=value,
                           color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
               color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(lrgen.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("")+
  ggtitle("A) Left-Right")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_lrgen.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
lrgen.plot
dev.off()


# lrecon.z

lrecon.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  lrecon.z.list[[i]]<-
    for.main.figures(DV="lrecon.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


lrecon.z.df<-do.call(rbind,lrecon.z.list)
#lrecon.z.df$value<-substr(lrecon.z.df$rowname,1,3)
lrecon.z.df$value<-value.names
lrecon.z.df$r2<-round_tidy(lrecon.z.df$d.r2,2)
lrecon.z.df$r2.total<-round_tidy(lrecon.z.df$d.r2.multi,2)

lrecon.z.plot.df<-
  data.frame(value=rep(lrecon.z.df$value,times=2),
             b=c(lrecon.z.df$Est.,lrecon.z.df$Est..mv),
             LL=c(lrecon.z.df$LL,lrecon.z.df$LL.mv),
             UL=c(lrecon.z.df$UL,lrecon.z.df$UL.mv),
             r2=rep(lrecon.z.df$r2,2),
             r2.total=rep(lrecon.z.df$r2.total,2))



lrecon.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
lrecon.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

lrecon.plot<-
  ggplot(lrecon.z.plot.df,aes(x=b,
                             y=value.loc,
                             group=value,
                             color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(lrecon.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("")+
  ggtitle("B) Economic Left-Right")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_lrecon.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
lrecon.plot
dev.off()






# galtan.z

galtan.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  galtan.z.list[[i]]<-
    for.main.figures(DV="galtan.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


galtan.z.df<-do.call(rbind,galtan.z.list)
#galtan.z.df$value<-substr(galtan.z.df$rowname,1,3)
galtan.z.df$value<-value.names
galtan.z.df$r2<-round_tidy(galtan.z.df$d.r2,2)
galtan.z.df$r2.total<-round_tidy(galtan.z.df$d.r2.multi,2)

galtan.z.plot.df<-
  data.frame(value=rep(galtan.z.df$value,times=2),
             b=c(galtan.z.df$Est.,galtan.z.df$Est..mv),
             LL=c(galtan.z.df$LL,galtan.z.df$LL.mv),
             UL=c(galtan.z.df$UL,galtan.z.df$UL.mv),
             r2=rep(galtan.z.df$r2,2),
             r2.total=rep(galtan.z.df$r2.total,2))



galtan.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
galtan.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

galtan.plot<-
  ggplot(galtan.z.plot.df,aes(x=b,
                             y=value.loc,
                             group=value,
                             color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(galtan.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("")+
  ggtitle("C) GAL-TAN")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_galtan.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
galtan.plot
dev.off()



# antielite_salience.z

antielite_salience.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  antielite_salience.z.list[[i]]<-
    for.main.figures(DV="antielite_salience.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


antielite_salience.z.df<-do.call(rbind,antielite_salience.z.list)
#antielite_salience.z.df$value<-substr(antielite_salience.z.df$rowname,1,3)
antielite_salience.z.df$value<-value.names
antielite_salience.z.df$r2<-round_tidy(antielite_salience.z.df$d.r2,2)
antielite_salience.z.df$r2.total<-round_tidy(antielite_salience.z.df$d.r2.multi,2)

antielite_salience.z.plot.df<-
  data.frame(value=rep(antielite_salience.z.df$value,times=2),
             b=c(antielite_salience.z.df$Est.,antielite_salience.z.df$Est..mv),
             LL=c(antielite_salience.z.df$LL,antielite_salience.z.df$LL.mv),
             UL=c(antielite_salience.z.df$UL,antielite_salience.z.df$UL.mv),
             r2=rep(antielite_salience.z.df$r2,2),
             r2.total=rep(antielite_salience.z.df$r2.total,2))



antielite_salience.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
antielite_salience.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

antielite_salience.plot<-
  ggplot(antielite_salience.z.plot.df,aes(x=b,
                             y=value.loc,
                             group=value,
                             color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(antielite_salience.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("")+
  ggtitle("D) Anti-elite")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_antielite_salience.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
antielite_salience.plot
dev.off()

# combine the four plots

# make also three figures stacked version

plot_comb<-
  ggarrange(lrgen.plot,
            lrecon.plot,
            galtan.plot,
            antielite_salience.plot,
            ncol=2, nrow=2,common.legend = T)

png("results/figures/values_mainPO_panels_AD.png",
     width = (21/1.5)*2,height=((29.7*(3/4))/3)*2,units = "cm",res = 600)
plot_comb
dev.off()






# corrupt_salience.z

corrupt_salience.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  corrupt_salience.z.list[[i]]<-
    for.main.figures(DV="corrupt_salience.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


corrupt_salience.z.df<-do.call(rbind,corrupt_salience.z.list)
#corrupt_salience.z.df$value<-substr(corrupt_salience.z.df$rowname,1,3)
corrupt_salience.z.df$value<-value.names
corrupt_salience.z.df$r2<-round_tidy(corrupt_salience.z.df$d.r2,2)
corrupt_salience.z.df$r2.total<-round_tidy(corrupt_salience.z.df$d.r2.multi,2)

corrupt_salience.z.plot.df<-
  data.frame(value=rep(corrupt_salience.z.df$value,times=2),
             b=c(corrupt_salience.z.df$Est.,corrupt_salience.z.df$Est..mv),
             LL=c(corrupt_salience.z.df$LL,corrupt_salience.z.df$LL.mv),
             UL=c(corrupt_salience.z.df$UL,corrupt_salience.z.df$UL.mv),
             r2=rep(corrupt_salience.z.df$r2,2),
             r2.total=rep(corrupt_salience.z.df$r2.total,2))



corrupt_salience.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
corrupt_salience.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

corrupt_salience.plot<-
  ggplot(corrupt_salience.z.plot.df,aes(x=b,
                                        y=value.loc,
                                        group=value,
                                        color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(corrupt_salience.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="corrupt_salience.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_corrupt_salience.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
corrupt_salience.plot
dev.off()





# spendvtax.z

spendvtax.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  spendvtax.z.list[[i]]<-
    for.main.figures(DV="spendvtax.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


spendvtax.z.df<-do.call(rbind,spendvtax.z.list)
#spendvtax.z.df$value<-substr(spendvtax.z.df$rowname,1,3)
spendvtax.z.df$value<-value.names
spendvtax.z.df$r2<-round_tidy(spendvtax.z.df$d.r2,2)
spendvtax.z.df$r2.total<-round_tidy(spendvtax.z.df$d.r2.multi,2)

spendvtax.z.plot.df<-
  data.frame(value=rep(spendvtax.z.df$value,times=2),
             b=c(spendvtax.z.df$Est.,spendvtax.z.df$Est..mv),
             LL=c(spendvtax.z.df$LL,spendvtax.z.df$LL.mv),
             UL=c(spendvtax.z.df$UL,spendvtax.z.df$UL.mv),
             r2=rep(spendvtax.z.df$r2,2),
             r2.total=rep(spendvtax.z.df$r2.total,2))



spendvtax.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
spendvtax.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

spendvtax.plot<-
  ggplot(spendvtax.z.plot.df,aes(x=b,
                                        y=value.loc,
                                        group=value,
                                        color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(spendvtax.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="spendvtax.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_spendvtax.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
spendvtax.plot
dev.off()






# deregulation.z

deregulation.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  deregulation.z.list[[i]]<-
    for.main.figures(DV="deregulation.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


deregulation.z.df<-do.call(rbind,deregulation.z.list)
#deregulation.z.df$value<-substr(deregulation.z.df$rowname,1,3)
deregulation.z.df$value<-value.names
deregulation.z.df$r2<-round_tidy(deregulation.z.df$d.r2,2)
deregulation.z.df$r2.total<-round_tidy(deregulation.z.df$d.r2.multi,2)

deregulation.z.plot.df<-
  data.frame(value=rep(deregulation.z.df$value,times=2),
             b=c(deregulation.z.df$Est.,deregulation.z.df$Est..mv),
             LL=c(deregulation.z.df$LL,deregulation.z.df$LL.mv),
             UL=c(deregulation.z.df$UL,deregulation.z.df$UL.mv),
             r2=rep(deregulation.z.df$r2,2),
             r2.total=rep(deregulation.z.df$r2.total,2))



deregulation.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
deregulation.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

deregulation.plot<-
  ggplot(deregulation.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(deregulation.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="deregulation.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_deregulation.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
deregulation.plot
dev.off()





# redistribution.z

redistribution.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  redistribution.z.list[[i]]<-
    for.main.figures(DV="redistribution.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


redistribution.z.df<-do.call(rbind,redistribution.z.list)
#redistribution.z.df$value<-substr(redistribution.z.df$rowname,1,3)
redistribution.z.df$value<-value.names
redistribution.z.df$r2<-round_tidy(redistribution.z.df$d.r2,2)
redistribution.z.df$r2.total<-round_tidy(redistribution.z.df$d.r2.multi,2)

redistribution.z.plot.df<-
  data.frame(value=rep(redistribution.z.df$value,times=2),
             b=c(redistribution.z.df$Est.,redistribution.z.df$Est..mv),
             LL=c(redistribution.z.df$LL,redistribution.z.df$LL.mv),
             UL=c(redistribution.z.df$UL,redistribution.z.df$UL.mv),
             r2=rep(redistribution.z.df$r2,2),
             r2.total=rep(redistribution.z.df$r2.total,2))



redistribution.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
redistribution.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

redistribution.plot<-
  ggplot(redistribution.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(redistribution.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="redistribution.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_redistribution.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
redistribution.plot
dev.off()





# econ_interven.z

econ_interven.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  econ_interven.z.list[[i]]<-
    for.main.figures(DV="econ_interven.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


econ_interven.z.df<-do.call(rbind,econ_interven.z.list)
#econ_interven.z.df$value<-substr(econ_interven.z.df$rowname,1,3)
econ_interven.z.df$value<-value.names
econ_interven.z.df$r2<-round_tidy(econ_interven.z.df$d.r2,2)
econ_interven.z.df$r2.total<-round_tidy(econ_interven.z.df$d.r2.multi,2)

econ_interven.z.plot.df<-
  data.frame(value=rep(econ_interven.z.df$value,times=2),
             b=c(econ_interven.z.df$Est.,econ_interven.z.df$Est..mv),
             LL=c(econ_interven.z.df$LL,econ_interven.z.df$LL.mv),
             UL=c(econ_interven.z.df$UL,econ_interven.z.df$UL.mv),
             r2=rep(econ_interven.z.df$r2,2),
             r2.total=rep(econ_interven.z.df$r2.total,2))



econ_interven.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
econ_interven.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

econ_interven.plot<-
  ggplot(econ_interven.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(econ_interven.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="econ_interven.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_econ_interven.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
econ_interven.plot
dev.off()





# civlib_laworder.z

civlib_laworder.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  civlib_laworder.z.list[[i]]<-
    for.main.figures(DV="civlib_laworder.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


civlib_laworder.z.df<-do.call(rbind,civlib_laworder.z.list)
#civlib_laworder.z.df$value<-substr(civlib_laworder.z.df$rowname,1,3)
civlib_laworder.z.df$value<-value.names
civlib_laworder.z.df$r2<-round_tidy(civlib_laworder.z.df$d.r2,2)
civlib_laworder.z.df$r2.total<-round_tidy(civlib_laworder.z.df$d.r2.multi,2)

civlib_laworder.z.plot.df<-
  data.frame(value=rep(civlib_laworder.z.df$value,times=2),
             b=c(civlib_laworder.z.df$Est.,civlib_laworder.z.df$Est..mv),
             LL=c(civlib_laworder.z.df$LL,civlib_laworder.z.df$LL.mv),
             UL=c(civlib_laworder.z.df$UL,civlib_laworder.z.df$UL.mv),
             r2=rep(civlib_laworder.z.df$r2,2),
             r2.total=rep(civlib_laworder.z.df$r2.total,2))



civlib_laworder.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
civlib_laworder.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

civlib_laworder.plot<-
  ggplot(civlib_laworder.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(civlib_laworder.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="civlib_laworder.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_civlib_laworder.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
civlib_laworder.plot
dev.off()





# sociallifestyle.z

sociallifestyle.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  sociallifestyle.z.list[[i]]<-
    for.main.figures(DV="sociallifestyle.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


sociallifestyle.z.df<-do.call(rbind,sociallifestyle.z.list)
#sociallifestyle.z.df$value<-substr(sociallifestyle.z.df$rowname,1,3)
sociallifestyle.z.df$value<-value.names
sociallifestyle.z.df$r2<-round_tidy(sociallifestyle.z.df$d.r2,2)
sociallifestyle.z.df$r2.total<-round_tidy(sociallifestyle.z.df$d.r2.multi,2)

sociallifestyle.z.plot.df<-
  data.frame(value=rep(sociallifestyle.z.df$value,times=2),
             b=c(sociallifestyle.z.df$Est.,sociallifestyle.z.df$Est..mv),
             LL=c(sociallifestyle.z.df$LL,sociallifestyle.z.df$LL.mv),
             UL=c(sociallifestyle.z.df$UL,sociallifestyle.z.df$UL.mv),
             r2=rep(sociallifestyle.z.df$r2,2),
             r2.total=rep(sociallifestyle.z.df$r2.total,2))



sociallifestyle.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
sociallifestyle.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

sociallifestyle.plot<-
  ggplot(sociallifestyle.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(sociallifestyle.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="sociallifestyle.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_sociallifestyle.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
sociallifestyle.plot
dev.off()





# religious_principle.z

religious_principle.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  religious_principle.z.list[[i]]<-
    for.main.figures(DV="religious_principle.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


religious_principle.z.df<-do.call(rbind,religious_principle.z.list)
#religious_principle.z.df$value<-substr(religious_principle.z.df$rowname,1,3)
religious_principle.z.df$value<-value.names
religious_principle.z.df$r2<-round_tidy(religious_principle.z.df$d.r2,2)
religious_principle.z.df$r2.total<-round_tidy(religious_principle.z.df$d.r2.multi,2)

religious_principle.z.plot.df<-
  data.frame(value=rep(religious_principle.z.df$value,times=2),
             b=c(religious_principle.z.df$Est.,religious_principle.z.df$Est..mv),
             LL=c(religious_principle.z.df$LL,religious_principle.z.df$LL.mv),
             UL=c(religious_principle.z.df$UL,religious_principle.z.df$UL.mv),
             r2=rep(religious_principle.z.df$r2,2),
             r2.total=rep(religious_principle.z.df$r2.total,2))



religious_principle.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
religious_principle.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

religious_principle.plot<-
  ggplot(religious_principle.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(religious_principle.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="religious_principle.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_religious_principle.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
religious_principle.plot
dev.off()





# immigrate_policy.z

immigrate_policy.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  immigrate_policy.z.list[[i]]<-
    for.main.figures(DV="immigrate_policy.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


immigrate_policy.z.df<-do.call(rbind,immigrate_policy.z.list)
#immigrate_policy.z.df$value<-substr(immigrate_policy.z.df$rowname,1,3)
immigrate_policy.z.df$value<-value.names
immigrate_policy.z.df$r2<-round_tidy(immigrate_policy.z.df$d.r2,2)
immigrate_policy.z.df$r2.total<-round_tidy(immigrate_policy.z.df$d.r2.multi,2)

immigrate_policy.z.plot.df<-
  data.frame(value=rep(immigrate_policy.z.df$value,times=2),
             b=c(immigrate_policy.z.df$Est.,immigrate_policy.z.df$Est..mv),
             LL=c(immigrate_policy.z.df$LL,immigrate_policy.z.df$LL.mv),
             UL=c(immigrate_policy.z.df$UL,immigrate_policy.z.df$UL.mv),
             r2=rep(immigrate_policy.z.df$r2,2),
             r2.total=rep(immigrate_policy.z.df$r2.total,2))



immigrate_policy.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
immigrate_policy.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

immigrate_policy.plot<-
  ggplot(immigrate_policy.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(immigrate_policy.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="immigrate_policy.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_immigrate_policy.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
immigrate_policy.plot
dev.off()





# multiculturalism.z

multiculturalism.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  multiculturalism.z.list[[i]]<-
    for.main.figures(DV="multiculturalism.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


multiculturalism.z.df<-do.call(rbind,multiculturalism.z.list)
#multiculturalism.z.df$value<-substr(multiculturalism.z.df$rowname,1,3)
multiculturalism.z.df$value<-value.names
multiculturalism.z.df$r2<-round_tidy(multiculturalism.z.df$d.r2,2)
multiculturalism.z.df$r2.total<-round_tidy(multiculturalism.z.df$d.r2.multi,2)

multiculturalism.z.plot.df<-
  data.frame(value=rep(multiculturalism.z.df$value,times=2),
             b=c(multiculturalism.z.df$Est.,multiculturalism.z.df$Est..mv),
             LL=c(multiculturalism.z.df$LL,multiculturalism.z.df$LL.mv),
             UL=c(multiculturalism.z.df$UL,multiculturalism.z.df$UL.mv),
             r2=rep(multiculturalism.z.df$r2,2),
             r2.total=rep(multiculturalism.z.df$r2.total,2))



multiculturalism.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
multiculturalism.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

multiculturalism.plot<-
  ggplot(multiculturalism.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(multiculturalism.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="multiculturalism.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_multiculturalism.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
multiculturalism.plot
dev.off()





# urban_rural.z

urban_rural.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  urban_rural.z.list[[i]]<-
    for.main.figures(DV="urban_rural.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


urban_rural.z.df<-do.call(rbind,urban_rural.z.list)
#urban_rural.z.df$value<-substr(urban_rural.z.df$rowname,1,3)
urban_rural.z.df$value<-value.names
urban_rural.z.df$r2<-round_tidy(urban_rural.z.df$d.r2,2)
urban_rural.z.df$r2.total<-round_tidy(urban_rural.z.df$d.r2.multi,2)

urban_rural.z.plot.df<-
  data.frame(value=rep(urban_rural.z.df$value,times=2),
             b=c(urban_rural.z.df$Est.,urban_rural.z.df$Est..mv),
             LL=c(urban_rural.z.df$LL,urban_rural.z.df$LL.mv),
             UL=c(urban_rural.z.df$UL,urban_rural.z.df$UL.mv),
             r2=rep(urban_rural.z.df$r2,2),
             r2.total=rep(urban_rural.z.df$r2.total,2))



urban_rural.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
urban_rural.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

urban_rural.plot<-
  ggplot(urban_rural.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(urban_rural.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="urban_rural.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_urban_rural.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
urban_rural.plot
dev.off()





# environment.z

environment.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  environment.z.list[[i]]<-
    for.main.figures(DV="environment.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


environment.z.df<-do.call(rbind,environment.z.list)
#environment.z.df$value<-substr(environment.z.df$rowname,1,3)
environment.z.df$value<-value.names
environment.z.df$r2<-round_tidy(environment.z.df$d.r2,2)
environment.z.df$r2.total<-round_tidy(environment.z.df$d.r2.multi,2)

environment.z.plot.df<-
  data.frame(value=rep(environment.z.df$value,times=2),
             b=c(environment.z.df$Est.,environment.z.df$Est..mv),
             LL=c(environment.z.df$LL,environment.z.df$LL.mv),
             UL=c(environment.z.df$UL,environment.z.df$UL.mv),
             r2=rep(environment.z.df$r2,2),
             r2.total=rep(environment.z.df$r2.total,2))



environment.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
environment.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

environment.plot<-
  ggplot(environment.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(environment.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="environment.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_environment.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
environment.plot
dev.off()





# regions.z

regions.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  regions.z.list[[i]]<-
    for.main.figures(DV="regions.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


regions.z.df<-do.call(rbind,regions.z.list)
#regions.z.df$value<-substr(regions.z.df$rowname,1,3)
regions.z.df$value<-value.names
regions.z.df$r2<-round_tidy(regions.z.df$d.r2,2)
regions.z.df$r2.total<-round_tidy(regions.z.df$d.r2.multi,2)

regions.z.plot.df<-
  data.frame(value=rep(regions.z.df$value,times=2),
             b=c(regions.z.df$Est.,regions.z.df$Est..mv),
             LL=c(regions.z.df$LL,regions.z.df$LL.mv),
             UL=c(regions.z.df$UL,regions.z.df$UL.mv),
             r2=rep(regions.z.df$r2,2),
             r2.total=rep(regions.z.df$r2.total,2))



regions.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
regions.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

regions.plot<-
  ggplot(regions.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(regions.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="regions.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_regions.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
regions.plot
dev.off()





# international_security.z

international_security.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  international_security.z.list[[i]]<-
    for.main.figures(DV="international_security.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


international_security.z.df<-do.call(rbind,international_security.z.list)
#international_security.z.df$value<-substr(international_security.z.df$rowname,1,3)
international_security.z.df$value<-value.names
international_security.z.df$r2<-round_tidy(international_security.z.df$d.r2,2)
international_security.z.df$r2.total<-round_tidy(international_security.z.df$d.r2.multi,2)

international_security.z.plot.df<-
  data.frame(value=rep(international_security.z.df$value,times=2),
             b=c(international_security.z.df$Est.,international_security.z.df$Est..mv),
             LL=c(international_security.z.df$LL,international_security.z.df$LL.mv),
             UL=c(international_security.z.df$UL,international_security.z.df$UL.mv),
             r2=rep(international_security.z.df$r2,2),
             r2.total=rep(international_security.z.df$r2.total,2))



international_security.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
international_security.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

international_security.plot<-
  ggplot(international_security.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(international_security.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="international_security.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_international_security.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
international_security.plot
dev.off()





# ethnic_minorities.z

ethnic_minorities.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  ethnic_minorities.z.list[[i]]<-
    for.main.figures(DV="ethnic_minorities.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


ethnic_minorities.z.df<-do.call(rbind,ethnic_minorities.z.list)
#ethnic_minorities.z.df$value<-substr(ethnic_minorities.z.df$rowname,1,3)
ethnic_minorities.z.df$value<-value.names
ethnic_minorities.z.df$r2<-round_tidy(ethnic_minorities.z.df$d.r2,2)
ethnic_minorities.z.df$r2.total<-round_tidy(ethnic_minorities.z.df$d.r2.multi,2)

ethnic_minorities.z.plot.df<-
  data.frame(value=rep(ethnic_minorities.z.df$value,times=2),
             b=c(ethnic_minorities.z.df$Est.,ethnic_minorities.z.df$Est..mv),
             LL=c(ethnic_minorities.z.df$LL,ethnic_minorities.z.df$LL.mv),
             UL=c(ethnic_minorities.z.df$UL,ethnic_minorities.z.df$UL.mv),
             r2=rep(ethnic_minorities.z.df$r2,2),
             r2.total=rep(ethnic_minorities.z.df$r2.total,2))



ethnic_minorities.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
ethnic_minorities.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

ethnic_minorities.plot<-
  ggplot(ethnic_minorities.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(ethnic_minorities.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="ethnic_minorities.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_ethnic_minorities.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
ethnic_minorities.plot
dev.off()





# nationalism.z

nationalism.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  nationalism.z.list[[i]]<-
    for.main.figures(DV="nationalism.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


nationalism.z.df<-do.call(rbind,nationalism.z.list)
#nationalism.z.df$value<-substr(nationalism.z.df$rowname,1,3)
nationalism.z.df$value<-value.names
nationalism.z.df$r2<-round_tidy(nationalism.z.df$d.r2,2)
nationalism.z.df$r2.total<-round_tidy(nationalism.z.df$d.r2.multi,2)

nationalism.z.plot.df<-
  data.frame(value=rep(nationalism.z.df$value,times=2),
             b=c(nationalism.z.df$Est.,nationalism.z.df$Est..mv),
             LL=c(nationalism.z.df$LL,nationalism.z.df$LL.mv),
             UL=c(nationalism.z.df$UL,nationalism.z.df$UL.mv),
             r2=rep(nationalism.z.df$r2,2),
             r2.total=rep(nationalism.z.df$r2.total,2))



nationalism.z.plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
nationalism.z.plot.df$Model<-rep(c("Single value","All values"),each=10)

nationalism.plot<-
  ggplot(nationalism.z.plot.df,aes(x=b,
                                 y=value.loc,
                                 group=value,
                                 color=Model))+
  geom_point(size=2.25)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_errorbar(aes(xmin=LL,xmax=UL))+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_hline(yintercept = seq(4.5,50,5), linetype=3, 
             color = "gray", size=.35)+
  geom_text(hjust=0,vjust=0,color="black",
            size=3,aes(x=-0.6,
                       label=ifelse(Model=="Single value", value,"")))+
  geom_text(hjust=0,vjust=0,color=met.brewer("OKeeffe1")[10],
            size=3,aes(x=0.32,
                       label=ifelse(Model=="Single value", substr(r2,2,4),"")))+
  geom_text(x=0.32, y=50, size=3, color="black",
            hjust=0,vjust=0, label=paste0("\u0394","R\u00b2"))+
  geom_text(x=0.38, y=50, size=3, color=met.brewer("OKeeffe1")[3],
            hjust=0,vjust=0,
            label=paste0("(",
                         substr(nationalism.z.plot.df$r2.total[1],2,4),
                         ")"))+
  ylab("")+
  xlab("Fixed slope estimate")+
  ggtitle(DV.vars[DV.vars$DV.var.name=="nationalism.z","DV.name"])+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.4,.3,.1))

png("results/figures/values_nationalism.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
nationalism.plot
dev.off()




