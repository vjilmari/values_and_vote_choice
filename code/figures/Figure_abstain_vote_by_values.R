# Prediction figures for abstaining/voting

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

coef.list<-list()

for (i in 1:length(value.vars)){
  coef.list[[i]]<-
    import(paste0("results/ERQ3/",value.vars[i],
                  "_FE.xlsx"))[4,]
  
}
coef.df<-do.call(rbind,coef.list)
coef.df$Value<-value.names


# full model

all.coef.df<-import("results/ERQ3/all_FE.xlsx")[4:13,]
cbind(all.coef.df$rowname,
      coef.df$rowname,
      value.names)

all.coef.df$Value<-value.names


plot.df<-
  data.frame(Value=c(coef.df$Value,all.coef.df$Value),
             b=c(coef.df$Est.,all.coef.df$Est.),
             LL=c(coef.df$LL,all.coef.df$LL),
             UL=c(coef.df$UL,all.coef.df$UL))
plot.df


plot.df$value.loc<-c(seq(1,50,5),seq(2,50,5))
plot.df$Model<-rep(c("Single value","All values"),each=10)

vote.plot<-
  ggplot(plot.df,aes(x=b,
                           y=value.loc,
                           group=Value,
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
                       label=ifelse(Model=="Single value", Value,"")))+
  ylab("")+
  xlab("Fixed log odds estimate")+
  ggtitle("Voting")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0),
        plot.title = element_text(face="bold",size=10)) +
  scale_y_continuous(limits=c(0,52))+
  scale_x_continuous(limits=c(-0.6,0.4),breaks=seq(-.2,.3,.1))

png("results/figures/values_vote.png",
    width = 21/1.5,height=(29.7*(3/4))/3,units = "cm",res = 600)
vote.plot
dev.off()

