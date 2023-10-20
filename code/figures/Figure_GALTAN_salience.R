library(ggplot2)
library(MetBrewer)
library(rio)
library(ggpubr)

# con

p.con<-import("results/figures/figdata/con_GALTAN_salience.xlsx")

#
p.con$Salience<-
  ifelse(p.con$galtan_salience=="Low Salience","Low",
         ifelse(p.con$galtan_salience=="Average Salience","Average",
                ifelse(p.con$galtan_salience=="High Salience","High",NA)))

p.con$Salience <- 
  factor(p.con$Salience, levels = c("Low", "Average", "High"))

# Exclude data points not present in real data
p.con.ex<-
  p.con[p.con$filter.low!=0 & p.con$filter.mid!=0 & p.con$filter.high!=0,]

p1.con<-ggplot(p.con.ex,aes(y=yvar,x=xvar,
                            color=Salience))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Conformity")+
  ylab("GAL-TAN")+
  scale_color_manual(values=met.brewer("Demuth")[c(7,4,2)])+
  #facet_wrap(~lrecon_salience,ncol=3)+
  theme(legend.position = "top",
        text=element_text(size=12,  family="sans"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ))
p1.con

png(filename = 
      "results/figures/con_GALTAN_salience.png",
    units = "cm",
    height = 29.7/2,width=(21.0/2),res = 600)
p1.con
dev.off()

# tra

p.tra<-import("results/figures/figdata/tra_GALTAN_salience.xlsx")

#
p.tra$Salience<-
  ifelse(p.tra$galtan_salience=="Low Salience","Low",
         ifelse(p.tra$galtan_salience=="Average Salience","Average",
                ifelse(p.tra$galtan_salience=="High Salience","High",NA)))

p.tra$Salience <- 
  factor(p.tra$Salience, levels = c("Low", "Average", "High"))

# Exclude data points not present in real data
p.tra.ex<-
  p.tra[p.tra$filter.low!=0 & p.tra$filter.mid!=0 & p.tra$filter.high!=0,]

p1.tra<-ggplot(p.tra.ex,aes(y=yvar,x=xvar,
                            color=Salience))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Tradition")+
  ylab("GAL-TAN")+
  scale_color_manual(values=met.brewer("Demuth")[c(7,4,2)])+
  #facet_wrap(~lretra_salience,ncol=3)+
  theme(legend.position = "top",
        text=element_text(size=12,  family="sans"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ))
p1.tra

png(filename = 
      "results/figures/tra_GALTAN_salience.png",
    units = "cm",
    height = 29.7/2,width=(21.0/2),res = 600)
p1.tra
dev.off()

# sec

p.sec<-import("results/figures/figdata/sec_GALTAN_salience.xlsx")

#
p.sec$Salience<-
  ifelse(p.sec$galtan_salience=="Low Salience","Low",
         ifelse(p.sec$galtan_salience=="Average Salience","Average",
                ifelse(p.sec$galtan_salience=="High Salience","High",NA)))

p.sec$Salience <- 
  factor(p.sec$Salience, levels = c("Low", "Average", "High"))

# Exclude data points not present in real data
p.sec.ex<-
  p.sec[p.sec$filter.low!=0 & p.sec$filter.mid!=0 & p.sec$filter.high!=0,]

p1.sec<-ggplot(p.sec.ex,aes(y=yvar,x=xvar,
                            color=Salience))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Security")+
  ylab("GAL-TAN")+
  scale_color_manual(values=met.brewer("Demuth")[c(7,4,2)])+
  #facet_wrap(~lresec_salience,ncol=3)+
  theme(legend.position = "top",
        text=element_text(size=12,  family="sans"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ))
p1.sec

png(filename = 
      "results/figures/sec_GALTAN_salience.png",
    units = "cm",
    height = 29.7/2,width=(21.0/2),res = 600)
p1.sec
dev.off()

# sdi

p.sdi<-import("results/figures/figdata/sdi_GALTAN_salience.xlsx")

#
p.sdi$Salience<-
  ifelse(p.sdi$galtan_salience=="Low Salience","Low",
         ifelse(p.sdi$galtan_salience=="Average Salience","Average",
                ifelse(p.sdi$galtan_salience=="High Salience","High",NA)))

p.sdi$Salience <- 
  factor(p.sdi$Salience, levels = c("Low", "Average", "High"))

# Exclude data points not present in real data
p.sdi.ex<-
  p.sdi[p.sdi$filter.low!=0 & p.sdi$filter.mid!=0 & p.sdi$filter.high!=0,]

p1.sdi<-ggplot(p.sdi.ex,aes(y=yvar,x=xvar,
                            color=Salience))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Self-direction")+
  ylab("GAL-TAN")+
  scale_color_manual(values=met.brewer("Demuth")[c(7,4,2)])+
  #facet_wrap(~lresdi_salience,ncol=3)+
  theme(legend.position = "top",
        text=element_text(size=12,  family="sans"),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ))
p1.sdi

png(filename = 
      "results/figures/sdi_GALTAN_salience.png",
    units = "cm",
    height = 29.7/2,width=(21.0/2),res = 600)
p1.sdi
dev.off()

# harmonise the plot axis limits
p2.con<-p1.con+scale_x_continuous(limits=c(-4,4))+
  scale_y_continuous(limits=c(-1.2,1.2))+
  xlab("")+
  ggtitle("A) Conformity")

p2.tra<-p1.tra+scale_x_continuous(limits=c(-4,4))+
  scale_y_continuous(limits=c(-1.2,1.2))+
  xlab("")+
  ggtitle("B) Tradition")

p2.sec<-p1.sec+scale_x_continuous(limits=c(-4,4))+
  scale_y_continuous(limits=c(-1.2,1.2))+
  xlab("")+
  ggtitle("C) Security")

p2.sdi<-p1.sdi+scale_x_continuous(limits=c(-4,4))+
  scale_y_continuous(limits=c(-1.2,1.2))+
  xlab("")+
  ggtitle("D) Self-direction")

# combine to same plot

plot_comb<-
  ggarrange(p2.con,
            p2.tra,
            p2.sec,
            p2.sdi,
            ncol=2, nrow=2,common.legend = T)
plot_comb

png("results/figures/GALTAN_salience_panels_AD.png",
    width = 21,height=29.7*(3/4),units = "cm",res = 600)
plot_comb
dev.off()


