#Figure West-East

#source("code/custom_functions.R")
library(rio)
library(ggplot2)
library(MetBrewer)
library(finalfit)
library(ggpubr)
library(dplyr)
library(ggflags)

met.brewer("OKeeffe1")
met.brewer("Demuth")

# lrgen uni

## read data

lrgen.uni.plot.df<-
  import("results/figures/figdata/uni.lrgen.mod3.ranefs.xlsx")
lrgen.uni.plot.df$west_east<-
  case_when(lrgen.uni.plot.df$grp == "AT" |
              lrgen.uni.plot.df$grp == "BE" |
              lrgen.uni.plot.df$grp == "CH" |
              lrgen.uni.plot.df$grp == "DE" |
              lrgen.uni.plot.df$grp == "DK" |
              lrgen.uni.plot.df$grp == "ES" |
              lrgen.uni.plot.df$grp == "FI" |
              lrgen.uni.plot.df$grp == "FR" |
              lrgen.uni.plot.df$grp == "GB" |
              lrgen.uni.plot.df$grp == "IE" |
              lrgen.uni.plot.df$grp == "IL" |
              lrgen.uni.plot.df$grp == "NL" |
              lrgen.uni.plot.df$grp == "NO" |
              lrgen.uni.plot.df$grp == "PT" |
              lrgen.uni.plot.df$grp == "SE" ~ "Western Europe",
            TRUE~"Post-communist")
lrgen.uni.plot.df

# order by slope estimate
lrgen.uni.plot.df<-
  lrgen.uni.plot.df[order(-lrgen.uni.plot.df$Slope),] 

# define y-coordinates
lrgen.uni.plot.df$cntry.loc<-seq(1,50,2.5)

lrgen.uni.plot<-
  ggplot(lrgen.uni.plot.df,aes(x=Slope,
                             y=cntry.loc,
                             #group=value,
                             color=west_east))+
  geom_point(size=6)+
  geom_flag(aes(x=Slope,
                y=cntry.loc,
                country=tolower(grp)),size=4)+
  geom_errorbar(aes(xmin=LL,xmax=UL),size=0.5)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_vline(xintercept = (lrgen.uni.plot.df$Slope-
                             lrgen.uni.plot.df$condval)[1]
             , linetype=2, 
             color = "black", size=.5)+
  ylab("")+
  xlab("")+
  ggtitle("A) Left-Right")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0)) +
  scale_y_continuous(limits=c(-2,52))+
  scale_x_continuous(limits=c(-0.7,0.2))

lrgen.uni.plot

png("results/figures/WE.lrgen.uni.plot.png",
    width = 21/1.5,height=(29.7*(3/4))/1.5,units = "cm",res = 600)
lrgen.uni.plot
dev.off()



# lrecon uni

## read data

lrecon.uni.plot.df<-
  import("results/figures/figdata/uni.lrecon.mod3.ranefs.xlsx")
lrecon.uni.plot.df$west_east<-
  case_when(lrecon.uni.plot.df$grp == "AT" |
              lrecon.uni.plot.df$grp == "BE" |
              lrecon.uni.plot.df$grp == "CH" |
              lrecon.uni.plot.df$grp == "DE" |
              lrecon.uni.plot.df$grp == "DK" |
              lrecon.uni.plot.df$grp == "ES" |
              lrecon.uni.plot.df$grp == "FI" |
              lrecon.uni.plot.df$grp == "FR" |
              lrecon.uni.plot.df$grp == "GB" |
              lrecon.uni.plot.df$grp == "IE" |
              lrecon.uni.plot.df$grp == "IL" |
              lrecon.uni.plot.df$grp == "NL" |
              lrecon.uni.plot.df$grp == "NO" |
              lrecon.uni.plot.df$grp == "PT" |
              lrecon.uni.plot.df$grp == "SE" ~ "Western Europe",
            TRUE~"Post-communist")
lrecon.uni.plot.df

# order by slope estimate
lrecon.uni.plot.df<-
  lrecon.uni.plot.df[order(-lrecon.uni.plot.df$Slope),] 

# define y-coordinates
lrecon.uni.plot.df$cntry.loc<-seq(1,50,2.5)

lrecon.uni.plot<-
  ggplot(lrecon.uni.plot.df,aes(x=Slope,
                               y=cntry.loc,
                               #group=value,
                               color=west_east))+
  geom_point(size=6)+
  geom_flag(aes(x=Slope,
                y=cntry.loc,
                country=tolower(grp)),size=4)+
  geom_errorbar(aes(xmin=LL,xmax=UL),size=0.5)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_vline(xintercept = (lrecon.uni.plot.df$Slope-
                             lrecon.uni.plot.df$condval)[1]
             , linetype=2, 
             color = "black", size=.5)+
  ylab("")+
  xlab("")+
  ggtitle("B) Economic Left-Right")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0)) +
  scale_y_continuous(limits=c(-2,52))+
  scale_x_continuous(limits=c(-0.7,0.2))

lrecon.uni.plot

png("results/figures/WE.lrecon.uni.plot.png",
    width = 21/1.5,height=(29.7*(3/4))/1.5,units = "cm",res = 600)
lrecon.uni.plot
dev.off()


# galtan uni

## read data

galtan.uni.plot.df<-
  import("results/figures/figdata/uni.galtan.mod3.ranefs.xlsx")
galtan.uni.plot.df$west_east<-
  case_when(galtan.uni.plot.df$grp == "AT" |
              galtan.uni.plot.df$grp == "BE" |
              galtan.uni.plot.df$grp == "CH" |
              galtan.uni.plot.df$grp == "DE" |
              galtan.uni.plot.df$grp == "DK" |
              galtan.uni.plot.df$grp == "ES" |
              galtan.uni.plot.df$grp == "FI" |
              galtan.uni.plot.df$grp == "FR" |
              galtan.uni.plot.df$grp == "GB" |
              galtan.uni.plot.df$grp == "IE" |
              galtan.uni.plot.df$grp == "IL" |
              galtan.uni.plot.df$grp == "NL" |
              galtan.uni.plot.df$grp == "NO" |
              galtan.uni.plot.df$grp == "PT" |
              galtan.uni.plot.df$grp == "SE" ~ "Western Europe",
            TRUE~"Post-communist")
galtan.uni.plot.df

# order by slope estimate
galtan.uni.plot.df<-
  galtan.uni.plot.df[order(-galtan.uni.plot.df$Slope),] 

# define y-coordinates
galtan.uni.plot.df$cntry.loc<-seq(1,50,2.5)

galtan.uni.plot<-
  ggplot(galtan.uni.plot.df,aes(x=Slope,
                                y=cntry.loc,
                                #group=value,
                                color=west_east))+
  geom_point(size=6)+
  geom_flag(aes(x=Slope,
                y=cntry.loc,
                country=tolower(grp)),size=4)+
  geom_errorbar(aes(xmin=LL,xmax=UL),size=0.5)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_vline(xintercept = (galtan.uni.plot.df$Slope-
                             galtan.uni.plot.df$condval)[1]
             , linetype=2, 
             color = "black", size=.5)+
  ylab("")+
  xlab("")+
  ggtitle("C) GAL-TAN")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0)) +
  scale_y_continuous(limits=c(-2,52))+
  scale_x_continuous(limits=c(-0.7,0.2))

galtan.uni.plot

png("results/figures/WE.galtan.uni.plot.png",
    width = 21/1.5,height=(29.7*(3/4))/1.5,units = "cm",res = 600)
galtan.uni.plot
dev.off()


# antielite_salience uni

## read data

antielite_salience.uni.plot.df<-
  import("results/figures/figdata/uni.antielite_salience.mod3.ranefs.xlsx")
antielite_salience.uni.plot.df$west_east<-
  case_when(antielite_salience.uni.plot.df$grp == "AT" |
              antielite_salience.uni.plot.df$grp == "BE" |
              antielite_salience.uni.plot.df$grp == "CH" |
              antielite_salience.uni.plot.df$grp == "DE" |
              antielite_salience.uni.plot.df$grp == "DK" |
              antielite_salience.uni.plot.df$grp == "ES" |
              antielite_salience.uni.plot.df$grp == "FI" |
              antielite_salience.uni.plot.df$grp == "FR" |
              antielite_salience.uni.plot.df$grp == "GB" |
              antielite_salience.uni.plot.df$grp == "IE" |
              antielite_salience.uni.plot.df$grp == "IL" |
              antielite_salience.uni.plot.df$grp == "NL" |
              antielite_salience.uni.plot.df$grp == "NO" |
              antielite_salience.uni.plot.df$grp == "PT" |
              antielite_salience.uni.plot.df$grp == "SE" ~ "Western Europe",
            TRUE~"Post-communist")
antielite_salience.uni.plot.df

# order by slope estimate
antielite_salience.uni.plot.df<-
  antielite_salience.uni.plot.df[order(-antielite_salience.uni.plot.df$Slope),] 

# define y-coordinates
antielite_salience.uni.plot.df$cntry.loc<-seq(1,50,2.5)

antielite_salience.uni.plot<-
  ggplot(antielite_salience.uni.plot.df,aes(x=Slope,
                                y=cntry.loc,
                                #group=value,
                                color=west_east))+
  geom_point(size=6)+
  geom_flag(aes(x=Slope,
                y=cntry.loc,
                country=tolower(grp)),size=4)+
  geom_errorbar(aes(xmin=LL,xmax=UL),size=0.5)+
  scale_color_manual(values=met.brewer("OKeeffe1")[c(3,10)])+
  geom_vline(xintercept = 0, linetype=1, 
             color = "black", size=.5)+
  geom_vline(xintercept = (antielite_salience.uni.plot.df$Slope-
                             antielite_salience.uni.plot.df$condval)[1]
             , linetype=2, 
             color = "black", size=.5)+
  ylab("")+
  xlab("")+
  ggtitle("D) Anti-elite")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.margin=margin(b=0)) +
  scale_y_continuous(limits=c(-2,52))+
  scale_x_continuous(limits=c(-0.7,0.2))

antielite_salience.uni.plot

png("results/figures/WE.antielite_salience.uni.plot.png",
    width = 21/1.5,height=(29.7*(3/4))/1.5,units = "cm",res = 600)
antielite_salience.uni.plot
dev.off()

# combine the four figures


plot_uni.comb<-
  ggarrange(lrgen.uni.plot,
            lrecon.uni.plot,
            galtan.uni.plot,
            antielite_salience.uni.plot,
            ncol=2, nrow=2,common.legend = T)

png("results/figures/WE_uni.png",
    width = (21/1.5)*2,height=((29.7*(3/4))/1.5)*2,
    units = "cm",res = 600)
plot_uni.comb
dev.off()



