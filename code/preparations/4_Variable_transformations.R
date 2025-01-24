#' ---
#' title: "Variable transformation before running the analysis"
#' output: 
#'   html_document: 
#'     toc: yes
#'     number_sections: yes
#'     keep_md: yes
#' ---
#' 
## ---- include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Load packages
#' 
## -------------------------------------------------------------------------
library(rio)
library(dplyr)

#' 
#' ## Load data
#' 
## -------------------------------------------------------------------------
# Long format data with ESS and CHES merged
dat<-import("../../data/processed/dat.xlsx")

# ESS raw data from which variable labels can be obtained
ESS.dat<-import("../../data/raw/ESS7e02_2.sav")

#' 
#' # Variable transformations
#' 
#' ## Gender
#' 
## -------------------------------------------------------------------------
attr(ESS.dat$gndr,"labels")

# Factorial gndr

dat$gndr.f<-case_when(dat$gndr==1~"Male",
                      dat$gndr==2~"Female",
                      TRUE~NA_character_)

table(dat$gndr.f,useNA="always")

# Numerical gndr

dat$gndr.c<-case_when(dat$gndr==1~-0.5,
                      dat$gndr==2~0.5,
                      TRUE~NA_real_)

table(dat$gndr.c,useNA="always")


#' 
#' ## Age
#' 
## -------------------------------------------------------------------------
attr(ESS.dat$agea,"labels")

table(dat$agea==999)

# centered age divided by 10
dat$age10.c<-(dat$agea-mean(dat$agea,na.rm=T))/10


#' 
#' ## Income
#' 
## -------------------------------------------------------------------------
attr(ESS.dat$hinctnta,"labels")

# recode deciles to quintiles

dat$income<-case_when(
  dat$hinctnta==1 | dat$hinctnta==2 ~ "quint.1",
  dat$hinctnta==3 | dat$hinctnta==4 ~ "quint.2",
  dat$hinctnta==5 | dat$hinctnta==6 ~ "quint.3",
  dat$hinctnta==7 | dat$hinctnta==8 ~ "quint.4",
  dat$hinctnta==9 | dat$hinctnta==10 ~ "quint.5"
)

table(dat$income,useNA="always")

# add missing as additional factor level (to a new variable income.f)

dat$income.f<-case_when(
  is.na(dat$income) ~ "missing",
  TRUE ~ dat$income
)

#define reference level (top quintile)
table(dat$income.f,useNA="always")
dat$income.fr = relevel(as.factor(dat$income.f),
                        ref="quint.5")
table(dat$income.fr,useNA="always")


#' 
#' ## Education
#' 
## -------------------------------------------------------------------------
attr(ESS.dat$eisced,"labels")

# recode education variable

dat$edu<-case_when(dat$eisced==0~NA_character_,
                   dat$eisced==1~"1. <LS",
                   dat$eisced==2~"2. LS",
                   dat$eisced==3~"3. LUS",
                   dat$eisced==4~"4. UUS",
                   dat$eisced==5~"5. AV",
                   dat$eisced==6~"6. BA",
                   dat$eisced==7~"7. MA",
                   TRUE~NA_character_)

table(dat$edu,useNA="always")

# recode reference education (highest) to a new variable (edu.f)

dat$edu.f<-relevel(as.factor(dat$edu),ref="7. MA")
table(dat$edu.f)

# code binary (college=0.5, no college=-0.5)

dat$edu.c<-case_when(dat$eisced==0~NA_real_,
                   dat$eisced>0 & dat$eisced<6~(-0.5),
                   dat$eisced>5 & dat$eisced<8~0.5,
                   TRUE~NA_real_)

table(dat$edu.c,useNA="always")

#' 
#' ## Values
#' 
## -------------------------------------------------------------------------
val.vars<-
  c("ipcrtiv",
    "imprich",
    "ipeqopt",
    "ipshabt",
    "impsafe",
    "impdiff",
    "ipfrule",
    "ipudrst",
    "ipmodst",
    "ipgdtim",
    "impfree",
    "iphlppl",
    "ipsuces",
    "ipstrgv",
    "ipadvnt",
    "ipbhprp",
    "iprspot",
    "iplylfr",
    "impenv",
    "imptrad",
    "impfun")

# reverse code so that high values indicate high endorsment
rev.values<-function(x){7-x}

table(dat[,val.vars[1]])
table(rev.values(dat[,val.vars[1]]))

dat[,val.vars]<-sapply(dat[,val.vars],rev.values)
table(dat[,val.vars[1]])

# code response style

dat$resp.style<-
  rowMeans(dat[,val.vars])

hist(dat$resp.style)

# calculate value means
# check https://www.europeansocialsurvey.org/data/themes.html?t=values

# con
attributes(ESS.dat[,val.vars[7]])$label
attributes(ESS.dat[,val.vars[16]])$label
dat$con<-rowMeans(dat[,c(val.vars[7],val.vars[16])])
dat$con.c<-dat$con-dat$resp.style

# tra
attributes(ESS.dat[,val.vars[9]])$label
attributes(ESS.dat[,val.vars[20]])$label
dat$tra<-rowMeans(dat[,c(val.vars[9],val.vars[20])])
dat$tra.c<-dat$tra-dat$resp.style

# ben
attributes(ESS.dat[,val.vars[12]])$label
attributes(ESS.dat[,val.vars[18]])$label
dat$ben<-rowMeans(dat[,c(val.vars[12],val.vars[18])])
dat$ben.c<-dat$ben-dat$resp.style

# uni
attributes(ESS.dat[,val.vars[3]])$label
attributes(ESS.dat[,val.vars[8]])$label
attributes(ESS.dat[,val.vars[19]])$label
dat$uni<-rowMeans(dat[,c(val.vars[3],val.vars[8],val.vars[19])])
dat$uni.c<-dat$uni-dat$resp.style

# sdi
attributes(ESS.dat[,val.vars[1]])$label
attributes(ESS.dat[,val.vars[11]])$label
dat$sdi<-rowMeans(dat[,c(val.vars[1],val.vars[11])])
dat$sdi.c<-dat$sdi-dat$resp.style

# sti
attributes(ESS.dat[,val.vars[6]])$label
attributes(ESS.dat[,val.vars[15]])$label
dat$sti<-rowMeans(dat[,c(val.vars[6],val.vars[15])])
dat$sti.c<-dat$sti-dat$resp.style

# hed
attributes(ESS.dat[,val.vars[10]])$label
attributes(ESS.dat[,val.vars[21]])$label
dat$hed<-rowMeans(dat[,c(val.vars[10],val.vars[21])])
dat$hed.c<-dat$hed-dat$resp.style

# ach
attributes(ESS.dat[,val.vars[4]])$label
attributes(ESS.dat[,val.vars[13]])$label
dat$ach<-rowMeans(dat[,c(val.vars[4],val.vars[13])])
dat$ach.c<-dat$ach-dat$resp.style

# pow
attributes(ESS.dat[,val.vars[2]])$label
attributes(ESS.dat[,val.vars[17]])$label
dat$pow<-rowMeans(dat[,c(val.vars[2],val.vars[17])])
dat$pow.c<-dat$pow-dat$resp.style

# sec
attributes(ESS.dat[,val.vars[5]])$label
attributes(ESS.dat[,val.vars[14]])$label
dat$sec<-rowMeans(dat[,c(val.vars[5],val.vars[14])])
dat$sec.c<-dat$sec-dat$resp.style

vals<-c("con","tra","ben","uni","sdi",
        "sti","hed","ach","pow","sec")



#' 
#' ## Political orientation
#' 
## -------------------------------------------------------------------------
#add scaling SDs to the data.frame from CHES dataset
CHES_2014<-
  import("../../data/raw/2014_CHES_dataset_means.csv")

#scale each variable with mean and sd from CHES

PI.vars<-c("lrgen","lrecon","galtan")
PP.vars<-c("spendvtax","deregulation","redistribution",
           "econ_interven","civlib_laworder","sociallifestyle",
           "religious_principle","immigrate_policy","multiculturalism",
           "urban_rural","environment","regions","international_security",
           "ethnic_minorities","nationalism")
SA.vars<-c("lrecon_salience","galtan_salience",
           "antielite_salience","corrupt_salience")
CHES.vars<-c(PI.vars,PP.vars,SA.vars)


dat$lrgen.z<-(dat[,CHES.vars[1]]-mean(CHES_2014[,CHES.vars[1]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[1]],na.rm=T)

dat$lrecon.z<-(dat[,CHES.vars[2]]-mean(CHES_2014[,CHES.vars[2]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[2]],na.rm=T)

dat$galtan.z<-(dat[,CHES.vars[3]]-mean(CHES_2014[,CHES.vars[3]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[3]],na.rm=T)

dat$spendvtax.z<-(dat[,CHES.vars[4]]-mean(CHES_2014[,CHES.vars[4]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[4]],na.rm=T)

dat$deregulation.z<-(dat[,CHES.vars[5]]-
                       mean(CHES_2014[,CHES.vars[5]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[5]],na.rm=T)

dat$redistribution.z<-(dat[,CHES.vars[6]]-
                         mean(CHES_2014[,CHES.vars[6]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[6]],na.rm=T)

dat$econ_interven.z<-(dat[,CHES.vars[7]]-
                        mean(CHES_2014[,CHES.vars[7]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[7]],na.rm=T)

dat$civlib_laworder.z<-(dat[,CHES.vars[8]]-
                          mean(CHES_2014[,CHES.vars[8]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[8]],na.rm=T)

dat$sociallifestyle.z<-(dat[,CHES.vars[9]]-
                          mean(CHES_2014[,CHES.vars[9]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[9]],na.rm=T)

dat$religious_principle.z<-(dat[,CHES.vars[10]]-
                              mean(CHES_2014[,CHES.vars[10]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[10]],na.rm=T)

dat$immigrate_policy.z<-(dat[,CHES.vars[11]]-
                           mean(CHES_2014[,CHES.vars[11]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[11]],na.rm=T)

dat$multiculturalism.z<-(dat[,CHES.vars[12]]-
                           mean(CHES_2014[,CHES.vars[12]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[12]],na.rm=T)

dat$urban_rural.z<-(dat[,CHES.vars[13]]-
                      mean(CHES_2014[,CHES.vars[13]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[13]],na.rm=T)

dat$environment.z<-(dat[,CHES.vars[14]]-
                      mean(CHES_2014[,CHES.vars[14]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[14]],na.rm=T)

dat$regions.z<-(dat[,CHES.vars[15]]-mean(CHES_2014[,CHES.vars[15]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[15]],na.rm=T)

dat$international_security.z<-(dat[,CHES.vars[16]]-
                                 mean(CHES_2014[,CHES.vars[16]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[16]],na.rm=T)

dat$ethnic_minorities.z<-(dat[,CHES.vars[17]]-
                            mean(CHES_2014[,CHES.vars[17]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[17]],na.rm=T)

dat$nationalism.z<-(dat[,CHES.vars[18]]-
                      mean(CHES_2014[,CHES.vars[18]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[18]],na.rm=T)

dat$lrecon_salience.z<-(dat[,CHES.vars[19]]-
                          mean(CHES_2014[,CHES.vars[19]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[19]],na.rm=T)

dat$galtan_salience.z<-(dat[,CHES.vars[20]]-
                          mean(CHES_2014[,CHES.vars[20]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[20]],na.rm=T)

dat$antielite_salience.z<-(dat[,CHES.vars[21]]-
                             mean(CHES_2014[,CHES.vars[21]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[21]],na.rm=T)

dat$corrupt_salience.z<-(dat[,CHES.vars[22]]-
                             mean(CHES_2014[,CHES.vars[22]],na.rm=T))/
  sd(CHES_2014[,CHES.vars[22]],na.rm=T)
  

#' 
#' ## Vote
#' 
## -------------------------------------------------------------------------
table(dat$vote)
attributes(ESS.dat$vote)

dat$vote.c<-case_when(dat$vote==1~0.5,
                      dat$vote==2~(-0.5),
                      TRUE~NA_real_)
table(dat$vote.c)

#' 
#' ## Belonging to minority ethnic group
#' 
## -------------------------------------------------------------------------
attr(ESS.dat$blgetmg,"labels")
table(ESS.dat$blgetmg,useNA="always")

# Factorial blgetmg

dat$blgetmg.f<-case_when(dat$blgetmg==1~"Yes",
                         TRUE~"No")

table(dat$blgetmg.f,useNA="always")


# Numerical blgetmg

dat$blgetmg.c<-case_when(dat$blgetmg==1~0.5,
                         TRUE~(-0.5))

table(dat$blgetmg.c,useNA="always")



#' 
#' # Final set of variables needed for the analysis
#' 
## -------------------------------------------------------------------------
analysis.vars<-
  c("idno","cntry",
    "dweight","pspwght","pweight",
    "pt.nmbr","pt.name",
    "gndr.f","gndr.c","agea","age10.c",
    "income","income.f","income.fr",
    "edu","edu.f","edu.c",
    "blgetmg","blgetmg.f","blgetmg.c",
    "vote","vote.c",
    "con","con.c",
    "tra","tra.c",
    "ben","ben.c",
    "uni","uni.c",
    "sdi","sdi.c",
    "sti","sti.c",
    "hed","hed.c",
    "ach","ach.c",
    "pow","pow.c",
    "sec","sec.c",
    "resp.style",
    all_of(CHES.vars),
    all_of(paste0(CHES.vars,".z")))

# test if they are all in the data file
analysis.vars %in% names(dat)

# exclude variable not needed
fdat<-dat[,analysis.vars]
str(fdat)

# construct analysis weights

fdat$anweight=fdat$pspwght*fdat$pweight

# save the final data file
export(fdat,
       "../../data/processed/fdat.xlsx",overwrite=T)

#' # Session information
#' 
## -------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

