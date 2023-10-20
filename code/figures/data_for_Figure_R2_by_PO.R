# Compile data used in figure with R2 by PO

library(rio)
library(finalfit)
library(dplyr)
library(rempsyc)
library(flextable)
library(officer)
library(tibble)

source("code/custom_functions.R")

fdat<-import("data/processed/fdat.xlsx")

value.vars<-
  c("con","tra","ben","uni","sdi",
    "sti","hed","ach","pow","sec")

value.vars.c<-paste0(value.vars,".c")

DV.vars<-names(fdat)[which(names(fdat)=="lrgen.z"):
                       which(names(fdat)=="corrupt_salience.z")]

# For the entire dataset

# ICCs

ICCs<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
 ICCs[i]<-
   import(paste0("results/main/",DV.vars[i],"/mod0_DC.xlsx"))[4,"total"]
}
cbind(DV.vars,round_tidy(as.numeric(ICCs),2))

# covariates

COVs<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  COVs[i]<-
    import(paste0("results/main/",DV.vars[i],"/mod1_R2.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(COVs),2))

# covariates including ethnic minority

COVs_minority<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  COVs_minority[i]<-
    import(paste0("results/main_with_minority/",
                  DV.vars[i],"/mod1_R2.xlsx"))[1,"within"]
}
cbind(DV.vars,
      round_tidy(as.numeric(COVs),2),
      round_tidy(as.numeric(COVs_minority),2))

# values

valueR2<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  valueR2[i]<-
    import(paste0("results/main/",DV.vars[i],
                  "/con_mod4_R2c_total.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(valueR2),2))

# values with minority

valueR2_minority<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  valueR2_minority[i]<-
    import(paste0("results/main_with_minority/",DV.vars[i],
                  "/con_mod4_R2c_total.xlsx"))[1,"within"]
}
cbind(DV.vars,
      round_tidy(as.numeric(valueR2),3),
      round_tidy(as.numeric(valueR2_minority),3))

# how much minority adds to COV R2?
round_tidy(100*(COVs_minority-COVs)/COVs,1)

# or changes value R2?
round_tidy(100*(valueR2_minority-valueR2)/valueR2,1)


Tab<-
  cbind.data.frame(DV.vars,
      ICC=round_tidy(as.numeric(ICCs),2),
      COVR2=round_tidy(as.numeric(COVs),2),
      VALUER2=round_tidy(as.numeric(valueR2),2),
      COVR2_minor=round_tidy(as.numeric(COVs_minority),2),
      VALUER2_minor=round_tidy(as.numeric(valueR2_minority),2))
Tab$Variable<-
  case_when(
    Tab$DV.vars=="lrgen.z"~"Left-Right General",
    Tab$DV.vars=="lrecon.z"~"Left-Right Economic",
    Tab$DV.vars=="galtan.z"~"GAL-TAN",
    Tab$DV.vars=="spendvtax.z"~"Public services vs. reducing taxes",
    Tab$DV.vars=="deregulation.z"~"Deregulation of markets",
    Tab$DV.vars=="redistribution.z"~"Redistribution of wealth",
    Tab$DV.vars=="econ_interven.z"~"State intervention in the economy",
    Tab$DV.vars=="civlib_laworder.z"~"Civil liberties vs. law and order",
    Tab$DV.vars=="sociallifestyle.z"~"Social lifestyle policies",
    Tab$DV.vars=="religious_principle.z"~"Religious principles",
    Tab$DV.vars=="immigrate_policy.z"~"Immigration policy",
    Tab$DV.vars=="multiculturalism.z"~"Multiculturalism vs. assimilation",
    Tab$DV.vars=="urban_rural.z"~"Urban vs. rural interests",
    Tab$DV.vars=="environment.z"~"Environmental protection vs. economic growth",
    Tab$DV.vars=="regions.z"~"Decentralization to regions/localities",
    Tab$DV.vars=="international_security.z"~"International troop deployment",
    Tab$DV.vars=="ethnic_minorities.z"~"Rights of ethnic minorities",
    Tab$DV.vars=="nationalism.z"~"Cosmopolitanism vs. nationalism",
    Tab$DV.vars=="lrecon_salience.z"~"Left-Right Economic salience",
    Tab$DV.vars=="galtan_salience.z"~"GAL-TAN salience",
    Tab$DV.vars=="antielite_salience.z"~"Anti-elite",
    Tab$DV.vars=="corrupt_salience.z"~"Reducing corruption")

Tab1<-
  cbind.data.frame(
    Variable=Tab$Variable,
    ICC=substr(Tab$ICC,2,4),
    COV_R2=substr(Tab$COVR2,2,4),
    VALUE_R2=substr(Tab$VALUER2,2,4),
    COV_R2_minor=substr(Tab$COVR2_minor,2,4),
    VALUE_R2_minor=substr(Tab$VALUER2_minor,2,4))
Tab1

nice_table(Tab1)

save_as_docx(nice_table(Tab1),
             path = "results/tables/table_R2_all.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))



# For college educated

# ICCs_col

ICCs_col<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  ICCs_col[i]<-
    import(paste0("results/college/",
                  DV.vars[i],"/mod0_DC.xlsx"))[4,"total"]
}
cbind(DV.vars,round_tidy(as.numeric(ICCs_col),2))

# covariates

COVs_col<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  COVs_col[i]<-
    import(paste0("results/college/",DV.vars[i],
                  "/mod1_R2.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(COVs_col),2))

# values

valueR2_col<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  valueR2_col[i]<-
    import(paste0("results/college/",DV.vars[i],
                  "/con_mod4_R2c_total.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(valueR2_col),2))

Tab_col<-
  cbind.data.frame(DV.vars,
                   ICC=round_tidy(as.numeric(ICCs_col),2),
                   COVR2=round_tidy(as.numeric(COVs_col),2),
                   valueR2_col=round_tidy(as.numeric(valueR2_col),2))
Tab_col$Variable<-
  case_when(
    Tab_col$DV.vars=="lrgen.z"~"Left-Right General",
    Tab_col$DV.vars=="lrecon.z"~"Left-Right Economic",
    Tab_col$DV.vars=="galtan.z"~"GAL-TAN",
    Tab_col$DV.vars=="spendvtax.z"~"Public services vs. reducing taxes",
    Tab_col$DV.vars=="deregulation.z"~"Deregulation of markets",
    Tab_col$DV.vars=="redistribution.z"~"Redistribution of wealth",
    Tab_col$DV.vars=="econ_interven.z"~"State intervention in the economy",
    Tab_col$DV.vars=="civlib_laworder.z"~"Civil liberties vs. law and order",
    Tab_col$DV.vars=="sociallifestyle.z"~"Social lifestyle policies",
    Tab_col$DV.vars=="religious_principle.z"~"Religious principles",
    Tab_col$DV.vars=="immigrate_policy.z"~"Immigration policy",
    Tab_col$DV.vars=="multiculturalism.z"~"Multiculturalism vs. assimilation",
    Tab_col$DV.vars=="urban_rural.z"~"Urban vs. rural interests",
    Tab_col$DV.vars=="environment.z"~"Environmental protection vs. economic growth",
    Tab_col$DV.vars=="regions.z"~"Decentralization to regions/localities",
    Tab_col$DV.vars=="international_security.z"~"International troop deployment",
    Tab_col$DV.vars=="ethnic_minorities.z"~"Rights of ethnic minorities",
    Tab_col$DV.vars=="nationalism.z"~"Cosmopolitanism vs. nationalism",
    Tab_col$DV.vars=="lrecon_salience.z"~"Left-Right Economic salience",
    Tab_col$DV.vars=="galtan_salience.z"~"GAL-TAN salience",
    Tab_col$DV.vars=="antielite_salience.z"~"Anti-elite",
    Tab_col$DV.vars=="corrupt_salience.z"~"Reducing corruption")

Tab_col1<-
  cbind.data.frame(
    Variable=Tab_col$Variable,
    ICC=substr(Tab_col$ICC,2,4),
    COV_R2=substr(Tab_col$COVR2,2,4),
    VALUE_R2=substr(Tab_col$valueR2_col,2,4))
Tab_col1

nice_table(Tab_col1)

save_as_docx(nice_table(Tab_col1),
             path = "results/tables/table_R2_college.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))







# For non college educated

# ICCs_nocol

ICCs_nocol<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  ICCs_nocol[i]<-
    import(paste0("results/no_college/",
                  DV.vars[i],"/mod0_DC.xlsx"))[4,"total"]
}
cbind(DV.vars,round_tidy(as.numeric(ICCs_nocol),2))

# covariates

COVs_nocol<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  COVs_nocol[i]<-
    import(paste0("results/no_college/",DV.vars[i],
                  "/mod1_R2.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(COVs_nocol),2))

# values

valueR2_nocol<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  valueR2_nocol[i]<-
    import(paste0("results/no_college/",DV.vars[i],
                  "/con_mod4_R2c_total.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(valueR2_nocol),2))

Tab_nocol<-
  cbind.data.frame(DV.vars,
                   ICC=round_tidy(as.numeric(ICCs_nocol),2),
                   COVR2=round_tidy(as.numeric(COVs_nocol),2),
                   valueR2_nocol=round_tidy(as.numeric(valueR2_nocol),2))
Tab_nocol$Variable<-
  case_when(
    Tab_nocol$DV.vars=="lrgen.z"~"Left-Right General",
    Tab_nocol$DV.vars=="lrecon.z"~"Left-Right Economic",
    Tab_nocol$DV.vars=="galtan.z"~"GAL-TAN",
    Tab_nocol$DV.vars=="spendvtax.z"~"Public services vs. reducing taxes",
    Tab_nocol$DV.vars=="deregulation.z"~"Deregulation of markets",
    Tab_nocol$DV.vars=="redistribution.z"~"Redistribution of wealth",
    Tab_nocol$DV.vars=="econ_interven.z"~"State intervention in the economy",
    Tab_nocol$DV.vars=="civlib_laworder.z"~"Civil liberties vs. law and order",
    Tab_nocol$DV.vars=="sociallifestyle.z"~"Social lifestyle policies",
    Tab_nocol$DV.vars=="religious_principle.z"~"Religious principles",
    Tab_nocol$DV.vars=="immigrate_policy.z"~"Immigration policy",
    Tab_nocol$DV.vars=="multiculturalism.z"~"Multiculturalism vs. assimilation",
    Tab_nocol$DV.vars=="urban_rural.z"~"Urban vs. rural interests",
    Tab_nocol$DV.vars=="environment.z"~"Environmental protection vs. economic growth",
    Tab_nocol$DV.vars=="regions.z"~"Decentralization to regions/localities",
    Tab_nocol$DV.vars=="international_security.z"~"International troop deployment",
    Tab_nocol$DV.vars=="ethnic_minorities.z"~"Rights of ethnic minorities",
    Tab_nocol$DV.vars=="nationalism.z"~"Cosmopolitanism vs. nationalism",
    Tab_nocol$DV.vars=="lrecon_salience.z"~"Left-Right Economic salience",
    Tab_nocol$DV.vars=="galtan_salience.z"~"GAL-TAN salience",
    Tab_nocol$DV.vars=="antielite_salience.z"~"Anti-elite",
    Tab_nocol$DV.vars=="corrupt_salience.z"~"Reducing corruption")

Tab_nocol1<-
  cbind.data.frame(
    Variable=Tab_nocol$Variable,
    ICC=substr(Tab_nocol$ICC,2,4),
    COV_R2=substr(Tab_nocol$COVR2,2,4),
    VALUE_R2=substr(Tab_nocol$valueR2_nocol,2,4))
Tab_nocol1

nice_table(Tab_nocol1)

save_as_docx(nice_table(Tab_nocol1),
             path = "results/tables/table_R2_no_college.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))






# For Western Europe

# ICCs_west

ICCs_west<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  ICCs_west[i]<-
    import(paste0("results/west/",
                  DV.vars[i],"/mod0_DC.xlsx"))[4,"total"]
}
cbind(DV.vars,round_tidy(as.numeric(ICCs_west),2))

# covariates

COVs_west<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  COVs_west[i]<-
    import(paste0("results/west/",DV.vars[i],
                  "/mod1_R2.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(COVs_west),2))

# values

valueR2_west<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  valueR2_west[i]<-
    import(paste0("results/west/",DV.vars[i],
                  "/con_mod4_R2c_total.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(valueR2_west),2))

Tab_west<-
  cbind.data.frame(DV.vars,
                   ICC=round_tidy(as.numeric(ICCs_west),2),
                   COVR2=round_tidy(as.numeric(COVs_west),2),
                   valueR2_west=round_tidy(as.numeric(valueR2_west),2))
Tab_west$Variable<-
  case_when(
    Tab_west$DV.vars=="lrgen.z"~"Left-Right General",
    Tab_west$DV.vars=="lrecon.z"~"Left-Right Economic",
    Tab_west$DV.vars=="galtan.z"~"GAL-TAN",
    Tab_west$DV.vars=="spendvtax.z"~"Public services vs. reducing taxes",
    Tab_west$DV.vars=="deregulation.z"~"Deregulation of markets",
    Tab_west$DV.vars=="redistribution.z"~"Redistribution of wealth",
    Tab_west$DV.vars=="econ_interven.z"~"State intervention in the economy",
    Tab_west$DV.vars=="civlib_laworder.z"~"Civil liberties vs. law and order",
    Tab_west$DV.vars=="sociallifestyle.z"~"Social lifestyle policies",
    Tab_west$DV.vars=="religious_principle.z"~"Religious principles",
    Tab_west$DV.vars=="immigrate_policy.z"~"Immigration policy",
    Tab_west$DV.vars=="multiculturalism.z"~"Multiculturalism vs. assimilation",
    Tab_west$DV.vars=="urban_rural.z"~"Urban vs. rural interests",
    Tab_west$DV.vars=="environment.z"~"Environmental protection vs. economic growth",
    Tab_west$DV.vars=="regions.z"~"Decentralization to regions/localities",
    Tab_west$DV.vars=="international_security.z"~"International troop deployment",
    Tab_west$DV.vars=="ethnic_minorities.z"~"Rights of ethnic minorities",
    Tab_west$DV.vars=="nationalism.z"~"Cosmopolitanism vs. nationalism",
    Tab_west$DV.vars=="lrecon_salience.z"~"Left-Right Economic salience",
    Tab_west$DV.vars=="galtan_salience.z"~"GAL-TAN salience",
    Tab_west$DV.vars=="antielite_salience.z"~"Anti-elite",
    Tab_west$DV.vars=="corrupt_salience.z"~"Reducing corruption")

Tab_west1<-
  cbind.data.frame(
    Variable=Tab_west$Variable,
    ICC=substr(Tab_west$ICC,2,4),
    COV_R2=substr(Tab_west$COVR2,2,4),
    VALUE_R2=substr(Tab_west$valueR2_west,2,4))
Tab_west1

nice_table(Tab_west1)

save_as_docx(nice_table(Tab_west1),
             path = "results/tables/table_R2_west.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))







# For Eastern Europe

# ICCs_east

ICCs_east<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  ICCs_east[i]<-
    import(paste0("results/east/",
                  DV.vars[i],"/mod0_DC.xlsx"))[4,"total"]
}
cbind(DV.vars,round_tidy(as.numeric(ICCs_east),2))

# covariates

COVs_east<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  COVs_east[i]<-
    import(paste0("results/east/",DV.vars[i],
                  "/mod1_R2.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(COVs_east),2))

# values

valueR2_east<-rep(NA,length(DV.vars))

for (i in 1:length(DV.vars)){
  valueR2_east[i]<-
    import(paste0("results/east/",DV.vars[i],
                  "/con_mod4_R2c_total.xlsx"))[1,"within"]
}
cbind(DV.vars,round_tidy(as.numeric(valueR2_east),2))

Tab_east<-
  cbind.data.frame(DV.vars,
                   ICC=round_tidy(as.numeric(ICCs_east),2),
                   COVR2=round_tidy(as.numeric(COVs_east),2),
                   valueR2_east=round_tidy(as.numeric(valueR2_east),2))
Tab_east$Variable<-
  case_when(
    Tab_east$DV.vars=="lrgen.z"~"Left-Right General",
    Tab_east$DV.vars=="lrecon.z"~"Left-Right Economic",
    Tab_east$DV.vars=="galtan.z"~"GAL-TAN",
    Tab_east$DV.vars=="spendvtax.z"~"Public services vs. reducing taxes",
    Tab_east$DV.vars=="deregulation.z"~"Deregulation of markets",
    Tab_east$DV.vars=="redistribution.z"~"Redistribution of wealth",
    Tab_east$DV.vars=="econ_interven.z"~"State intervention in the economy",
    Tab_east$DV.vars=="civlib_laworder.z"~"Civil liberties vs. law and order",
    Tab_east$DV.vars=="sociallifestyle.z"~"Social lifestyle policies",
    Tab_east$DV.vars=="religious_principle.z"~"Religious principles",
    Tab_east$DV.vars=="immigrate_policy.z"~"Immigration policy",
    Tab_east$DV.vars=="multiculturalism.z"~"Multiculturalism vs. assimilation",
    Tab_east$DV.vars=="urban_rural.z"~"Urban vs. rural interests",
    Tab_east$DV.vars=="environment.z"~"Environmental protection vs. economic growth",
    Tab_east$DV.vars=="regions.z"~"Decentralization to regions/localities",
    Tab_east$DV.vars=="international_security.z"~"International troop deployment",
    Tab_east$DV.vars=="ethnic_minorities.z"~"Rights of ethnic minorities",
    Tab_east$DV.vars=="nationalism.z"~"Cosmopolitanism vs. nationalism",
    Tab_east$DV.vars=="lrecon_salience.z"~"Left-Right Economic salience",
    Tab_east$DV.vars=="galtan_salience.z"~"GAL-TAN salience",
    Tab_east$DV.vars=="antielite_salience.z"~"Anti-elite",
    Tab_east$DV.vars=="corrupt_salience.z"~"Reducing corruption")

Tab_east1<-
  cbind.data.frame(
    Variable=Tab_east$Variable,
    ICC=substr(Tab_east$ICC,2,4),
    COV_R2=substr(Tab_east$COVR2,2,4),
    VALUE_R2=substr(Tab_east$valueR2_east,2,4))
Tab_east1

nice_table(Tab_east1)

save_as_docx(nice_table(Tab_east1),
             path = "results/tables/table_R2_east.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# combine moderator levels

Tab_mod1<-
  left_join(Tab_col1,
            Tab_nocol1,
            by="Variable")

Tab_mod1<-
  left_join(Tab_mod1,
            Tab_west1,
            by="Variable")

Tab_mod1<-
  left_join(Tab_mod1,
            Tab_east1,
            by="Variable")

names(Tab_mod1)
Tab_mod1<-
  Tab_mod1[,c("Variable","ICC.x","ICC.y","ICC.x.x","ICC.y.y",
              "COV_R2.x","COV_R2.y","COV_R2.x.x","COV_R2.y.y",
              "VALUE_R2.x","VALUE_R2.y","VALUE_R2.x.x","VALUE_R2.y.y")]







# save a separate collection of R2 values in a long format file

R2longtab<-
  data.frame(
    Sample=rep(c("All","College","No college",
                 "Western Europe","Post-communist"),each=22),
    Variable=rep(Tab1$Variable,times=5),
    ICC=as.numeric(c(ICCs,ICCs_col,ICCs_nocol,ICCs_west,ICCs_east)),
    COVR2=as.numeric(c(COVs,COVs_col,COVs_nocol,COVs_west,COVs_east)),
    VALUER2=as.numeric(c(valueR2,valueR2_col,valueR2_nocol,valueR2_west,valueR2_east)))


export(R2longtab,
       overwrite=T,"results/figures/figdata/R2longtab.xlsx")
