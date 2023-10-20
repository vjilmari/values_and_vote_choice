# Table for education interactions

source("code/custom_functions.R")
library(rio)
library(finalfit)
library(dplyr)
library(rempsyc)
library(flextable)
library(officer)
library(tibble)

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


# lrgen.z

lrgen.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  lrgen.z.list[[i]]<-
    for.edu.figures(DV="lrgen.z",
                     IV=value.vars.c.gmc[i],
                     fp="results")
  
}


lrgen.z.df<-do.call(rbind,lrgen.z.list)
#lrgen.z.df$value<-substr(lrgen.z.df$rowname,1,3)
lrgen.z.df$value<-value.names
lrgen.z.df


Tab_lrgen<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(lrgen.z.df[lrgen.z.df$type=="Interaction","est"],2),
    p_int=lrgen.z.df[lrgen.z.df$type=="Interaction","p"],
    College=round_tidy(lrgen.z.df[lrgen.z.df$type=="College degree","est"],2),
    p_col=lrgen.z.df[lrgen.z.df$type=="College degree","p"],
    'No college'=round_tidy(lrgen.z.df[lrgen.z.df$type=="No college degree","est"],2),
    p_nocol=lrgen.z.df[lrgen.z.df$type=="No college degree","p"]
    )

nice_table(Tab_lrgen,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_lrgen,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_lrgen_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# lrecon.z

lrecon.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  lrecon.z.list[[i]]<-
    for.edu.figures(DV="lrecon.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


lrecon.z.df<-do.call(rbind,lrecon.z.list)
#lrecon.z.df$value<-substr(lrecon.z.df$rowname,1,3)
lrecon.z.df$value<-value.names

Tab_lrecon<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(lrecon.z.df[lrecon.z.df$type=="Interaction","est"],2),
    p_int=lrecon.z.df[lrecon.z.df$type=="Interaction","p"],
    College=round_tidy(lrecon.z.df[lrecon.z.df$type=="College degree","est"],2),
    p_col=lrecon.z.df[lrecon.z.df$type=="College degree","p"],
    'No college'=round_tidy(lrecon.z.df[lrecon.z.df$type=="No college degree","est"],2),
    p_nocol=lrecon.z.df[lrecon.z.df$type=="No college degree","p"]
  )

nice_table(Tab_lrecon,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_lrecon,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_lrecon_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# galtan.z

galtan.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  galtan.z.list[[i]]<-
    for.edu.figures(DV="galtan.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


galtan.z.df<-do.call(rbind,galtan.z.list)
#galtan.z.df$value<-substr(galtan.z.df$rowname,1,3)
galtan.z.df$value<-value.names

Tab_galtan<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(galtan.z.df[galtan.z.df$type=="Interaction","est"],2),
    p_int=galtan.z.df[galtan.z.df$type=="Interaction","p"],
    College=round_tidy(galtan.z.df[galtan.z.df$type=="College degree","est"],2),
    p_col=galtan.z.df[galtan.z.df$type=="College degree","p"],
    'No college'=round_tidy(galtan.z.df[galtan.z.df$type=="No college degree","est"],2),
    p_nocol=galtan.z.df[galtan.z.df$type=="No college degree","p"]
  )

nice_table(Tab_galtan,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_galtan,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_galtan_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))



# spendvtax.z

spendvtax.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  spendvtax.z.list[[i]]<-
    for.edu.figures(DV="spendvtax.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}

spendvtax.z.df<-do.call(rbind,spendvtax.z.list)
#spendvtax.z.df$value<-substr(spendvtax.z.df$rowname,1,3)
spendvtax.z.df$value<-value.names

Tab_spendvtax<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(spendvtax.z.df[spendvtax.z.df$type=="Interaction","est"],2),
    p_int=spendvtax.z.df[spendvtax.z.df$type=="Interaction","p"],
    College=round_tidy(spendvtax.z.df[spendvtax.z.df$type=="College degree","est"],2),
    p_col=spendvtax.z.df[spendvtax.z.df$type=="College degree","p"],
    'No college'=round_tidy(spendvtax.z.df[spendvtax.z.df$type=="No college degree","est"],2),
    p_nocol=spendvtax.z.df[spendvtax.z.df$type=="No college degree","p"]
  )

nice_table(Tab_spendvtax,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_spendvtax,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_spendvtax_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# deregulation.z

deregulation.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  deregulation.z.list[[i]]<-
    for.edu.figures(DV="deregulation.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


deregulation.z.df<-do.call(rbind,deregulation.z.list)
#deregulation.z.df$value<-substr(deregulation.z.df$rowname,1,3)
deregulation.z.df$value<-value.names

Tab_deregulation<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(deregulation.z.df[deregulation.z.df$type=="Interaction","est"],2),
    p_int=deregulation.z.df[deregulation.z.df$type=="Interaction","p"],
    College=round_tidy(deregulation.z.df[deregulation.z.df$type=="College degree","est"],2),
    p_col=deregulation.z.df[deregulation.z.df$type=="College degree","p"],
    'No college'=round_tidy(deregulation.z.df[deregulation.z.df$type=="No college degree","est"],2),
    p_nocol=deregulation.z.df[deregulation.z.df$type=="No college degree","p"]
  )

nice_table(Tab_deregulation,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_deregulation,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_deregulation_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# redistribution.z

redistribution.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  redistribution.z.list[[i]]<-
    for.edu.figures(DV="redistribution.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


redistribution.z.df<-do.call(rbind,redistribution.z.list)
#redistribution.z.df$value<-substr(redistribution.z.df$rowname,1,3)
redistribution.z.df$value<-value.names

Tab_redistribution<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(redistribution.z.df[redistribution.z.df$type=="Interaction","est"],2),
    p_int=redistribution.z.df[redistribution.z.df$type=="Interaction","p"],
    College=round_tidy(redistribution.z.df[redistribution.z.df$type=="College degree","est"],2),
    p_col=redistribution.z.df[redistribution.z.df$type=="College degree","p"],
    'No college'=round_tidy(redistribution.z.df[redistribution.z.df$type=="No college degree","est"],2),
    p_nocol=redistribution.z.df[redistribution.z.df$type=="No college degree","p"]
  )

nice_table(Tab_redistribution,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_redistribution,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_redistribution_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# econ_interven.z

econ_interven.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  econ_interven.z.list[[i]]<-
    for.edu.figures(DV="econ_interven.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


econ_interven.z.df<-do.call(rbind,econ_interven.z.list)
#econ_interven.z.df$value<-substr(econ_interven.z.df$rowname,1,3)
econ_interven.z.df$value<-value.names

Tab_econ_interven<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(econ_interven.z.df[econ_interven.z.df$type=="Interaction","est"],2),
    p_int=econ_interven.z.df[econ_interven.z.df$type=="Interaction","p"],
    College=round_tidy(econ_interven.z.df[econ_interven.z.df$type=="College degree","est"],2),
    p_col=econ_interven.z.df[econ_interven.z.df$type=="College degree","p"],
    'No college'=round_tidy(econ_interven.z.df[econ_interven.z.df$type=="No college degree","est"],2),
    p_nocol=econ_interven.z.df[econ_interven.z.df$type=="No college degree","p"]
  )

nice_table(Tab_econ_interven,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_econ_interven,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_econ_interven_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# civlib_laworder.z

civlib_laworder.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  civlib_laworder.z.list[[i]]<-
    for.edu.figures(DV="civlib_laworder.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


civlib_laworder.z.df<-do.call(rbind,civlib_laworder.z.list)
#civlib_laworder.z.df$value<-substr(civlib_laworder.z.df$rowname,1,3)
civlib_laworder.z.df$value<-value.names

Tab_civlib_laworder<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(civlib_laworder.z.df[civlib_laworder.z.df$type=="Interaction","est"],2),
    p_int=civlib_laworder.z.df[civlib_laworder.z.df$type=="Interaction","p"],
    College=round_tidy(civlib_laworder.z.df[civlib_laworder.z.df$type=="College degree","est"],2),
    p_col=civlib_laworder.z.df[civlib_laworder.z.df$type=="College degree","p"],
    'No college'=round_tidy(civlib_laworder.z.df[civlib_laworder.z.df$type=="No college degree","est"],2),
    p_nocol=civlib_laworder.z.df[civlib_laworder.z.df$type=="No college degree","p"]
  )

nice_table(Tab_civlib_laworder,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_civlib_laworder,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_civlib_laworder_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# sociallifestyle.z

sociallifestyle.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  sociallifestyle.z.list[[i]]<-
    for.edu.figures(DV="sociallifestyle.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


sociallifestyle.z.df<-do.call(rbind,sociallifestyle.z.list)
#sociallifestyle.z.df$value<-substr(sociallifestyle.z.df$rowname,1,3)
sociallifestyle.z.df$value<-value.names

Tab_sociallifestyle<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(sociallifestyle.z.df[sociallifestyle.z.df$type=="Interaction","est"],2),
    p_int=sociallifestyle.z.df[sociallifestyle.z.df$type=="Interaction","p"],
    College=round_tidy(sociallifestyle.z.df[sociallifestyle.z.df$type=="College degree","est"],2),
    p_col=sociallifestyle.z.df[sociallifestyle.z.df$type=="College degree","p"],
    'No college'=round_tidy(sociallifestyle.z.df[sociallifestyle.z.df$type=="No college degree","est"],2),
    p_nocol=sociallifestyle.z.df[sociallifestyle.z.df$type=="No college degree","p"]
  )

nice_table(Tab_sociallifestyle,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_sociallifestyle,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_sociallifestyle_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# religious_principle.z

religious_principle.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  religious_principle.z.list[[i]]<-
    for.edu.figures(DV="religious_principle.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


religious_principle.z.df<-do.call(rbind,religious_principle.z.list)
#religious_principle.z.df$value<-substr(religious_principle.z.df$rowname,1,3)
religious_principle.z.df$value<-value.names

Tab_religious_principle<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(religious_principle.z.df[religious_principle.z.df$type=="Interaction","est"],2),
    p_int=religious_principle.z.df[religious_principle.z.df$type=="Interaction","p"],
    College=round_tidy(religious_principle.z.df[religious_principle.z.df$type=="College degree","est"],2),
    p_col=religious_principle.z.df[religious_principle.z.df$type=="College degree","p"],
    'No college'=round_tidy(religious_principle.z.df[religious_principle.z.df$type=="No college degree","est"],2),
    p_nocol=religious_principle.z.df[religious_principle.z.df$type=="No college degree","p"]
  )

nice_table(Tab_religious_principle,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_religious_principle,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_religious_principle_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# immigrate_policy.z

immigrate_policy.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  immigrate_policy.z.list[[i]]<-
    for.edu.figures(DV="immigrate_policy.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


immigrate_policy.z.df<-do.call(rbind,immigrate_policy.z.list)
#immigrate_policy.z.df$value<-substr(immigrate_policy.z.df$rowname,1,3)
immigrate_policy.z.df$value<-value.names

Tab_immigrate_policy<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(immigrate_policy.z.df[immigrate_policy.z.df$type=="Interaction","est"],2),
    p_int=immigrate_policy.z.df[immigrate_policy.z.df$type=="Interaction","p"],
    College=round_tidy(immigrate_policy.z.df[immigrate_policy.z.df$type=="College degree","est"],2),
    p_col=immigrate_policy.z.df[immigrate_policy.z.df$type=="College degree","p"],
    'No college'=round_tidy(immigrate_policy.z.df[immigrate_policy.z.df$type=="No college degree","est"],2),
    p_nocol=immigrate_policy.z.df[immigrate_policy.z.df$type=="No college degree","p"]
  )

nice_table(Tab_immigrate_policy,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_immigrate_policy,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_immigrate_policy_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# multiculturalism.z

multiculturalism.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  multiculturalism.z.list[[i]]<-
    for.edu.figures(DV="multiculturalism.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


multiculturalism.z.df<-do.call(rbind,multiculturalism.z.list)
#multiculturalism.z.df$value<-substr(multiculturalism.z.df$rowname,1,3)
multiculturalism.z.df$value<-value.names

Tab_multiculturalism<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(multiculturalism.z.df[multiculturalism.z.df$type=="Interaction","est"],2),
    p_int=multiculturalism.z.df[multiculturalism.z.df$type=="Interaction","p"],
    College=round_tidy(multiculturalism.z.df[multiculturalism.z.df$type=="College degree","est"],2),
    p_col=multiculturalism.z.df[multiculturalism.z.df$type=="College degree","p"],
    'No college'=round_tidy(multiculturalism.z.df[multiculturalism.z.df$type=="No college degree","est"],2),
    p_nocol=multiculturalism.z.df[multiculturalism.z.df$type=="No college degree","p"]
  )

nice_table(Tab_multiculturalism,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_multiculturalism,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_multiculturalism_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# urban_rural.z

urban_rural.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  urban_rural.z.list[[i]]<-
    for.edu.figures(DV="urban_rural.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


urban_rural.z.df<-do.call(rbind,urban_rural.z.list)
#urban_rural.z.df$value<-substr(urban_rural.z.df$rowname,1,3)
urban_rural.z.df$value<-value.names

Tab_urban_rural<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(urban_rural.z.df[urban_rural.z.df$type=="Interaction","est"],2),
    p_int=urban_rural.z.df[urban_rural.z.df$type=="Interaction","p"],
    College=round_tidy(urban_rural.z.df[urban_rural.z.df$type=="College degree","est"],2),
    p_col=urban_rural.z.df[urban_rural.z.df$type=="College degree","p"],
    'No college'=round_tidy(urban_rural.z.df[urban_rural.z.df$type=="No college degree","est"],2),
    p_nocol=urban_rural.z.df[urban_rural.z.df$type=="No college degree","p"]
  )

nice_table(Tab_urban_rural,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_urban_rural,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_urban_rural_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# environment.z

environment.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  environment.z.list[[i]]<-
    for.edu.figures(DV="environment.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


environment.z.df<-do.call(rbind,environment.z.list)
#environment.z.df$value<-substr(environment.z.df$rowname,1,3)
environment.z.df$value<-value.names

Tab_environment<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(environment.z.df[environment.z.df$type=="Interaction","est"],2),
    p_int=environment.z.df[environment.z.df$type=="Interaction","p"],
    College=round_tidy(environment.z.df[environment.z.df$type=="College degree","est"],2),
    p_col=environment.z.df[environment.z.df$type=="College degree","p"],
    'No college'=round_tidy(environment.z.df[environment.z.df$type=="No college degree","est"],2),
    p_nocol=environment.z.df[environment.z.df$type=="No college degree","p"]
  )

nice_table(Tab_environment,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_environment,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_environment_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# regions.z

regions.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  regions.z.list[[i]]<-
    for.edu.figures(DV="regions.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


regions.z.df<-do.call(rbind,regions.z.list)
#regions.z.df$value<-substr(regions.z.df$rowname,1,3)
regions.z.df$value<-value.names

Tab_regions<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(regions.z.df[regions.z.df$type=="Interaction","est"],2),
    p_int=regions.z.df[regions.z.df$type=="Interaction","p"],
    College=round_tidy(regions.z.df[regions.z.df$type=="College degree","est"],2),
    p_col=regions.z.df[regions.z.df$type=="College degree","p"],
    'No college'=round_tidy(regions.z.df[regions.z.df$type=="No college degree","est"],2),
    p_nocol=regions.z.df[regions.z.df$type=="No college degree","p"]
  )

nice_table(Tab_regions,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_regions,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_regions_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))



# international_security.z

international_security.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  international_security.z.list[[i]]<-
    for.edu.figures(DV="international_security.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


international_security.z.df<-do.call(rbind,international_security.z.list)
#international_security.z.df$value<-substr(international_security.z.df$rowname,1,3)
international_security.z.df$value<-value.names

Tab_international_security<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(international_security.z.df[international_security.z.df$type=="Interaction","est"],2),
    p_int=international_security.z.df[international_security.z.df$type=="Interaction","p"],
    College=round_tidy(international_security.z.df[international_security.z.df$type=="College degree","est"],2),
    p_col=international_security.z.df[international_security.z.df$type=="College degree","p"],
    'No college'=round_tidy(international_security.z.df[international_security.z.df$type=="No college degree","est"],2),
    p_nocol=international_security.z.df[international_security.z.df$type=="No college degree","p"]
  )

nice_table(Tab_international_security,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_international_security,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_international_security_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# ethnic_minorities.z

ethnic_minorities.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  ethnic_minorities.z.list[[i]]<-
    for.edu.figures(DV="ethnic_minorities.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


ethnic_minorities.z.df<-do.call(rbind,ethnic_minorities.z.list)
#ethnic_minorities.z.df$value<-substr(ethnic_minorities.z.df$rowname,1,3)
ethnic_minorities.z.df$value<-value.names

Tab_ethnic_minorities<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(ethnic_minorities.z.df[ethnic_minorities.z.df$type=="Interaction","est"],2),
    p_int=ethnic_minorities.z.df[ethnic_minorities.z.df$type=="Interaction","p"],
    College=round_tidy(ethnic_minorities.z.df[ethnic_minorities.z.df$type=="College degree","est"],2),
    p_col=ethnic_minorities.z.df[ethnic_minorities.z.df$type=="College degree","p"],
    'No college'=round_tidy(ethnic_minorities.z.df[ethnic_minorities.z.df$type=="No college degree","est"],2),
    p_nocol=ethnic_minorities.z.df[ethnic_minorities.z.df$type=="No college degree","p"]
  )

nice_table(Tab_ethnic_minorities,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_ethnic_minorities,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_ethnic_minorities_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# nationalism.z

nationalism.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  nationalism.z.list[[i]]<-
    for.edu.figures(DV="nationalism.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


nationalism.z.df<-do.call(rbind,nationalism.z.list)
#nationalism.z.df$value<-substr(nationalism.z.df$rowname,1,3)
nationalism.z.df$value<-value.names

Tab_nationalism<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(nationalism.z.df[nationalism.z.df$type=="Interaction","est"],2),
    p_int=nationalism.z.df[nationalism.z.df$type=="Interaction","p"],
    College=round_tidy(nationalism.z.df[nationalism.z.df$type=="College degree","est"],2),
    p_col=nationalism.z.df[nationalism.z.df$type=="College degree","p"],
    'No college'=round_tidy(nationalism.z.df[nationalism.z.df$type=="No college degree","est"],2),
    p_nocol=nationalism.z.df[nationalism.z.df$type=="No college degree","p"]
  )

nice_table(Tab_nationalism,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_nationalism,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_nationalism_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# antielite_salience.z

antielite_salience.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  antielite_salience.z.list[[i]]<-
    for.edu.figures(DV="antielite_salience.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


antielite_salience.z.df<-do.call(rbind,antielite_salience.z.list)
#antielite_salience.z.df$value<-substr(antielite_salience.z.df$rowname,1,3)
antielite_salience.z.df$value<-value.names

Tab_antielite_salience<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(antielite_salience.z.df[antielite_salience.z.df$type=="Interaction","est"],2),
    p_int=antielite_salience.z.df[antielite_salience.z.df$type=="Interaction","p"],
    College=round_tidy(antielite_salience.z.df[antielite_salience.z.df$type=="College degree","est"],2),
    p_col=antielite_salience.z.df[antielite_salience.z.df$type=="College degree","p"],
    'No college'=round_tidy(antielite_salience.z.df[antielite_salience.z.df$type=="No college degree","est"],2),
    p_nocol=antielite_salience.z.df[antielite_salience.z.df$type=="No college degree","p"]
  )

nice_table(Tab_antielite_salience,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_antielite_salience,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_antielite_salience_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# corrupt_salience.z

corrupt_salience.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  corrupt_salience.z.list[[i]]<-
    for.edu.figures(DV="corrupt_salience.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


corrupt_salience.z.df<-do.call(rbind,corrupt_salience.z.list)
#corrupt_salience.z.df$value<-substr(corrupt_salience.z.df$rowname,1,3)
corrupt_salience.z.df$value<-value.names

Tab_corrupt_salience<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(corrupt_salience.z.df[corrupt_salience.z.df$type=="Interaction","est"],2),
    p_int=corrupt_salience.z.df[corrupt_salience.z.df$type=="Interaction","p"],
    College=round_tidy(corrupt_salience.z.df[corrupt_salience.z.df$type=="College degree","est"],2),
    p_col=corrupt_salience.z.df[corrupt_salience.z.df$type=="College degree","p"],
    'No college'=round_tidy(corrupt_salience.z.df[corrupt_salience.z.df$type=="No college degree","est"],2),
    p_nocol=corrupt_salience.z.df[corrupt_salience.z.df$type=="No college degree","p"]
  )

nice_table(Tab_corrupt_salience,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_corrupt_salience,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_corrupt_salience_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# urban_rural.z

urban_rural.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  urban_rural.z.list[[i]]<-
    for.edu.figures(DV="urban_rural.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


urban_rural.z.df<-do.call(rbind,urban_rural.z.list)
#urban_rural.z.df$value<-substr(urban_rural.z.df$rowname,1,3)
urban_rural.z.df$value<-value.names

Tab_urban_rural<-
  cbind.data.frame(
    Value=value.names,
    Interaction=round_tidy(urban_rural.z.df[urban_rural.z.df$type=="Interaction","est"],2),
    p_int=urban_rural.z.df[urban_rural.z.df$type=="Interaction","p"],
    College=round_tidy(urban_rural.z.df[urban_rural.z.df$type=="College degree","est"],2),
    p_col=urban_rural.z.df[urban_rural.z.df$type=="College degree","p"],
    'No college'=round_tidy(urban_rural.z.df[urban_rural.z.df$type=="No college degree","est"],2),
    p_nocol=urban_rural.z.df[urban_rural.z.df$type=="No college degree","p"]
  )

nice_table(Tab_urban_rural,
           col.format.p = c(3,5,7))

save_as_docx(nice_table(Tab_urban_rural,
                        col.format.p = c(3,5,7)),
             path = "results/tables/table_urban_rural_edu_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

