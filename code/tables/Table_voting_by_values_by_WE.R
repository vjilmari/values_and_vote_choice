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

# WE-interaction

# lrgen.z

lrgen_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  lrgen_WE.z.list[[i]]<-
    for.WE.figures(DV="lrgen.z",
                    IV=value.vars.c.gmc[i],
                    fp="results")
  
}


lrgen_WE.z.df<-do.call(rbind,lrgen_WE.z.list)
#lrgen_WE.z.df$value<-substr(lrgen_WE.z.df$rowname,1,3)
lrgen_WE.z.df$value<-value.names

Tab_lrgen_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(lrgen_WE.z.df[lrgen_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=lrgen_WE.z.df[lrgen_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(lrgen_WE.z.df[lrgen_WE.z.df$type=="Interaction","est"],2),
    p_int=lrgen_WE.z.df[lrgen_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(lrgen_WE.z.df[lrgen_WE.z.df$type=="Western Europe","est"],2),
    p_WE=lrgen_WE.z.df[lrgen_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(lrgen_WE.z.df[lrgen_WE.z.df$type=="Post-communist","est"],2),
    p_PC=lrgen_WE.z.df[lrgen_WE.z.df$type=="Post-communist","p"]
  )
Tab_lrgen_WE

nice_table(Tab_lrgen_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_lrgen_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_lrgen_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# lrecon.z

lrecon_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  lrecon_WE.z.list[[i]]<-
    for.WE.figures(DV="lrecon.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


lrecon_WE.z.df<-do.call(rbind,lrecon_WE.z.list)
#lrecon_WE.z.df$value<-substr(lrecon_WE.z.df$rowname,1,3)
lrecon_WE.z.df$value<-value.names

Tab_lrecon_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(lrecon_WE.z.df[lrecon_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=lrecon_WE.z.df[lrecon_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(lrecon_WE.z.df[lrecon_WE.z.df$type=="Interaction","est"],2),
    p_int=lrecon_WE.z.df[lrecon_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(lrecon_WE.z.df[lrecon_WE.z.df$type=="Western Europe","est"],2),
    p_WE=lrecon_WE.z.df[lrecon_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(lrecon_WE.z.df[lrecon_WE.z.df$type=="Post-communist","est"],2),
    p_PC=lrecon_WE.z.df[lrecon_WE.z.df$type=="Post-communist","p"]
  )
Tab_lrecon_WE

nice_table(Tab_lrecon_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_lrecon_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_lrecon_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# galtan.z

galtan_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  galtan_WE.z.list[[i]]<-
    for.WE.figures(DV="galtan.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


galtan_WE.z.df<-do.call(rbind,galtan_WE.z.list)
#galtan_WE.z.df$value<-substr(galtan_WE.z.df$rowname,1,3)
galtan_WE.z.df$value<-value.names

Tab_galtan_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(galtan_WE.z.df[galtan_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=galtan_WE.z.df[galtan_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(galtan_WE.z.df[galtan_WE.z.df$type=="Interaction","est"],2),
    p_int=galtan_WE.z.df[galtan_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(galtan_WE.z.df[galtan_WE.z.df$type=="Western Europe","est"],2),
    p_WE=galtan_WE.z.df[galtan_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(galtan_WE.z.df[galtan_WE.z.df$type=="Post-communist","est"],2),
    p_PC=galtan_WE.z.df[galtan_WE.z.df$type=="Post-communist","p"]
  )
Tab_galtan_WE

nice_table(Tab_galtan_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_galtan_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_galtan_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# spendvtax.z

spendvtax_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  spendvtax_WE.z.list[[i]]<-
    for.WE.figures(DV="spendvtax.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


spendvtax_WE.z.df<-do.call(rbind,spendvtax_WE.z.list)
#spendvtax_WE.z.df$value<-substr(spendvtax_WE.z.df$rowname,1,3)
spendvtax_WE.z.df$value<-value.names

Tab_spendvtax_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Interaction","est"],2),
    p_int=spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Western Europe","est"],2),
    p_WE=spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Post-communist","est"],2),
    p_PC=spendvtax_WE.z.df[spendvtax_WE.z.df$type=="Post-communist","p"]
  )
Tab_spendvtax_WE

nice_table(Tab_spendvtax_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_spendvtax_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_spendvtax_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# deregulation.z

deregulation_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  deregulation_WE.z.list[[i]]<-
    for.WE.figures(DV="deregulation.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


deregulation_WE.z.df<-do.call(rbind,deregulation_WE.z.list)
#deregulation_WE.z.df$value<-substr(deregulation_WE.z.df$rowname,1,3)
deregulation_WE.z.df$value<-value.names

Tab_deregulation_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(deregulation_WE.z.df[deregulation_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=deregulation_WE.z.df[deregulation_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(deregulation_WE.z.df[deregulation_WE.z.df$type=="Interaction","est"],2),
    p_int=deregulation_WE.z.df[deregulation_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(deregulation_WE.z.df[deregulation_WE.z.df$type=="Western Europe","est"],2),
    p_WE=deregulation_WE.z.df[deregulation_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(deregulation_WE.z.df[deregulation_WE.z.df$type=="Post-communist","est"],2),
    p_PC=deregulation_WE.z.df[deregulation_WE.z.df$type=="Post-communist","p"]
  )
Tab_deregulation_WE

nice_table(Tab_deregulation_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_deregulation_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_deregulation_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))


# redistribution.z

redistribution_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  redistribution_WE.z.list[[i]]<-
    for.WE.figures(DV="redistribution.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


redistribution_WE.z.df<-do.call(rbind,redistribution_WE.z.list)
#redistribution_WE.z.df$value<-substr(redistribution_WE.z.df$rowname,1,3)
redistribution_WE.z.df$value<-value.names

Tab_redistribution_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(redistribution_WE.z.df[redistribution_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=redistribution_WE.z.df[redistribution_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(redistribution_WE.z.df[redistribution_WE.z.df$type=="Interaction","est"],2),
    p_int=redistribution_WE.z.df[redistribution_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(redistribution_WE.z.df[redistribution_WE.z.df$type=="Western Europe","est"],2),
    p_WE=redistribution_WE.z.df[redistribution_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(redistribution_WE.z.df[redistribution_WE.z.df$type=="Post-communist","est"],2),
    p_PC=redistribution_WE.z.df[redistribution_WE.z.df$type=="Post-communist","p"]
  )
Tab_redistribution_WE

nice_table(Tab_redistribution_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_redistribution_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_redistribution_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# econ_interven.z

econ_interven_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  econ_interven_WE.z.list[[i]]<-
    for.WE.figures(DV="econ_interven.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


econ_interven_WE.z.df<-do.call(rbind,econ_interven_WE.z.list)
#econ_interven_WE.z.df$value<-substr(econ_interven_WE.z.df$rowname,1,3)
econ_interven_WE.z.df$value<-value.names

Tab_econ_interven_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Interaction","est"],2),
    p_int=econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Western Europe","est"],2),
    p_WE=econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Post-communist","est"],2),
    p_PC=econ_interven_WE.z.df[econ_interven_WE.z.df$type=="Post-communist","p"]
  )
Tab_econ_interven_WE

nice_table(Tab_econ_interven_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_econ_interven_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_econ_interven_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# civlib_laworder.z

civlib_laworder_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  civlib_laworder_WE.z.list[[i]]<-
    for.WE.figures(DV="civlib_laworder.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


civlib_laworder_WE.z.df<-do.call(rbind,civlib_laworder_WE.z.list)
#civlib_laworder_WE.z.df$value<-substr(civlib_laworder_WE.z.df$rowname,1,3)
civlib_laworder_WE.z.df$value<-value.names

Tab_civlib_laworder_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Interaction","est"],2),
    p_int=civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Western Europe","est"],2),
    p_WE=civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Post-communist","est"],2),
    p_PC=civlib_laworder_WE.z.df[civlib_laworder_WE.z.df$type=="Post-communist","p"]
  )
Tab_civlib_laworder_WE

nice_table(Tab_civlib_laworder_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_civlib_laworder_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_civlib_laworder_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# sociallifestyle.z

sociallifestyle_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  sociallifestyle_WE.z.list[[i]]<-
    for.WE.figures(DV="sociallifestyle.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


sociallifestyle_WE.z.df<-do.call(rbind,sociallifestyle_WE.z.list)
#sociallifestyle_WE.z.df$value<-substr(sociallifestyle_WE.z.df$rowname,1,3)
sociallifestyle_WE.z.df$value<-value.names

Tab_sociallifestyle_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Interaction","est"],2),
    p_int=sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Western Europe","est"],2),
    p_WE=sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Post-communist","est"],2),
    p_PC=sociallifestyle_WE.z.df[sociallifestyle_WE.z.df$type=="Post-communist","p"]
  )
Tab_sociallifestyle_WE

nice_table(Tab_sociallifestyle_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_sociallifestyle_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_sociallifestyle_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# religious_principle.z

religious_principle_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  religious_principle_WE.z.list[[i]]<-
    for.WE.figures(DV="religious_principle.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


religious_principle_WE.z.df<-do.call(rbind,religious_principle_WE.z.list)
#religious_principle_WE.z.df$value<-substr(religious_principle_WE.z.df$rowname,1,3)
religious_principle_WE.z.df$value<-value.names

Tab_religious_principle_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Interaction","est"],2),
    p_int=religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Western Europe","est"],2),
    p_WE=religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Post-communist","est"],2),
    p_PC=religious_principle_WE.z.df[religious_principle_WE.z.df$type=="Post-communist","p"]
  )
Tab_religious_principle_WE

nice_table(Tab_religious_principle_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_religious_principle_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_religious_principle_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# immigrate_policy.z

immigrate_policy_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  immigrate_policy_WE.z.list[[i]]<-
    for.WE.figures(DV="immigrate_policy.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


immigrate_policy_WE.z.df<-do.call(rbind,immigrate_policy_WE.z.list)
#immigrate_policy_WE.z.df$value<-substr(immigrate_policy_WE.z.df$rowname,1,3)
immigrate_policy_WE.z.df$value<-value.names

Tab_immigrate_policy_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Interaction","est"],2),
    p_int=immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Western Europe","est"],2),
    p_WE=immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Post-communist","est"],2),
    p_PC=immigrate_policy_WE.z.df[immigrate_policy_WE.z.df$type=="Post-communist","p"]
  )
Tab_immigrate_policy_WE

nice_table(Tab_immigrate_policy_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_immigrate_policy_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_immigrate_policy_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# multiculturalism.z

multiculturalism_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  multiculturalism_WE.z.list[[i]]<-
    for.WE.figures(DV="multiculturalism.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


multiculturalism_WE.z.df<-do.call(rbind,multiculturalism_WE.z.list)
#multiculturalism_WE.z.df$value<-substr(multiculturalism_WE.z.df$rowname,1,3)
multiculturalism_WE.z.df$value<-value.names

Tab_multiculturalism_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Interaction","est"],2),
    p_int=multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Western Europe","est"],2),
    p_WE=multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Post-communist","est"],2),
    p_PC=multiculturalism_WE.z.df[multiculturalism_WE.z.df$type=="Post-communist","p"]
  )
Tab_multiculturalism_WE

nice_table(Tab_multiculturalism_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_multiculturalism_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_multiculturalism_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# urban_rural.z

urban_rural_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  urban_rural_WE.z.list[[i]]<-
    for.WE.figures(DV="urban_rural.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


urban_rural_WE.z.df<-do.call(rbind,urban_rural_WE.z.list)
#urban_rural_WE.z.df$value<-substr(urban_rural_WE.z.df$rowname,1,3)
urban_rural_WE.z.df$value<-value.names

Tab_urban_rural_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Interaction","est"],2),
    p_int=urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Western Europe","est"],2),
    p_WE=urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Post-communist","est"],2),
    p_PC=urban_rural_WE.z.df[urban_rural_WE.z.df$type=="Post-communist","p"]
  )
Tab_urban_rural_WE

nice_table(Tab_urban_rural_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_urban_rural_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_urban_rural_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# environment.z

environment_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  environment_WE.z.list[[i]]<-
    for.WE.figures(DV="environment.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


environment_WE.z.df<-do.call(rbind,environment_WE.z.list)
#environment_WE.z.df$value<-substr(environment_WE.z.df$rowname,1,3)
environment_WE.z.df$value<-value.names

Tab_environment_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(environment_WE.z.df[environment_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=environment_WE.z.df[environment_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(environment_WE.z.df[environment_WE.z.df$type=="Interaction","est"],2),
    p_int=environment_WE.z.df[environment_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(environment_WE.z.df[environment_WE.z.df$type=="Western Europe","est"],2),
    p_WE=environment_WE.z.df[environment_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(environment_WE.z.df[environment_WE.z.df$type=="Post-communist","est"],2),
    p_PC=environment_WE.z.df[environment_WE.z.df$type=="Post-communist","p"]
  )
Tab_environment_WE

nice_table(Tab_environment_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_environment_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_environment_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# regions.z

regions_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  regions_WE.z.list[[i]]<-
    for.WE.figures(DV="regions.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


regions_WE.z.df<-do.call(rbind,regions_WE.z.list)
#regions_WE.z.df$value<-substr(regions_WE.z.df$rowname,1,3)
regions_WE.z.df$value<-value.names

Tab_regions_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(regions_WE.z.df[regions_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=regions_WE.z.df[regions_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(regions_WE.z.df[regions_WE.z.df$type=="Interaction","est"],2),
    p_int=regions_WE.z.df[regions_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(regions_WE.z.df[regions_WE.z.df$type=="Western Europe","est"],2),
    p_WE=regions_WE.z.df[regions_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(regions_WE.z.df[regions_WE.z.df$type=="Post-communist","est"],2),
    p_PC=regions_WE.z.df[regions_WE.z.df$type=="Post-communist","p"]
  )
Tab_regions_WE

nice_table(Tab_regions_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_regions_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_regions_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# international_security.z

international_security_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  international_security_WE.z.list[[i]]<-
    for.WE.figures(DV="international_security.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


international_security_WE.z.df<-do.call(rbind,international_security_WE.z.list)
#international_security_WE.z.df$value<-substr(international_security_WE.z.df$rowname,1,3)
international_security_WE.z.df$value<-value.names

Tab_international_security_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(international_security_WE.z.df[international_security_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=international_security_WE.z.df[international_security_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(international_security_WE.z.df[international_security_WE.z.df$type=="Interaction","est"],2),
    p_int=international_security_WE.z.df[international_security_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(international_security_WE.z.df[international_security_WE.z.df$type=="Western Europe","est"],2),
    p_WE=international_security_WE.z.df[international_security_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(international_security_WE.z.df[international_security_WE.z.df$type=="Post-communist","est"],2),
    p_PC=international_security_WE.z.df[international_security_WE.z.df$type=="Post-communist","p"]
  )
Tab_international_security_WE

nice_table(Tab_international_security_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_international_security_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_international_security_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# ethnic_minorities.z

ethnic_minorities_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  ethnic_minorities_WE.z.list[[i]]<-
    for.WE.figures(DV="ethnic_minorities.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


ethnic_minorities_WE.z.df<-do.call(rbind,ethnic_minorities_WE.z.list)
#ethnic_minorities_WE.z.df$value<-substr(ethnic_minorities_WE.z.df$rowname,1,3)
ethnic_minorities_WE.z.df$value<-value.names

Tab_ethnic_minorities_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Interaction","est"],2),
    p_int=ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Western Europe","est"],2),
    p_WE=ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Post-communist","est"],2),
    p_PC=ethnic_minorities_WE.z.df[ethnic_minorities_WE.z.df$type=="Post-communist","p"]
  )
Tab_ethnic_minorities_WE

nice_table(Tab_ethnic_minorities_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_ethnic_minorities_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_ethnic_minorities_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# nationalism.z

nationalism_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  nationalism_WE.z.list[[i]]<-
    for.WE.figures(DV="nationalism.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


nationalism_WE.z.df<-do.call(rbind,nationalism_WE.z.list)
#nationalism_WE.z.df$value<-substr(nationalism_WE.z.df$rowname,1,3)
nationalism_WE.z.df$value<-value.names

Tab_nationalism_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(nationalism_WE.z.df[nationalism_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=nationalism_WE.z.df[nationalism_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(nationalism_WE.z.df[nationalism_WE.z.df$type=="Interaction","est"],2),
    p_int=nationalism_WE.z.df[nationalism_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(nationalism_WE.z.df[nationalism_WE.z.df$type=="Western Europe","est"],2),
    p_WE=nationalism_WE.z.df[nationalism_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(nationalism_WE.z.df[nationalism_WE.z.df$type=="Post-communist","est"],2),
    p_PC=nationalism_WE.z.df[nationalism_WE.z.df$type=="Post-communist","p"]
  )
Tab_nationalism_WE

nice_table(Tab_nationalism_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_nationalism_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_nationalism_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# antielite_salience.z

antielite_salience_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  antielite_salience_WE.z.list[[i]]<-
    for.WE.figures(DV="antielite_salience.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


antielite_salience_WE.z.df<-do.call(rbind,antielite_salience_WE.z.list)
#antielite_salience_WE.z.df$value<-substr(antielite_salience_WE.z.df$rowname,1,3)
antielite_salience_WE.z.df$value<-value.names

Tab_antielite_salience_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Interaction","est"],2),
    p_int=antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Western Europe","est"],2),
    p_WE=antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Post-communist","est"],2),
    p_PC=antielite_salience_WE.z.df[antielite_salience_WE.z.df$type=="Post-communist","p"]
  )
Tab_antielite_salience_WE

nice_table(Tab_antielite_salience_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_antielite_salience_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_antielite_salience_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

# corrupt_salience.z

corrupt_salience_WE.z.list<-list()

for (i in 1:length(value.vars.c.gmc)){
  corrupt_salience_WE.z.list[[i]]<-
    for.WE.figures(DV="corrupt_salience.z",
                   IV=value.vars.c.gmc[i],
                   fp="results")
  
}


corrupt_salience_WE.z.df<-do.call(rbind,corrupt_salience_WE.z.list)
#corrupt_salience_WE.z.df$value<-substr(corrupt_salience_WE.z.df$rowname,1,3)
corrupt_salience_WE.z.df$value<-value.names

Tab_corrupt_salience_WE<-
  cbind.data.frame(
    Value=value.names,
    "Slope SD"=round_tidy(corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Interaction","slope_sd"],2),
    p_slope=corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Interaction","slope_p"],
    Interaction=round_tidy(corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Interaction","est"],2),
    p_int=corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Interaction","p"],
    WE=round_tidy(corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Western Europe","est"],2),
    p_WE=corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Western Europe","p"],
    PC=round_tidy(corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Post-communist","est"],2),
    p_PC=corrupt_salience_WE.z.df[corrupt_salience_WE.z.df$type=="Post-communist","p"]
  )
Tab_corrupt_salience_WE

nice_table(Tab_corrupt_salience_WE,
           col.format.p = c(3,5,7,9))

save_as_docx(nice_table(Tab_corrupt_salience_WE,
                        col.format.p = c(3,5,7,9)),
             path = "results/tables/table_corrupt_salience_WE_int.docx",
             pr_section=prop_section(
               page_size = page_size(width = 8.3, height = 11.7)))

