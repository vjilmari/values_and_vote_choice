get.ESS.label<-function(var){
  attr(var,which = "label")
}



vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}



# make a separate function to obtain nice data frame from r2mlm

## decomposition

DE.frame<-function(model){
  
  s<-r2mlm(model,bargraph = F)[[1]]
  srn<-rownames(s)
  scn<-colnames(s)
  
  attr(s, "dimnames") <- NULL
  
  d<-data.frame(matrix(as.vector(s),
                       nrow=dim(s)[1],
                       ncol=dim(s)[2],
                       byrow=F))
  colnames(d)<-scn
  rownames(d)<-srn
  return(d)
  
}

## effect size

R2.frame<-function(model){
  
  s<-r2mlm(model,bargraph = F)[[2]]
  srn<-rownames(s)
  scn<-colnames(s)
  
  attr(s, "dimnames") <- NULL
  
  d<-data.frame(matrix(as.vector(s),
                       nrow=dim(s)[1],
                       ncol=dim(s)[2],
                       byrow=F))
  colnames(d)<-scn
  rownames(d)<-srn
  return(d)
  
}


## effect size change

R2c.frame<-function(modelA,modelB){
  
  s<-r2mlm_comp(modelA,modelB,bargraph = F)[[3]]
  srn<-rownames(s)
  scn<-colnames(s)
  
  attr(s, "dimnames") <- NULL
  
  d<-data.frame(matrix(as.vector(s),
                       nrow=dim(s)[1],
                       ncol=dim(s)[2],
                       byrow=F))
  colnames(d)<-scn
  rownames(d)<-srn
  return(d)
  
}


DV_by_values_pipe<-function(DV,IV,IV.c,directory,data){
  
  #DV=DV.vars[1]
  #IV=value.vars
  #IV.c=value.vars.c
  #directory="results/main"
  #data=fdat
  
  dir.create(path = paste0(directory,"/",DV))
  
  dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- data %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               "gndr.c",
                               "age10.c"))
  
  # fit DV-only model
  # define the formula
  mod0.f<-
    as.formula(paste0(DV,"~","(1|cntry)"))
  
  ## fit the model
  mod0<-lmer(mod0.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(data.frame(summary(mod0)$coefficients)),
         paste0(dir.temp,"mod0_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod0,round = 12),
         paste0(dir.temp,"mod0_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod0)),
         paste0(dir.temp,"mod0_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod0)),
         paste0(dir.temp,"mod0_R2.xlsx"),overwrite=T)
  
  # fit covariates-only model
  # define the formula
  mod1.f<-
    as.formula(paste0(DV,"~","gndr.c.gmc+age10.c.gmc+","(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod1,round=12,p.round=12)),
         paste0(dir.temp,"mod1_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1,round = 12),
         paste0(dir.temp,"mod1_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod1)),
         paste0(dir.temp,"mod1_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod1)),
         paste0(dir.temp,"mod1_R2.xlsx"),overwrite=T)
  
  # Loop through each IV (and) IV.c
  
  for (i in 1:length(IV)){
    #i=4
    IV.temp<-paste0(IV[i],".gmc")
    IV.c.temp<-paste0(IV.c[i],".gmc")
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        "(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod2,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod2_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod2,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod2_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod1,mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2c.xlsx")),overwrite=T)
    
    # formula for FE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod3,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod3_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod3,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod3_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod2,mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2c.xlsx")),overwrite=T)
    
    ## Test for model improvement
    
    export(rownames_to_column(data.frame(anova(mod2,mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_MC.xlsx")),overwrite=T)
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"||cntry)"))
    
    export(rownames_to_column(data.frame(anova(lmer(mod3.f.no.recov,
                                                    weights = anweight,
                                                    data=temp.fdat),
                                               mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_RECOV.xlsx")),overwrite=T)
    
    
    # formula for FE as one of many predictors
    
    mod4.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    ## fit the model
    mod4<-lmer(mod4.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod4,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod4_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod4,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod4_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2c_total.xlsx")),overwrite=T)
    
    # compare to model without the focal predictor
    
    mod4.red.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(paste0(IV[-i],".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    mod4.red<-lmer(mod4.red.f,
                   weights = anweight,data=temp.fdat)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4.red,mod4)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod4_R2c_unique.xlsx")),overwrite=T)
    
    # formula for RE as one of many predictors
    
    mod5.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(",IV.temp,"|cntry)"))
    
    ## fit the model
    mod5<-lmer(mod5.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod5,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod5_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod5,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod5_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2c_total.xlsx")),
           overwrite=T)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4,mod5)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod5_R2c_unique.xlsx")),overwrite=T)
    
    
  }
  
  
}




DV_by_values_CLI_pipe<-function(DV,IV,IV.c,data,
                                moderator,directory){
  
  #DV="lrgen.z"
  #IV=value.vars
  #IV.c=value.vars.c
  #moderator="West_vs_post_comm"
  #directory="Z:/postdoc/Values and Voting/results/ERQ4"
  
  dir.create(path = paste0(directory,"/",DV))
  
  dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- data %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  all_of(moderator),
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               "gndr.c",
                               "age10.c"))
  
  # fit DV-only model
  # define the formula
  mod0.f<-
    as.formula(paste0(DV,"~","(1|cntry)"))
  
  ## fit the model
  mod0<-lmer(mod0.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(data.frame(summary(mod0)$coefficients)),
         paste0(dir.temp,"mod0_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod0,round = 12),
         paste0(dir.temp,"mod0_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod0)),
         paste0(dir.temp,"mod0_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod0)),
         paste0(dir.temp,"mod0_R2.xlsx"),overwrite=T)
  
  # fit covariates-only model
  # define the formula
  mod1.f<-
    as.formula(paste0(DV,"~","gndr.c.gmc+age10.c.gmc+","(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod1,round=12,p.round=12)),
         paste0(dir.temp,"mod1_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1,round = 12),
         paste0(dir.temp,"mod1_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod1)),
         paste0(dir.temp,"mod1_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod1)),
         paste0(dir.temp,"mod1_R2.xlsx"),overwrite=T)
  
  # Loop through each IV (and) IV.c
  
  for (i in 1:length(IV)){
    
    IV.temp<-paste0(IV[i],".gmc")
    IV.c.temp<-paste0(IV.c[i],".gmc")
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        "(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod2,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod2_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod2,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod2_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod1,mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2c.xlsx")),overwrite=T)
    
    # formula for RE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod3,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod3_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod3,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod3_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod2,mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2c.xlsx")),overwrite=T)
    
    ## Test for model improvement
    
    export(rownames_to_column(data.frame(anova(mod2,mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_MC.xlsx")),overwrite=T)
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"||cntry)"))
    
    export(rownames_to_column(data.frame(anova(lmer(mod3.f.no.recov,
                                                    weights = anweight,
                                                    data=temp.fdat),
                                               mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_RECOV.xlsx")),overwrite=T)
    
    
    # formula for moderator FE
    
    mod4.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",moderator,"+",
                        IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod4<-lmer(mod4.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod4,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod4_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod4,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod4_RE.xlsx")),overwrite=T)
    
    ## Decomposition of variance
    
    export(rownames_to_column(DE.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod3,mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2c.xlsx")),overwrite=T)
    
    # formula for cross-level interaction model
    
    mod5.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",moderator,"+",
                        IV.c.temp,"+",moderator,
                        ":",IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    
    ## fit the model
    mod5<-lmer(mod5.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod5,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod5_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod5,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod5_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod4,mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2c.xlsx")),
           overwrite=T)
    
    ## Marginal effects
    
    temp.marg<-
      emtrends(mod5,
             var=IV.c.temp,
             specs=moderator,
             at=list(moderator=c(-0.5,0.5)),
             disable.pbkrtest=T,
             lmerTest.limit = 40000,
             infer=c(T,T))
    
    export(data.frame(temp.marg),
           paste0(dir.temp,paste0(IV[i],"_mod5_marginal.xlsx")),
           overwrite=T)
  }
}


# Summary for level-2 moderation effects

lvl2.mod.summary<-function(DV,IV,fp){
  
  #DV="lrgen.z"
  #IV="con.c.gmc"
  #fp="Z:/postdoc/Values and Voting/results/ERQ4"
  
  # Obtain fixed effect test from random effect model
  
  fp.temp<-paste0(fp,"/",DV,"/")
  
  d.fe<-import(paste0(fp.temp,substr(IV,1,3),"_mod3_FE.xlsx"))[4,]
  d.fe<-
    c(FE_est=round_tidy(d.fe$Est.,2),
      p_FE=ifelse(d.fe$p<.001,paste0("<.001"),
               round_tidy(d.fe$p,3)))
  
  # Obtain random slope significance test
  
  d.rs<-import(paste0(fp.temp,substr(IV,1,3),"_mod3_MC.xlsx"))
  d.rs<-
    c("RE_test"=paste0("X2(",paste0(d.rs[2,"Df"]),") = ",
         round_tidy(d.rs[2,"Chisq"],2),", ",
         ifelse(d.rs[2,"Pr..Chisq."]<.001,
                paste0("p < .001"),
                paste0("p = ",round_tidy(d.rs[2,"Pr..Chisq."],3)))))
  
  # Obtain cross-level interaction estimate
  
  d.cli<-
    import(paste0(fp.temp,
                  substr(IV,1,3),
                  "_mod5_FE.xlsx"))[grepl(":",
         import(paste0(fp.temp,substr(IV,1,3),
                       "_mod5_FE.xlsx"))[,"rowname"]),]
  
  d.cli<-
    c(CLI_Est=round_tidy(d.cli$Est.,2),
      p_CLI=ifelse(d.cli$p<.001,paste0("<.001"),
               round_tidy(d.cli$p,3)))
  
  # obtain marginal effects
  
  d.me<-import(paste0(fp.temp,
                substr(IV,1,3),
                "_mod5_marginal.xlsx"))
  d.me_1<-
    c(me_1=round_tidy(d.me[1,2],2),
      p_1=ifelse(d.me[1,"p.value"]<.001,"<.001",
                 round_tidy(d.me[1,"p.value"],3)),
      CI_1=paste0("[",
                  round_tidy(d.me[1,"lower.CL"],2),
                  ", ",
                  round_tidy(d.me[1,"upper.CL"],2),
                  "]"))
  
  d.me_2<-
    c(me_2=round_tidy(d.me[2,2],2),
      p_2=ifelse(d.me[2,"p.value"]<.001,"<.001",
                 round_tidy(d.me[2,"p.value"],3)),
      CI_2=paste0("[",
                  round_tidy(d.me[2,"lower.CL"],2),
                  ", ",
                  round_tidy(d.me[2,"upper.CL"],2),
                  "]"))
  
  output<-c(d.fe,d.rs,d.cli,d.me_1,d.me_2)
  
  return(output)
  
}


# pipe for level-1 moderations

DV_by_values_lvl1_pipe<-function(DV,IV,IV.c,
                                 moderator,directory,data){
  
  #data=fdat
  #DV="lrgen.z"
  #IV=value.vars
  #IV.c=value.vars.c
  #moderator="edu.c"
  #directory="Z:/postdoc/Values and Voting/results/ERQ5"
  
  dir.create(path = paste0(directory,"/",DV))
  
  dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- data %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  all_of(moderator),
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               "edu.c",
                               "gndr.c",
                               "age10.c"))
  
  # fit DV-only model
  # define the formula
  mod0.f<-
    as.formula(paste0(DV,"~","(1|cntry)"))
  
  ## fit the model
  mod0<-lmer(mod0.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(data.frame(summary(mod0)$coefficients)),
         paste0(dir.temp,"mod0_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod0,round = 12),
         paste0(dir.temp,"mod0_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod0)),
         paste0(dir.temp,"mod0_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod0)),
         paste0(dir.temp,"mod0_R2.xlsx"),overwrite=T)
  
  # fit covariates-only model
  # define the formula
  mod1.f<-
    as.formula(paste0(DV,"~","gndr.c.gmc+age10.c.gmc+","(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod1,round=12,p.round=12)),
         paste0(dir.temp,"mod1_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1,round = 12),
         paste0(dir.temp,"mod1_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod1)),
         paste0(dir.temp,"mod1_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod1)),
         paste0(dir.temp,"mod1_R2.xlsx"),overwrite=T)
  
  # Loop through each IV (and) IV.c
  
  for (i in 1:length(IV)){
    
    IV.temp<-paste0(IV[i],".gmc")
    IV.c.temp<-paste0(IV.c[i],".gmc")
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        "(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod2,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod2_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod2,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod2_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod1,mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2c.xlsx")),overwrite=T)
    
    # formula for RE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod3,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod3_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod3,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod3_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod2,mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2c.xlsx")),overwrite=T)
    
    ## Test for model improvement
    
    export(rownames_to_column(data.frame(anova(mod2,mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_MC.xlsx")),overwrite=T)
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"||cntry)"))
    
    export(rownames_to_column(data.frame(anova(lmer(mod3.f.no.recov,
                                                    weights = anweight,
                                                    data=temp.fdat),
                                               mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_RECOV.xlsx")),overwrite=T)
    
    
    # formula for moderator FE (include also level-2 summary)
    
    mod4.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",moderator,"+",
                        moderator,".gm","+",
                        IV.c.temp,"+(1|cntry)"))
    
    ## fit the model
    mod4<-lmer(mod4.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod4,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod4_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod4,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod4_RE.xlsx")),overwrite=T)
    
    ## Decomposition of variance
    
    export(rownames_to_column(DE.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    #export(rownames_to_column(R2c.frame(mod3,mod4)),
    #       paste0(dir.temp,paste0(IV[i],"_mod4_R2c.xlsx")),overwrite=T)
    
    # formula for level-1 interaction model
    
    mod5.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",moderator,"+",
                        moderator,".gm","+",
                        IV.c.temp,"+",
                        IV.c.temp,":",moderator,
                        "+(1|cntry)"))
    
    
    ## fit the model
    mod5<-lmer(mod5.f,
               weights = anweight,data=temp.fdat,
               control = lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e6)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod5,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod5_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod5,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod5_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    #export(rownames_to_column(R2c.frame(mod4,mod5)),
    #       paste0(dir.temp,paste0(IV[i],"_mod5_R2c.xlsx")),
    #       overwrite=T)
    
    ## Marginal effects
    
    temp.marg<-
      emtrends(mod5,
               var=IV.c.temp,
               specs=moderator,
               at=list(moderator=c(-0.5,0.5)),
               disable.pbkrtest=T,
               lmerTest.limit = 40000,
               infer=c(T,T))
    
    export(data.frame(temp.marg),
           paste0(dir.temp,paste0(IV[i],"_mod5_marginal.xlsx")),
           overwrite=T)
  }
  
  # Run model that includes all ten values and their interactions with moderator
  
  # define the formula
  mod10.f<-
    as.formula(paste0(DV,"~",
                      "gndr.c.gmc+age10.c.gmc+",moderator,"+",
                      moderator,".gm","+",
                      paste0(IV,".gmc",collapse="+"),"+",
                      paste0(paste0(IV,".gmc"),":",moderator,collapse="+"),
                      "+(1|cntry)"))
  
  
  ## fit the model
  mod10<-lmer(mod10.f,
              weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod10,round=12,p.round=12)),
         paste0(dir.temp,paste0(IV[i],"_mod10_FE.xlsx")),overwrite=T)
  
  ## Random effects
  export(getVC(mod10,round = 12),
         paste0(dir.temp,paste0(IV[i],"_mod10_RE.xlsx")),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod10)),
         paste0(dir.temp,paste0(IV[i],"_mod10_DC.xlsx")),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod10)),
         paste0(dir.temp,paste0(IV[i],"_mod10_R2.xlsx")),overwrite=T)
  
  
}


## main and marginal effects summary

main.marginal.summary<-function(DV,IV,fp){
  
  #DV="lrgen.z"
  #IV="con.c.gmc"
  #fp="Z:/postdoc/Values and Voting/results"
  
  # Obtain fixed main effect from single predictor model
  
  #fp.temp<-paste0(fp,"/",DV,"/")
  #fp.temp<-paste0(fp,"/main_and_marginal/",DV,"/")
  #fp
  
  d.fe<-import(paste0(fp,"/main/",DV,"/",
                      substr(IV,1,3),"_mod2_FE.xlsx"))[4,]
  d.fe.b<-d.fe$Est.
  d.fe.p<-d.fe$p
  
  d.fe<-
    c(FE_est=round_tidy(d.fe$Est.,2),
      p_FE=ifelse(d.fe$p<.001,paste0("<.001"),
                  round_tidy(d.fe$p,3)))
  
  # obtain FE variance explained
  d.r2<-import(paste0(fp,"/main/",DV,"/",
                      substr(IV,1,3),"_mod2_R2c.xlsx"))
  d.r2<-
    round_tidy(100*d.r2[d.r2$rowname=="fv","within"],2)
  
  # obtain fixed effect from multi-predictor model
  # Obtain fixed main effect from single predictor model
  
  #fp.temp<-paste0(fp,"/",DV,"/")
  #fp.temp<-paste0(fp,"/main_and_marginal/",DV,"/")
  #fp
  
  d.fe.multi<-import(paste0(fp,"/main/",DV,"/",
                      substr(IV,1,3),"_mod5_FE.xlsx"))
  d.fe.multi<-
    d.fe.multi[grepl(substr(IV,1,3),d.fe.multi$rowname),]
  
  
  d.fe.multi.b<-d.fe.multi$Est.
  d.fe.multi.p<-d.fe.multi$p
  
  d.fe.multi<-
    c(FEm_est=round_tidy(d.fe.multi$Est.,2),
      p_FEm=ifelse(d.fe.multi$p<.001,paste0("<.001"),
                  round_tidy(d.fe.multi$p,3)))
  
  # obtain logical for education moderation
  d.edu<-import(paste0(fp,"/RQ5_ERQ5/",DV,"/",
                            substr(IV,1,3),"_mod5_FE.xlsx"))
  d.edu.p<-d.edu[grepl(":",d.edu$rowname),"p"]
  
  # obtain marginal effects if significant interaction
  d.edu.marg<-import(paste0(fp,"/RQ5_ERQ5/",DV,"/",
                       substr(IV,1,3),"_mod5_marginal.xlsx"))
  d.edu.marg.1<-c(No_col=round_tidy(d.edu.marg[1,2],2),
                  p_No_col=ifelse(d.edu.marg[1,"p.value"]<.001,
                                  paste0("<.001"),
                              round_tidy(d.edu.marg[1,"p.value"],3)))
  d.edu.marg.2<-c(Col=round_tidy(d.edu.marg[2,2],2),
                  p_Col=ifelse(d.edu.marg[2,"p.value"]<.001,
                                  paste0("<.001"),
                                  round_tidy(d.edu.marg[2,"p.value"],3)))
  
  # obtain logical for random slope
  d.rs.p<-import(paste0(fp,"/RQ5_ERQ5/",DV,"/",
                       substr(IV,1,3),"_mod3_MC.xlsx"))
  d.rs.p<-d.rs.p[2,"Pr..Chisq."]
  
  # obtain fixed effect from random slope model
  d.fe.re<-import(paste0(fp,"/RQ5_ERQ5/",DV,"/",
                         substr(IV,1,3),"_mod3_FE.xlsx"))[4,]
  d.fe.re.b<-d.fe.re$Est.
  d.fe.re.p<-d.fe.re$p
  
  # obtain logical for WP moderation
  d.wp.p<-import(paste0(fp,"/ERQ4/",DV,"/",
                        substr(IV,1,3),"_mod5_FE.xlsx"))
  d.wp.p<-d.wp.p[grepl(":",d.wp.p$rowname),"p"]
  
  # obtain marginal effects if significant interaction
  d.wp.marg<-import(paste0(fp,"/ERQ4/",DV,"/",
                            substr(IV,1,3),"_mod5_marginal.xlsx"))
  
  d.wp.marg.1<-c(West=round_tidy(d.wp.marg[1,2],2),
                  p_West=ifelse(d.wp.marg[1,"p.value"]<.001,
                                  paste0("<.001"),
                                  round_tidy(d.wp.marg[1,"p.value"],3)))
  
  d.wp.marg.2<-c(Post=round_tidy(d.wp.marg[2,2],2),
                  p_Post=ifelse(d.wp.marg[2,"p.value"]<.001,
                                  paste0("<.001"),
                                  round_tidy(d.wp.marg[2,"p.value"],3)))
  
  # output based on significance
  
  if(d.fe.b/d.fe.multi.b>0 & d.fe.p<.05 & d.fe.multi.p<.05){b.fe.temp<-round_tidy(d.fe.b,2)} else {b.fe.temp<-c("")}
  if(d.fe.b/d.fe.multi.b>0 & d.fe.p<.05 & d.fe.multi.p<.05){r2.fe.temp<-d.r2} else {r2.fe.temp<-c("")}
  
  if(d.fe.b/d.fe.multi.b>0 & d.fe.p<.05 & d.fe.multi.p<.05 & d.rs.p<.05 & d.fe.re.p<.05){b.re.temp<-round_tidy(d.fe.re.b,2)} else {b.re.temp<-c("")}
  
  if(d.edu.p<.05){edu.temp.1<-d.edu.marg.1} else {edu.temp.1<-c("","")}
  if(d.edu.p<.05){edu.temp.2<-d.edu.marg.2} else {edu.temp.2<-c("","")}
  
  if(d.rs.p<.05 & d.wp.p<.05){wp.temp.1<-d.wp.marg.1} else {wp.temp.1<-c("","")}
  if(d.rs.p<.05 & d.wp.p<.05){wp.temp.2<-d.wp.marg.2} else {wp.temp.2<-c("","")}
  
  #if(d.fe.b/d.fe.multi.b>0 & d.fe.p<.05 & d.fe.multi.p<.05 & d.edu.p<.05){edu.temp.1<-d.edu.marg.1} else {edu.temp.1<-c("","")}
  #if(d.fe.b/d.fe.multi.b>0 & d.fe.p<.05 & d.fe.multi.p<.05 & d.edu.p<.05){edu.temp.2<-d.edu.marg.2} else {edu.temp.2<-c("","")}
  
  #if(d.fe.b/d.fe.multi.b>0 & d.fe.p<.05 & d.fe.multi.p<.05 & d.rs.p<.05 & d.wp.p<.05){wp.temp.1<-d.wp.marg.1} else {wp.temp.1<-c("","")}
  #if(d.fe.b/d.fe.multi.b>0 & d.fe.p<.05 & d.fe.multi.p<.05 & d.rs.p<.05 & d.wp.p<.05){wp.temp.2<-d.wp.marg.2} else {wp.temp.2<-c("","")}
  
  output<-
    c(d.fe,R2=d.r2,d.fe.multi,
      edu.temp.1,edu.temp.2,wp.temp.1,wp.temp.2)
  
  output.no.p<-
    c(b=b.fe.temp,R2=r2.fe.temp,b_re=b.re.temp,
      edu.temp.1,edu.temp.2,wp.temp.1,wp.temp.2)
  
  
  #return(list(output,output.no.p))
  return(output.no.p)
  
}



## main and marginal effects summary

for.main.figures<-function(DV,IV,fp){
  
  #DV="lrgen.z"
  #IV="con.c.gmc"
  #fp="results"
  
  # Obtain fixed main effect from single predictor model
  
  #fp.temp<-paste0(fp,"/",DV,"/")
  #fp.temp<-paste0(fp,"/main_and_marginal/",DV,"/")
  #fp
  
  d.fe<-import(paste0(fp,"/main/",DV,"/",
                      substr(IV,1,3),"_mod2_FE.xlsx"))[4,]
  #d.fe.b<-d.fe$Est.
  #d.fe.p<-d.fe$p
  
  #d.fe<-
  #  c(FE_est=round_tidy(d.fe$Est.,2),
  #    p_FE=ifelse(d.fe$p<.001,paste0("<.001"),
  #                round_tidy(d.fe$p,3)))
  
  # obtain FE variance explained
  d.r2<-import(paste0(fp,"/main/",DV,"/",
                      substr(IV,1,3),"_mod2_R2c.xlsx"))
  d.r2<-
    d.r2[d.r2$rowname=="f1","within"]
  
  # obtain fixed effect from multi-predictor model
  # Obtain fixed main effect from single predictor model
  
  #fp.temp<-paste0(fp,"/",DV,"/")
  #fp.temp<-paste0(fp,"/main_and_marginal/",DV,"/")
  #fp
  
  d.fe.multi<-import(paste0(fp,"/main/",DV,"/",
                            substr(IV,1,3),"_mod4_FE.xlsx"))
  d.fe.multi<-
    d.fe.multi[grepl(substr(IV,1,3),d.fe.multi$rowname),]
  
  
  # obtain FE variance explained from multivariable model
  
  d.r2.multi<-import(paste0(fp,"/main/",DV,"/",
                      substr(IV,1,3),"_mod4_R2c_total.xlsx"))
  d.r2.multi<-
    d.r2.multi[d.r2.multi$rowname=="f1","within"]
  
  names(d.fe.multi)<-paste0(names(d.fe.multi),".mv")
  
  # statistics for non-ipsatized single predictors
  IV_non_ip<-paste0(substr(IV,1,3),".gmc")
  
  d.fe_non_ip<-import(paste0(fp,"/main_non_ipsatized/",DV,"/",
                      substr(IV,1,3),"_mod2_FE.xlsx"))[4,]
  
  names(d.fe_non_ip)<-paste0(names(d.fe_non_ip),"_non_ip")
  
  d.r2_non_ip<-import(paste0(fp,"/main_non_ipsatized/",DV,"/",
                      substr(IV,1,3),"_mod2_R2c.xlsx"))
  
  d.r2_non_ip<-
    d.r2_non_ip[d.r2_non_ip$rowname=="f1","within"]
  
  # statistics for lrecon controlled models
  #IV_ct_lrecon<-paste0(substr(IV,1,3),".gmc")
  
  #d.fe_ct_lrecon<-import(paste0(fp,"/main_control_lrecon/",DV,"/",
  #                           substr(IV,1,3),"_mod2_FE.xlsx"))[4,]
  
  #names(d.fe_ct_lrecon)<-paste0(names(d.fe_ct_lrecon),"_ct_lrecon")
  
  #d.r2_ct_lrecon<-import(paste0(fp,"/main_control_lrecon/",DV,"/",
  #                           substr(IV,1,3),"_mod2_R2c.xlsx"))
  
  #d.r2_ct_lrecon<-
  #  d.r2_ct_lrecon[d.r2_ct_lrecon$rowname=="f1","within"]
  
  # statistics for galtan controlled models
  #IV_ct_galtan<-paste0(substr(IV,1,3),".gmc")
  
  #d.fe_ct_galtan<-import(paste0(fp,"/main_control_galtan/",DV,"/",
  #                              substr(IV,1,3),"_mod2_FE.xlsx"))[4,]
  
  #names(d.fe_ct_galtan)<-paste0(names(d.fe_ct_galtan),"_ct_galtan")
  
  #d.r2_ct_galtan<-import(paste0(fp,"/main_control_galtan/",DV,"/",
  #                              substr(IV,1,3),"_mod2_R2c.xlsx"))
  
  #d.r2_ct_galtan<-
  #  d.r2_ct_galtan[d.r2_ct_galtan$rowname=="f1","within"]
  
  # statistics for populism controlled models
  IV_ct_populism<-paste0(substr(IV,1,3),".gmc")
  
  d.fe_ct_populism<-import(paste0(fp,"/main_control_populism/",DV,"/",
                                substr(IV,1,3),"_mod2_FE.xlsx"))[4,]
  
  names(d.fe_ct_populism)<-paste0(names(d.fe_ct_populism),"_ct_populism")
  
  d.r2_ct_populism<-import(paste0(fp,"/main_control_populism/",DV,"/",
                                substr(IV,1,3),"_mod2_R2c.xlsx"))
  
  d.r2_ct_populism<-
    d.r2_ct_populism[d.r2_ct_populism$rowname=="f1","within"]
  
  # combine the results to same array
  output<-
    cbind(d.fe,d.r2,d.fe.multi,d.r2.multi,
          d.fe_non_ip,d.r2_non_ip,
          #d.fe_ct_lrecon,d.r2_ct_lrecon,
          #d.fe_ct_galtan,d.r2_ct_galtan)#,
          d.fe_ct_populism,d.r2_ct_populism)
  
  #return(list(output,output.no.p))
  return(output)
  
}

for.edu.figures<-function(DV,IV,fp){
  
  #DV="lrgen.z"
  #IV="con.c.gmc"
  #fp="Z:/postdoc/Values and Voting/results"
  
  d.fe<-import(paste0(fp,"/RQ5_ERQ5/",DV,"/",
                      substr(IV,1,3),"_mod5_FE.xlsx"))[7,]
  
  d.marg<-import(paste0(fp,"/RQ5_ERQ5/",DV,"/",
                        substr(IV,1,3),"_mod5_marginal.xlsx"))
  
  output<-data.frame(
    type=c("Interaction","No college degree","College degree"),
    est=c(d.fe$Est.,d.marg[,2]),
    SE=c(d.fe$SE,d.marg$SE),
    df=c(d.fe$df,d.marg$df),
    t_value=c(d.fe$t,d.marg$t.ratio),
    p=c(d.fe$p,d.marg$p.value),
    LL=c(d.fe$LL,d.marg$lower.CL),
    UL=c(d.fe$UL,d.marg$upper.CL))
  
  return(output)
  
}


for.WE.figures<-function(DV,IV,fp){
  
  #DV="lrgen.z"
  #IV="con.c.gmc"
  #fp="Z:/postdoc/Values and Voting/results"
  
  slope_sd<-import(paste0(fp,"/ERQ4/",DV,"/",
                      substr(IV,1,3),"_mod3_RE.xlsx"))[2,"sdcor"]
  slope_p<-import(paste0(fp,"/ERQ4/",DV,"/",
                         substr(IV,1,3),"_mod3_MC.xlsx"))[2,"Pr..Chisq."]
  cli.eff<-import(paste0(fp,"/ERQ4/",DV,"/",
                         substr(IV,1,3),"_mod5_FE.xlsx"))[6,]
  
  d.marg<-import(paste0(fp,"/ERQ4/",DV,"/",
                        substr(IV,1,3),"_mod5_marginal.xlsx"))
  
  
  output<-data.frame(
    type=c("Interaction","Western Europe","Post-communist"),
    est=c(cli.eff$Est.,d.marg[,2]),
    SE=c(cli.eff$SE,d.marg$SE),
    df=c(cli.eff$df,d.marg$df),
    t_value=c(cli.eff$t,d.marg$t.ratio),
    p=c(cli.eff$p,d.marg$p.value),
    LL=c(cli.eff$LL,d.marg$lower.CL),
    UL=c(cli.eff$UL,d.marg$upper.CL),
    slope_sd=slope_sd,
    slope_p=slope_p)

  return(output)
  
}

# main effect pipe for minority covariate

DV_by_values_minority_pipe<-function(DV,IV,IV.c,directory,data){
  
  
  #DV=DV.vars[1]
  #IV=value.vars
  #data=fdat
  #IV.c=value.vars.c
  #directory="results/main_with_minority"
  
  dir.create(path = paste0(directory,"/",DV))
  
  dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- data %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  "gndr.c",
                  "age10.c",
                  "blgetmg.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               "gndr.c",
                               "blgetmg.c",
                               "age10.c"))
  
  # fit DV-only model
  # define the formula
  mod0.f<-
    as.formula(paste0(DV,"~","(1|cntry)"))
  
  ## fit the model
  mod0<-lmer(mod0.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(data.frame(summary(mod0)$coefficients)),
         paste0(dir.temp,"mod0_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod0,round = 12),
         paste0(dir.temp,"mod0_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod0)),
         paste0(dir.temp,"mod0_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod0)),
         paste0(dir.temp,"mod0_R2.xlsx"),overwrite=T)
  
  # fit covariates-only model
  # define the formula
  mod1.f<-
    as.formula(paste0(DV,"~",
                      "gndr.c.gmc+age10.c.gmc+blgetmg.c.gmc+",
                      "(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod1,round=12,p.round=12)),
         paste0(dir.temp,"mod1_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1,round = 12),
         paste0(dir.temp,"mod1_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod1)),
         paste0(dir.temp,"mod1_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod1)),
         paste0(dir.temp,"mod1_R2.xlsx"),overwrite=T)
  
  # Loop through each IV (and) IV.c
  
  for (i in 1:length(IV)){
    
    IV.temp<-paste0(IV[i],".gmc")
    IV.c.temp<-paste0(IV.c[i],".gmc")
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+blgetmg.c.gmc+",
                        IV.c.temp,"+",
                        "(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod2,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod2_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod2,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod2_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod1,mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2c.xlsx")),overwrite=T)
    
    # formula for FE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+blgetmg.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod3,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod3_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod3,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod3_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod2,mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2c.xlsx")),overwrite=T)
    
    ## Test for model improvement
    
    export(rownames_to_column(data.frame(anova(mod2,mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_MC.xlsx")),overwrite=T)
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+blgetmg.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"||cntry)"))
    
    export(rownames_to_column(data.frame(anova(lmer(mod3.f.no.recov,
                                                    weights = anweight,
                                                    data=temp.fdat),
                                               mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_RECOV.xlsx")),overwrite=T)
    
    
    # formula for FE as one of many predictors
    
    mod4.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+blgetmg.c.gmc+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    ## fit the model
    mod4<-lmer(mod4.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod4,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod4_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod4,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod4_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2c_total.xlsx")),overwrite=T)
    
    # compare to model without the focal predictor
    
    mod4.red.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+blgetmg.c.gmc+",
                        paste0(paste0(IV[-i],".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    mod4.red<-lmer(mod4.red.f,
                   weights = anweight,data=temp.fdat)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4.red,mod4)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod4_R2c_unique.xlsx")),overwrite=T)
    
    # formula for RE as one of many predictors
    
    mod5.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+blgetmg.c.gmc+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(",IV.temp,"|cntry)"))
    
    ## fit the model
    mod5<-lmer(mod5.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod5,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod5_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod5,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod5_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2c_total.xlsx")),
           overwrite=T)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4,mod5)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod5_R2c_unique.xlsx")),overwrite=T)
    
    
  }
  
  
}


Heterogeneity_plot_pipe<-
  function(DV,IV,IV.c,directory,data){
  
  #DV=DV.vars[1]
  #IV=value.vars
  #IV.c=value.vars.c
  #data=fdat
  #directory="../../results/main/figures"
  
  #dir.create(path = paste0(directory,"/",DV))
  
  #dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- data %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               "gndr.c",
                               "age10.c"))
  
  # define the covariate model formula
  mod1.f<-
    as.formula(paste0(DV,"~","gndr.c.gmc+age10.c.gmc+","(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  # Loop through each IV (and) IV.c
  #plot.list<-list()
  
  for (i in 1:length(IV)){
    #i=4
    IV.temp<-paste0(IV[i],".gmc")
    IV.c.temp<-paste0(IV.c[i],".gmc")
    DV.temp<-DV
    
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        "(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat)
    
    # formula for RE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e7)))
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+(",IV.c.temp,"||cntry)"))
    
    mod3.no.recov<-lmer(mod3.f.no.recov,
                        weights = anweight,data=temp.fdat,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e7)))
    
    
    if(check_convergence(mod3)[[1]]) {
      modRE<-mod3} else {modRE<-mod3.no.recov}
    
    # setup for the figure
    
    estimates<-posterior_vs_fixed_lmer(model=mod3,predictor=IV.c.temp,
                                       cluster = "cntry",
                                       alpha = .05/length(unique(temp.fdat$cntry)))
    estimates$relation_to_fixed<-
      case_when(
        estimates$post_est> estimates$UL~"superior",
        estimates$post_est< estimates$LL~"inferior",
        TRUE~"indifferent")
    
    crits<-
      get_crit(est=estimates$fixed_est[1],
               se=estimates$fixed_se[1],
               df=estimates$fixed_df[1],
               at=c((.05/length(unique(temp.fdat$cntry)))/2,
                    .025,.975,
                    1-(.05/length(unique(temp.fdat$cntry)))/2))
    
    
    tmp.plot<-
      ggplot(data=estimates,aes(x=post_est,
                                y=reorder(rownames(estimates),
                                          desc(rownames(estimates)))))+
      geom_point(size=12,aes(color=relation_to_fixed),show.legend = F)+
      scale_color_manual(values=c("black",
                                  met.brewer("Troy")[3],
                                  met.brewer("Troy")[6]))+
      geom_flag(aes(country=tolower(rownames(estimates))),size=8)+
      #geom_vline(xintercept=a$fixed_est[1],linewidth=3)+
      geom_segment(aes(x = estimates$fixed_est[1],
                       xend = estimates$fixed_est[1],
                       y = 0, yend = 21),linewidth=3)+
      geom_segment(aes(x = 0, xend = 0,
                       y = 0, yend = 21),linewidth=3,
                   color=met.brewer("Troy")[4])+
      geom_segment(aes(x = crits[1], xend = crits[1],
                       y = 0, yend = 21),linewidth=1,linetype=3)+
      geom_segment(aes(x = crits[4], xend = crits[4],
                       y = 0, yend = 21),linewidth=1,linetype=3)+
      geom_segment(aes(x = crits[2], xend = crits[2],
                       y = 0, yend = 21),linewidth=1,linetype=2)+
      geom_segment(aes(x = crits[3], xend = crits[3],
                       y = 0, yend = 21),linewidth=1,linetype=2)+
      theme(panel.background = element_blank())+
      annotate('rect', xmin=crits[1],
               xmax=crits[4],
               ymin=0,ymax=21, alpha=.5, fill='gray')+
      annotate(geom="text", x=crits[1], y=21.5, label="0.125%",
               color="black",size=3)+
      annotate(geom="text", x=crits[2], y=22.0, label="2.5%",
               color="black",size=3)+
      annotate(geom="text", x=crits[3], y=22.0, label="97.5%",
               color="black",size=3)+
      annotate(geom="text", x=crits[4], y=21.5, label="99.875%",
               color="black",size=3)+
      annotate(geom="text", x=-0.3,
               y=23,color="black",size=4,hjust="left",
               label=paste0("From single predictor random effect model: ",
                            "fixed slope = ",
                            round_tidy(estimates$fixed_est[1],2),", random slope SD = ",getVC(mod3)[2,"sdcor"]))+
      annotate(geom="text", x=-0.3, y=22.5,color="black",size=4,hjust="left",
               label=paste0("R-squared within countries via fixed and random slopes = ",
                            round_tidy(100*R2c.frame(mod1,modRE)["fv","within"],1),"%",
                            " (of which via random ",
                            round_tidy(100*R2c.frame(mod1,modRE)["v","within"]/
                                         R2c.frame(mod1,modRE)["fv","within"],1),"%)"))+
      coord_cartesian(ylim=c(0,23),xlim=c(-0.6,0.6),clip="off")+
      xlab(paste0("Slope estimate of ",
                   Value.names[which(value.vars.c==IV.c[i])],
                   " on ",DV.names[which(DV.vars==DV)]))+
      ylab("Country")#+
      #ggtitle(paste0("Figure S",21+k*10-10+i))
    
    print(tmp.plot)
  }
  
}


get_crit<-function(est,se,df,at){
  est+qt(p=at,df=df)*se
}


posterior_vs_fixed_lmer<-
  function(model,predictor,cluster,alpha){
    # obtain cluster names
    cluster_labels<-rownames(ranef(model)[cluster][[1]])
    # obtain conditional posterior modes
    cond_ests<-ranef(model)[cluster][[1]][,predictor]
    # obtain fixed effect estimate, standard error, and degrees of freedom
    fixef_est=summary(model)$coefficients[predictor,"Estimate"]
    fixef_se=summary(model)$coefficients[predictor,"Std. Error"]
    fixef_df=summary(model)$coefficients[predictor,"df"]
    # compute z-scores 
    z_scores<-cond_ests/fixef_se
    # compute p-values
    pvals<-2*(1-pt(q = abs(z_scores),
                   df = fixef_df))
    # obtain posterior modes
    post_ests=fixef_est+cond_ests
    # obtain critical values
    L_crit=fixef_est+qt(p = alpha/2,df=fixef_df)*fixef_se
    U_crit=fixef_est+qt(p = 1-(alpha/2),df=fixef_df)*fixef_se
    
    # combine to output
    output<-
      cbind(
        fixed_est=fixef_est,
        fixed_se=fixef_se,
        fixed_df=fixef_df,
        LL=L_crit,
        UL=U_crit,
        post_est=post_ests,
        z=z_scores,
        p=pvals,
        p_adj_holm=p.adjust(pvals,"holm"),
        p_adj_bonf=p.adjust(pvals,"bonferroni")
      )
    rownames(output)<-cluster_labels
    output<-data.frame(output)
    return(output)
  }


# This pipe is for analyzing the associations without 
# subtracting the response style

DV_by_non_ipsatized_values_pipe<-function(DV,IV,IV.c,directory,data){
  
  #DV=DV.vars[1]
  #IV=value.vars
  #IV.c=value.vars.c
  #directory="results/main_non_ipsatized"
  #data=fdat
  
  dir.create(path = paste0(directory,"/",DV))
  
  dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- data %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               "gndr.c",
                               "age10.c"))
  
  # fit DV-only model
  # define the formula
  mod0.f<-
    as.formula(paste0(DV,"~","(1|cntry)"))
  
  ## fit the model
  mod0<-lmer(mod0.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(data.frame(summary(mod0)$coefficients)),
         paste0(dir.temp,"mod0_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod0,round = 12),
         paste0(dir.temp,"mod0_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod0)),
         paste0(dir.temp,"mod0_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod0)),
         paste0(dir.temp,"mod0_R2.xlsx"),overwrite=T)
  
  # fit covariates-only model
  # define the formula
  mod1.f<-
    as.formula(paste0(DV,"~","gndr.c.gmc+age10.c.gmc+","(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod1,round=12,p.round=12)),
         paste0(dir.temp,"mod1_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1,round = 12),
         paste0(dir.temp,"mod1_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod1)),
         paste0(dir.temp,"mod1_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod1)),
         paste0(dir.temp,"mod1_R2.xlsx"),overwrite=T)
  
  # Loop through each IV (and) IV.c
  
  for (i in 1:length(IV)){
    #i=1
    IV.temp<-paste0(IV[i],".gmc")
    #IV.c.temp<-paste0(IV.c[i],".gmc")
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.temp,"+",
                        "(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod2,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod2_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod2,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod2_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod1,mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2c.xlsx")),overwrite=T)
    
    # formula for FE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.temp,"+(",IV.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e7)))
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod3,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod3_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod3,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod3_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod2,mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2c.xlsx")),overwrite=T)
    
    ## Test for model improvement
    
    export(rownames_to_column(data.frame(anova(mod2,mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_MC.xlsx")),overwrite=T)
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.temp,"+(",IV.temp,"||cntry)"))
    
    export(rownames_to_column(data.frame(anova(lmer(mod3.f.no.recov,
                                                    weights = anweight,
                                                    data=temp.fdat,
                                                    control=lmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=2e7))),
                                               mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_RECOV.xlsx")),overwrite=T)
    
    
  }
  
}


# Main effect pipe with POs as controls

DV_by_values_control_pipe<-function(DV,IV,IV.c,covariates,directory,data){
  
  #DV=DV.vars[1]
  #IV=value.vars
  #IV.c=value.vars.c
  #directory="results/main_control_lrecon"
  #data=fdat
  #covariates="lrecon.z"
  
  dir.create(path = paste0(directory,"/",DV))
  
  dir.temp<-paste0(directory,"/",DV,"/")
  
  # select only the defined variables
  temp.fdat<- data %>% 
    dplyr::select(all_of(IV),
                  all_of(IV.c),
                  all_of(DV),
                  all_of(covariates),
                  "gndr.c",
                  "age10.c",
                  "cntry",
                  "anweight") %>%
    na.omit()
  
  # center all IVs within country
  
  temp.fdat<-
    group_mean_center(data=temp.fdat,group.var = "cntry",
                      vars = c(IV,
                               IV.c,
                               covariates,
                               "gndr.c",
                               "age10.c"))
  
  # fit DV-only model
  # define the formula
  mod0.f<-
    as.formula(paste0(DV,"~","(1|cntry)"))
  
  ## fit the model
  mod0<-lmer(mod0.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(data.frame(summary(mod0)$coefficients)),
         paste0(dir.temp,"mod0_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod0,round = 12),
         paste0(dir.temp,"mod0_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod0)),
         paste0(dir.temp,"mod0_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod0)),
         paste0(dir.temp,"mod0_R2.xlsx"),overwrite=T)
  
  # fit covariates-only model
  
  # define the formula
  mod1.f<-
    as.formula(paste0(DV,"~",
                      "gndr.c.gmc+age10.c.gmc+",
                      paste0(covariates,".gmc",collapse="+"),
                      "+(1|cntry)"))
  
  ## fit the model
  mod1<-lmer(mod1.f,
             weights = anweight,data=temp.fdat)
  
  ## collect important output
  
  ## Fixed effects
  export(rownames_to_column(getFE(mod1,round=12,p.round=12)),
         paste0(dir.temp,"mod1_FE.xlsx"),overwrite=T)
  
  ## Random effects
  export(getVC(mod1,round = 12),
         paste0(dir.temp,"mod1_RE.xlsx"),overwrite=T)
  
  ## Decompositions of variance
  
  export(rownames_to_column(DE.frame(mod1)),
         paste0(dir.temp,"mod1_DC.xlsx"),overwrite=T)
  
  ## Variance explained
  
  export(rownames_to_column(R2.frame(mod1)),
         paste0(dir.temp,"mod1_R2.xlsx"),overwrite=T)
  
  # Loop through each IV (and) IV.c
  
  for (i in 1:length(IV)){
    #i=4
    IV.temp<-paste0(IV[i],".gmc")
    IV.c.temp<-paste0(IV.c[i],".gmc")
    
    # formula for FE sole predictor
    
    mod2.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        paste0(covariates,".gmc",collapse="+"),
                        "+(1|cntry)"))
    
    ## fit the model
    mod2<-lmer(mod2.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod2,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod2_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod2,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod2_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod1,mod2)),
           paste0(dir.temp,paste0(IV[i],"_mod2_R2c.xlsx")),overwrite=T)
    
    # formula for FE sole predictor
    
    mod3.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        paste0(covariates,".gmc",collapse="+"),
                        "+(",IV.c.temp,"|cntry)"))
    
    ## fit the model
    mod3<-lmer(mod3.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod3,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod3_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod3,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod3_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2.xlsx")),overwrite=T)
    
    ## Variance explained change
    
    export(rownames_to_column(R2c.frame(mod2,mod3)),
           paste0(dir.temp,paste0(IV[i],"_mod3_R2c.xlsx")),overwrite=T)
    
    ## Test for model improvement
    
    export(rownames_to_column(data.frame(anova(mod2,mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_MC.xlsx")),overwrite=T)
    
    # refit without random effect correlation
    
    mod3.f.no.recov<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        IV.c.temp,"+",
                        paste0(covariates,".gmc",collapse="+"),
                        "+(",IV.c.temp,"||cntry)"))
    
    export(rownames_to_column(data.frame(anova(lmer(mod3.f.no.recov,
                                                    weights = anweight,
                                                    data=temp.fdat),
                                               mod3))),
           paste0(dir.temp,paste0(IV[i],"_mod3_RECOV.xlsx")),overwrite=T)
    
    
    # formula for FE as one of many predictors
    
    mod4.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(covariates,".gmc",collapse="+"),
                        "+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    
    ## fit the model
    mod4<-lmer(mod4.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod4,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod4_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod4,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod4_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod4)),
           paste0(dir.temp,paste0(IV[i],"_mod4_R2c_total.xlsx")),overwrite=T)
    
    # compare to model without the focal predictor
    
    mod4.red.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(covariates,".gmc",collapse="+"),
                        "+",
                        paste0(paste0(IV[-i],".gmc"),collapse=" + "),
                        "+",
                        "(1|cntry)"))
    
    mod4.red<-lmer(mod4.red.f,
                   weights = anweight,data=temp.fdat)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4.red,mod4)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod4_R2c_unique.xlsx")),overwrite=T)
    
    # formula for RE as one of many predictors
    
    mod5.f<-
      as.formula(paste0(DV,"~",
                        "gndr.c.gmc+age10.c.gmc+",
                        paste0(covariates,".gmc",collapse="+"),
                        "+",
                        paste0(paste0(IV,".gmc"),collapse=" + "),
                        "+",
                        "(",IV.temp,"|cntry)"))
    
    ## fit the model
    mod5<-lmer(mod5.f,
               weights = anweight,data=temp.fdat)
    
    ## collect important output
    
    ## Fixed effects
    export(rownames_to_column(getFE(mod5,round=12,p.round=12)),
           paste0(dir.temp,paste0(IV[i],"_mod5_FE.xlsx")),overwrite=T)
    
    ## Random effects
    export(getVC(mod5,round = 12),
           paste0(dir.temp,paste0(IV[i],"_mod5_RE.xlsx")),overwrite=T)
    
    ## Decompositions of variance
    
    export(rownames_to_column(DE.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_DC.xlsx")),overwrite=T)
    
    ## Variance explained
    
    export(rownames_to_column(R2.frame(mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2.xlsx")),overwrite=T)
    
    ## Variance explained change (total)
    
    export(rownames_to_column(R2c.frame(mod1,mod5)),
           paste0(dir.temp,paste0(IV[i],"_mod5_R2c_total.xlsx")),
           overwrite=T)
    
    ## Variance explained unique
    
    export(rownames_to_column(R2c.frame(mod4,mod5)),
           paste0(dir.temp,
                  paste0(IV[i],"_mod5_R2c_unique.xlsx")),overwrite=T)
    
    
  }
  
  
}

