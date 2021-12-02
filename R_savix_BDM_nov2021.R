# =======================================================
# BMA code to the replicate the results of the study 
# "What drives profits in savings groups? 
# Bayesian data mining evidence from the SAVIX database"
# -------------------------------------------------------
# Rolando Gonzales Martinez
# v. November 2021
# =======================================================
      rm(list=ls())
      dev.off()
      # Loading data:
          library(foreign)
          data <- read.dta("~/SAVIXpanel_for_R.dta")
            # 118 vars
            # 234948 obs   
          
# -------------- Removing empty cells ------------------------------        
    data = data[!(rowSums(is.na(data[,1:115]))),] # Removing rows with NAs
    # Obs: 110702
# ------------------------------------------------------------------
# Results for Returns on Savings (ROS)
# Dependent variable:
  y  <- (data$Y_ROS)*100
# Continuous variables:
  XC <- data[,4:11]
# Macro-economic variables:
  XM <- data[,12:22]
# Discrete (dummy) variables:
  XD <- data[,23:44]
        colSums(XD)
            # Remove columns with few entries
            XD <- XD[,colSums(XD)>150]
            # From 22 to 20 variables
            # Dropping: _GSsma, _FAOXFA
            colSums(XD)
# Fixed effects:
  Fe <- data[,45:115]
        colSums(Fe)
            # Remove columns with few entries
              Fe <- Fe[,colSums(Fe)>1000]
            # From 71 to 34
            colSums(Fe)
  
# Converting to a single data.frame:
    df = data.frame(cbind(y,XC,XD,XM,Fe))
    names(df)
    
# Adding names:
    names(df)<-c("y", # dependent variable
                 "X_MATT"         ,
                 "X_DROP"         ,
                 "X_NLOM"         ,
                 "X_WOMM"         ,
                 "X_GSIZ"         ,
                 "X_ALPM"         ,
                 "X_SFPM"         ,
                 "X_FUUR"         ,
                 "Xdum_SGPlus"    ,
                 "Xdum_SGPsoc"    ,
                 "Xdum_SGPfin"    ,
                 "Xdum_FANONE"    ,
                 "Xdum_FAPLAN"    ,
                 "Xdum_FAAKHA"    ,
                 "Xdum_FACARE"    ,
                 "Xdum_FAWOVI"    ,
                 "Xdum_FACRES"    ,
                 "Xdum_urban"     ,
                 "Xdum_GSact"     ,
                 "Xdum_GSgra"     ,
                 "Xdum_GSsup"     ,
                 "Xdum_GFBfield"  ,
                 "Xdum_GFBvilla"  ,
                 "Xdum_GFBgpaid"  ,
                 "Xdum_GFBprojp"  ,
                 "Xdum_GFBunpai"  ,
                 "Xdum_GFBappre"  ,
                 "Xdum_GFBspont"  ,
                 "mak_SINF"       ,
                 "mak_INFR"       ,
                 "mak_AGDR"       ,
                 "mak_GINI"       ,
                 "mak_FIND"       ,
                 "mak_LITR"       ,
                 "mak_GDPC"       ,
                 "mak_POPD"       ,
                 "mak_RURD"       ,
                 "mak_POVY"       ,
                 "mak_GDPG"       ,
                 "fe_1",
                 "fe_2",
                 "fe_3",
                 "fe_4",
                 "fe_5",
                 "fe_6",
                 "fe_7",
                 "fe_8",
                 "fe_9",
                 "fe_10",
                 "fe_11",
                 "fe_12",
                 "fe_13",
                 "fe_14",
                 "fe_15",
                 "fe_16",
                 "fe_17",
                 "fe_18",
                 "fe_19",
                 "fe_20",
                 "fe_21",
                 "fe_22",
                 "fe_23",
                 "fe_24",
                 "fe_25",
                 "fe_26",
                 "fe_27",
                 "fe_28",
                 "fe_29",
                 "fe_30",
                 "fe_31",
                 "fe_32",
                 "fe_33",
                 "fe_34")
# Bayesian Model Averaging   
library(BMS) 
K = length(df) - 1  # Number of regressors
priorK  = rep(.1,K) # Prior inclusion probabilities
set.seed(666)       # for replicability
# Model:
mod = bms(df, 
          # MCMC parameters: 
          burn   = 10000,
          iter   = 100000,
          # MCMC samplers: 
              mcmc ="rev.jump", # Reversible-jump sampler
          # Best models:
              nmodel = 100000,
          # Model size prior:
              mprior = "PIP",mprior.size = priorK, # Prior inclusion probabilities
          # g-prior:
              #g = "EBL",    # g = max(0,F_m - 1), F_m stat de cada modelos
              #g = "UIP",    # default, g = N
              #g = "RIC",    # g = K^2
              #g = "BRIC",   # g = max(N,K^2)
              #g = "hyper=2.9",# no sirve!
              #g = 1, # Marin
            g = 0.011,
             # g -> 0, tight around the prior inclusion probability
             # g -> infty+, loose around the prior inclusion probability
                          # (Posterior inclusion will tend to 1)
          g.stats = F, # statistics on the shrinkage factor g/(1+g) are collected
          start.value = K,
          force.full.ols=TRUE,
          randomizeTimer = F,
          fixed.reg = c( "fe_1",
                         "fe_2",
                         "fe_3",
                         "fe_4",
                         "fe_5",
                         "fe_6",
                         "fe_7",
                         "fe_8",
                         "fe_9",
                         "fe_10",
                         "fe_11",
                         "fe_12",
                         "fe_13",
                         "fe_14",
                         "fe_15",
                         "fe_16",
                         "fe_17",
                         "fe_18",
                         "fe_19",
                         "fe_20",
                         "fe_21",
                         "fe_22",
                         "fe_23",
                         "fe_24",
                         "fe_25",
                         "fe_26",
                         "fe_27",
                         "fe_28",
                         "fe_29",
                         "fe_30",
                         "fe_31",
                         "fe_32",
                         "fe_33",
                         "fe_34")
          )
resmod = coef(mod,std.coefs = F, order.by.pip = F, include.constant = T,  exact = TRUE)
resmod

# ==============================================================================
# Bayes factors         
library("BayesFactor")
  # Full model
        beta = data.matrix(resmod[,2])
           X = data.matrix(cbind(df[,2:ncol(df)],rep(1,nrow(df))))
        yfit = X %*% beta
        SSR = sum((y-yfit)^2)
        TSS = sum((y - mean(y))^2)
          R2 = 1 - (SSR/TSS)
    BF_full = linearReg.R2stat(nrow(df),(ncol(X)-1),R2, rscale = 1)
    BF_full[1]
    # Reduced model 
    #Variables included
    included_vars_red = c(3,6,7,10,14,16,19,20,36,37,40:74)
    beta_red = beta[included_vars_red,]
    X_red = X[,included_vars_red]
    yfit_red = X_red %*% beta_red
    SSR_red = sum((y-yfit_red)^2)
    TSS_red = sum((y - mean(y))^2)
    R2_red = 1 - (SSR_red/TSS_red)
    BF_red =linearReg.R2stat(nrow(df),(ncol(X_red)-1),R2_red, rscale = 1)
    BF_red[1]
    # Bayes factor of reduced model 1 against general model
    BFr = BF_red[[1]]/BF_full[[1]]
    BFr
    #2*log(BFr)
    #   2lnBF 	    BF 	       Strength of evidence
    # ---------------------------------------------------------
    #  0 to 2 	 1 to 3 	  Not worth more than a bare mention
    #  2 to 6 	 3 to 20 	  Positive
    #  6 to 10 	20 to 150   Strong
    #  > 10 	   > 150 	    Very strong 

# /////////////////////////////////////////////////////////////////////////////
# Additional estimates: returns on assets (ROA)
    df_ROA <- df
    df_ROA$y <- data$y_roa
    
    # Model for ROA:
    mod_ROA = bms(df_ROA, 
                   # MCMC parameters: 
                   burn   = 10000,
                   iter   = 100000,
                   # MCMC samplers: 
                   mcmc ="rev.jump", #Reversible-jump sampler
                   # Best models:
                   nmodel = 100000,
                   # Model size prior:
                   mprior = "PIP",mprior.size = priorK, # Prior inclusion probabilities
                   # g-prior:
                   #g = "EBL",    # g = max(0,F_m - 1), F_m stat de cada modelos
                   #g = "UIP",    # default, g = N
                   #g = "RIC",    # g = K^2
                   #g = "BRIC",   # g = max(N,K^2)
                   #g = "hyper=2.9",# no sirve!
                   #g = 1, # Marin
                   g = 0.011,
                   # g -> 0, tight around the prior inclusion probability
                   # g -> infty+, loose around the prior inclusion probability
                   # (Posterior inclusion will tend to 1)
                   g.stats = F, # statistics on the shrinkage factor g/(1+g) are collected
                   start.value = K,
                   force.full.ols=TRUE,
                   randomizeTimer = F,
                   fixed.reg = c( "fe_1",
                                  "fe_2",
                                  "fe_3",
                                  "fe_4",
                                  "fe_5",
                                  "fe_6",
                                  "fe_7",
                                  "fe_8",
                                  "fe_9",
                                  "fe_10",
                                  "fe_11",
                                  "fe_12",
                                  "fe_13",
                                  "fe_14",
                                  "fe_15",
                                  "fe_16",
                                  "fe_17",
                                  "fe_18",
                                  "fe_19",
                                  "fe_20",
                                  "fe_21",
                                  "fe_22",
                                  "fe_23",
                                  "fe_24",
                                  "fe_25",
                                  "fe_26",
                                  "fe_27",
                                  "fe_28",
                                  "fe_29",
                                  "fe_30",
                                  "fe_31",
                                  "fe_32",
                                  "fe_33",
                                  "fe_34")
    )
    resmod_ROA = coef(mod_ROA,std.coefs = F, order.by.pip = F, include.constant = T,  exact = TRUE)
    resmod_ROA
# ==============================================================================