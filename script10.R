rm(list=ls(all=TRUE))
packageurl <- "https://cran.r-project.org/src/contrib/Archive/mstate/mstate_0.2.7.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

#==================================================================
# code version: mstate
#==================================================================
R.version$version.string
packageDescription("mstate", fields = "Version") # Must be 0.2.7 version

#==================================================================
# Load the packages
#==================================================================
sapply(c('nlme','lattice','xtable','latticeExtra','mstate','tidyr',
         'car','ggplot2','JM'),
       library, char=T)

#==================================================================
# Load the datas
#==================================================================
mstate<-read.table("mstate.txt",h=T)

df<-read.table("df3.txt",h=T)
df$value_log <- log(df$value)

#==================================================================
# Linear mixed model:
#==================================================================
df <- df[order(df$ID),]
df$times <- df$times/365
df$Diag_rand_time <- df$Diag_rand_time/365
mstate<- mstate[,-c(32:38)]
#Multi-state model
mstate$ss1 <- ifelse(mstate$psa1<3,1,2)
mstate$ss2 <- ifelse(mstate$psa2<3,1,2)
mstate$ss3 <- ifelse(mstate$psa3<3,1,2)
mstate$ss4 <- ifelse(mstate$S4==3,3,0)
mstate$ss4[is.na(mstate$ss4)]  <- 0
#
library(dplyr)
mstate$Estado_1 <- case_when(mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 0~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 0 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 1)

mstate$Estado_2 <- case_when(mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 0 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 0~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 1)

mstate$Estado_3 <- case_when(mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 1 & mstate$ss4== 0~ 0 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 1  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 1 &
                             mstate$ss3== 2 & mstate$ss4== 0 ~ 0 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 3    ~ 1 ,
                             mstate$ss1== 2  &  mstate$ss2== 2 &
                             mstate$ss3== 1 & mstate$ss4== 0 ~ 0)

mstate$Tempo_1 <- mstate$State_1

mstate$Tempo_2 <- case_when(mstate$Estado_2 == 0 & mstate$Estado_3 == 1
                            ~ mstate$Death_rand_time,
                            mstate$Estado_2 == 1 & mstate$Estado_3 == 0
                            ~ mstate$psa2_rand_time,
                            mstate$Estado_2 == 1 & mstate$Estado_3 == 1
                            ~ mstate$psa3_rand_time,
                            mstate$Estado_2 == 0 & mstate$Estado_3 == 0
                            ~ mstate$psa3_rand_time)

mstate$Tempo_3 <- case_when(mstate$Estado_2 == 0 & mstate$Estado_3 == 1
                            ~ mstate$Death_rand_time,
                            mstate$Estado_2 == 1 & mstate$Estado_3 == 0
                            ~ mstate$psa3_rand_time,
                            mstate$Estado_2 == 1 & mstate$Estado_3 == 1
                            ~ mstate$Death_rand_time,
                            mstate$Estado_2 == 0 & mstate$Estado_3 == 0
                            ~ mstate$psa3_rand_time)

# Construct the 3*3 matrix of possible transitions:
head(mstate)
mstate$Time_1 <- mstate$Time_1/365
mstate$Time_2 <- mstate$Time_2/365
mstate$Time_3 <- mstate$Time_3/365
mstate$Tempo_1 <- mstate$Tempo_1/365
mstate$Tempo_2 <- mstate$Tempo_2/365
mstate$Tempo_3 <- mstate$Tempo_3/365

tmat <- matrix(NA, 3, 3)
tmat[1, 2:3] <- 1:2
tmat[2, 3] <- 3

dimnames(tmat) <- list(from = c("State 1","State 2", "State 3"),
                       to = c("State 1", "State 2", "State 3"))

mstate$Diag_rand_time <- mstate$Diag_rand_time/365
covs <- c("Diag_rand_time")
data_mstate <- msprep(time = c(NA,"Tempo_2",
                               "Tempo_3"),
                      status = c(NA, "Estado_2", "Estado_3"),
                      data = mstate, trans = tmat,
                      keep = covs,
                      id = "ID")

# 'expand.covs()' permits to define the set of covariates which impacts
# each transition:
data_mstate2 <- expand.covs(data_mstate, covs,
                            append = TRUE,
                            longnames = FALSE)

events(data_mstate2)$Frequencies

#=========================================================
#=========================================================

lmeFit <- lme(value_log ~  Diag_rand_time + times  #age_at_entry +
              #I(times^2)
              , data = df,
              #random = ~ 1| ID,
              #random = ~ times + I(times^2)| ID,
              random = ~ times | ID,
              control = list(opt = "optim"),na.action=na.omit)
summary(lmeFit)


coxFit <- coxph(Surv(Tstart, Tstop, status) ~
                  Diag_rand_time.1+
                  Diag_rand_time.2+
                  Diag_rand_time.3+
                  strata(trans),
                data = data_mstate2, #method = "breslow",
                x = TRUE, model = TRUE)

summary(coxFit)

#===================================================================
# Joint multi-state sub-part
#===================================================================
# Define the derivative of the fixed and random parts in the mixed model,
# and indicate which covariates are kept, for the dependency on the slope
# of the marker:
dForm <- list(fixed = ~1
              ,indFixed = 3,
              random = ~1 ,indRandom = 2)
source('JMstateModel.R')
#===================================================================
# Joint multi-state model with:
# - current level and current slope as dependence function,
# - adaptative cubic B-splines to approximate the log-baseline intensities,
# - 9 Gauss-Hermite quadrature points in the pseudo-adaptative numerical
# integration to approximate the integrals over random effects,
# - 3 Gauss-Kronrod quadrature points to approximate the integral over
# time.
#===================================================================
jointFit <- JMstateModel(lmeObject = lmeFit,
                         survObject = coxFit,
                         timeVar = "times",
                         parameterization = "both",
                         method = "spline-PH-aGH",
                         interFact = list(value =
                                         ~strata(trans) - 1,
                                          slope = ~strata(trans) - 1),
                         #data = data_mstate2,
                         derivForm = dForm,
                         Mstate = TRUE,
                         data.Mstate = data_mstate2,
                         ID.Mstate = "ID",verbose=T,
                        ,control = list(GHk = 9, lng.in.kn = 2,
                                        iter.EM = 600, iter.qN = 800,
                                        numeriDeriv='dc', eps.Hes=1e-04))
summary(jointFit)
