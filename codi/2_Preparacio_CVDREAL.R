
rm(list=ls())

library("SNPassoc")
library("htmlwidgets")
library("compareGroups")
library("foreign")
library("lattice")
library("Hmisc")
library("ggplot2")
library("pander")
library("readxl")
library("rmarkdown")
library("knitr")
library("data.table")
library("MatchIt")
library("survival")
library("plyr")
library("survminer")
library(dplyr)

source(here::here("codi","funcions_CVDREAL.R"))


#
####                                  Llegir dades    #####
dades<-read.spss(here::here("dades","DBPACIENTSN239733_v9.sav"),use.value.labels = TRUE,to.data.frame=TRUE,trim_values=TRUE)
dades<-data.table(dades)


# Cambiar de levels
table(dades$tempsdm_cat4)
table(dades$CKD.COMBI)
levels(dades$tempsdm_cat4)<-c("<5","5-10","11-20",">20") 
levels(dades$CKD.COMBI)<-c("No","FG<60 or CAC>30")
table(dades$tempsdm_cat4)
table(dades$CKD.COMBI)

# Generar noves variables ----


####          CANVI DE CATEGORIA DE REFERENCIA            ################
dades$grup<-factor(dades$grup,levels=c("oGLD","SGLT2"))

## Generar variable Dislipemia 
# (statinas)  y /o colesterol total >200, y/o LDL>100, y/o HDL<40 para hombre y HDL<50 para mujeres y/o trigliceridos >150

dades<-dades %>% mutate(Dislipemia=case_when(FX.ESTATINAS=="Yes"~"Yes",
                              val_last.COLTOT>200~"Yes",
                              val_last.COLLDL>100~"Yes",
                              val_last.COLHDL<40 & sexe=="Male"~"Yes",
                              val_last.COLHDL<50 & sexe=="Female"~"Yes",
                              TRUE~"No"            )) 

##
##  afago mostra  ###
#
# dades<-dades[1:50000,]
##                                              ###
dadestotal<-dades
dadestotal<-etiquetar(dadestotal,taulavariables=here::here('VARIABLES_R.xls'))

####  Llegir etiquetes i variables a analitzar ####
variables <- read_excel(here::here('VARIABLES_R.xls'))
variables[is.na(variables)]<- 0
#
#
##########################        Baseline             ###################
dades<-selectorvariables("Baseline",taulavariables = here::here('VARIABLES_R.xls'),dt=dades)

################################        COMPARE GRUPS       #########################
res <- compareGroups(grup ~ .-idp, data=dades,include.miss = F)
restab<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=T)

#################     propensity            ########################################################
#### selector de dades segons propensity   ######
dades<-selectorvariables(taula="propensity",taulavariables=here::here('VARIABLES_R.xls'),dt=dadestotal)

##########  MATCHIT #################################################
set.seed(123)
##    genero la formula del propensity amb les variables D'interes
##  elimino camps --> cohort i idp de llista de variables 
llistaPS<-as.vector(names(dades)) [!as.vector(names(dades)) %in%c("idp","cohort")]
formulaPS<-as.formula(paste("cohort", paste(llistaPS, collapse=" + "), sep=" ~ "))

##  aplicar matching      #
m.out<-matchit(formulaPS,method="nearest",data=dades,caliper=0.05,ratio=1)
####    comprovar el caliper (->>0.05)####
sd(m.out$distance)*0.25
summary(m.out$distance)
summary(subset(m.out$distance,m.out$weights==1))

PS_pre<-m.out$distance
PS_post<-subset(m.out$distance,m.out$weights==1)

sd(PS_pre)*0.25
sd(PS_post)*0.25

#########################     FI MATCHING                 ###########
###       AGREGO VARIABLES A TAULA TOTAL    ###
###   afegeixo a dadestotal la variable PS 
dadestotal<-data.table(dadestotal,ps=m.out$weights)
#
#
### actualitzo descriptiu nomes amb mostra PS 
################################        COMPARE GRUPS       #########################
##  SELECT 
dades<-dadestotal %>% dplyr::filter(ps==1)


restabPS<-update(restab, x = update(res, grup ~ .-FX.MET_SGTL2 , subset = ps==1))

# restabPS<-compareGroups::descrTable(cohort ~ .-idp,data=dades,hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=T)


#
#####     fer el Compare per taula1 taula2 taula3
res <- compareGroups(formula("taula1"), data=dades,include.miss = F,include.label=T)
taula1<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=T)
taula1
#
#####     fer el compare amb taula1 taula2 taula3
res <- compareGroups(formula("taula2"), data=dades,include.miss = F,include.label=T)
taula2<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=T)
taula2

#####     fer el compare amb taula1 taula2 taula3
res <- compareGroups(formula("taula3"), data=dades,include.miss = F,include.label=T)
taula3<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=T)
taula3

####      EVENTS        ##################################

#####     fer el compare amb taula1 taula2 taula3
res <- compareGroups(formula("events"), data=dades,include.miss = F,include.label=T)
taulaevents<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=F,show.n=F,show.all=T)
taulaevents

####    survival       ##########

# ho poso en un data frame time-to-overall death variable

dadesDF<-data.frame(idp=dades$idp,
                    exitus=dades$exitus,
                    temps=dades$temps, 
                    cohort=dades$cohort,
                    EV.INSUF_CARD=dades$EV.INSUF_CARD,
                    tmp_insuf_card=dades$tmp_insuf_card,
                    EV.FA=dades$EV.FA,
                    tmp_fa= dades$tmp_fa,
                    EV.ICTUS=dades$EV.ICTUS,
                    tmp_ictus=dades$tmp_ictus,
                    EV.CVD_INF_MIO=dades$EV.CVD_INF_MIO, 
                    tmp_CVD_INF_MIO=dades$tmp_CVD_INF_MIO, 
                    grup=dades$grup,
                    temps.otd=dades$temps.otd,
                    exitus.otd=dades$exitus.otd,
                    EV.INSUF_CARD.OTD=dades$EV.INSUF_CARD.OTD,
                    tmp_insuf_card.OTD=dades$tmp_insuf_card.OTD,
                    EV.ICTUS.OTD=dades$EV.ICTUS.OTD,
                    tmp_ictus.OTD=dades$tmp_ictus.OTD,
                    EV.FA.OTD=dades$EV.FA.OTD,
                    tmp_fa.OTD=dades$tmp_fa.OTD,
                    EV.CVD_INF_MIO.OTD=dades$EV.CVD_INF_MIO.OTD,
                    tmp_cvd_inf_mio.OTD=dades$tmp_cvd_inf_mio.OTD,
                    sexe=dades$sexe,
                    age=dades$age,
                    tempsdm_cat4=dades$tempsdm_cat4,
                    HTA_PScat3=dades$HTA_PScat3,
                    IMC_cat3=dades$IMC_cat3,
                    FX.BB=dades$FX.BB,
                    FX.BBCCARD=dades$FX.BBCCARD,
                    FX.DIUTIAZ=dades$FX.DIUTIAZ,
                    FX.DTA=dades$FX.DTA,
                    FX.ARAIECA=dades$FX.ARAIECA,
                    DG.CVD_INF_MIO=dades$DG.CVD_INF_MIO,
                    DG.INSUF_CARD=dades$DG.INSUF_CARD)



# #############################################################
# dadesDF$grup<-factor(dadesDF$grup,labels=c("ISGLT2","OAD"))
# # CANVI DE CATEGORIA DE REFERENCIA  #######
# dadesDF$grup<-factor(dadesDF$grup,levels=c("OAD","ISGLT2"))

#################             ANALISIS PER ITT                #######################
dadesDF$exitus_surv<-Surv(dadesDF$temps,as.integer(dadesDF$exitus=="Yes"))
res <- compareGroups(exitus_surv~grup,data=dadesDF,include.miss = F,include.label=T)
restab_exitus.ITT<-createTable(res, show.ratio = T,hide.no = c('NA','No'), show.p.overall=F,show.n=F,show.all=F)
restab_exitus.ITT
#
dadesDF$exitus_surv<-Surv(dadesDF$tmp_insuf_card, as.integer(dadesDF$EV.INSUF_CARD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_HF.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_HF.ITT
#
dadesDF$exitus_surv<-Surv(dadesDF$tmp_fa, as.integer(dadesDF$EV.FA=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_FA.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_FA.ITT
#
dadesDF$exitus_surv<-Surv(dadesDF$tmp_ictus, as.integer(dadesDF$EV.ICTUS=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_ICTUS.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_ICTUS.ITT
#
dadesDF$exitus_surv<-Surv(dadesDF$tmp_CVD_INF_MIO, as.integer(dadesDF$EV.CVD_INF_MIO=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_IMIO.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_IMIO.ITT

#########                     ANALISIS PER OTD                       ########################
#
dadesDF$exitus_surv<-Surv(dadesDF$temps.otd,as.integer(dadesDF$exitus.otd=="Yes"))
res <- compareGroups(exitus_surv~grup,data=dadesDF,include.miss = F,include.label=T)
restab_exitus<-createTable(res, show.ratio = T,hide.no = c('NA','No'), show.p.overall=F,show.n=F,show.all=F)
restab_exitus
#
dadesDF$exitus_surv<-Surv(dades$tmp_insuf_card.OTD, as.integer(dades$EV.INSUF_CARD.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_HF<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_HF
#
dadesDF$exitus_surv<-Surv(dadesDF$tmp_fa.OTD, as.integer(dadesDF$EV.FA.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_FA<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_FA
#
dadesDF$exitus_surv<-Surv(dadesDF$tmp_ictus.OTD, as.integer(dadesDF$EV.ICTUS.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_ICTUS<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_ICTUS
#
dadesDF$exitus_surv<-Surv(dadesDF$tmp_cvd_inf_mio.OTD, as.integer(dadesDF$EV.CVD_INF_MIO.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_IMIO<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_IMIO
#

###   noves taules dels 4 nous events per ITT i per OT ###### 

dadesDF$exitus_surv<-Surv(dades$tmp_exitus.hf, as.integer(dades$EV.EXITUS.HF=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EXITUS.HF.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EXITUS.HF.ITT

# exitushf.otd=c("tmp_exitus.hf.otd","EV.EXITUS.HF.OTD"),
# exitushf.itt=c("tmp_exitus.hf","EV.EXITUS.HF"),
dadesDF$exitus_surv<-Surv(dades$tmp_exitus.hf.otd, as.integer(dades$EV.EXITUS.HF.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EXITUS.HF<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EXITUS.HF


# ictus.isq.itt=c("tmp_ictus.isq","EV.ICTUS_ISQ"),
dadesDF$exitus_surv<-Surv(dades$tmp_ictus.isq, as.integer(dades$EV.ICTUS_ISQ=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EV.ICTUS_ISQ.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EV.ICTUS_ISQ.ITT

# ictus.isq.otd=c("tmp_ictus.isq.otd","EV.ISQ.ICTUS.OTD"),
dadesDF$exitus_surv<-Surv(dades$tmp_ictus.isq.otd, as.integer(dades$EV.ISQ.ICTUS.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EV.ICTUS_ISQ<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EV.ICTUS_ISQ

# mace.mod.itt= c("tmp.ev.mace.mod","EV.MACE.MOD"),
dadesDF$exitus_surv<-Surv(dades$tmp.ev.mace.mod, as.integer(dades$EV.MACE.MOD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EV.MACE.MOD.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EV.MACE.MOD.ITT

# mace.mod.otd=  c("tmp.ev.mace.mod.otd","EV.MACE.MOD.OTD"),
dadesDF$exitus_surv<-Surv(dades$tmp.ev.mace.mod.otd, as.integer(dades$EV.MACE.MOD.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EV.MACE.MOD<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EV.MACE.MOD

# ckd.mod.itt=c("tmp_ckd","EV.CKD"))
dadesDF$exitus_surv<-Surv(dades$tmp_ckd, as.integer(dades$EV.CKD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EV.CKD.ITT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EV.CKD.ITT

# ckd.mod.otd=c("tmp_ckd.otd","EV.CKD.OTD"),
dadesDF$exitus_surv<-Surv(dades$tmp_ckd.otd, as.integer(dades$EV.CKD.OTD=="Yes"))
res <- compareGroups(exitus_surv~grup, data=dadesDF,include.miss = F,include.label=T)
restab_EV.CKD.OT<-createTable(res, show.ratio = T, hide.no = c('NA','No'), show.p.overall=F,show.n=T,show.all=F)
restab_EV.CKD.OT

#
########                  HR   CRUDE & AJUSTATS   ##########################################
#
#grup + sexe + age + tempsdm_cat4 + HTA_PScat3 + IMC_cat3 + FX.BB+ FX.BBCCARD + FX.DIUTIAZ +FX.DTA + FX.ARAIECA+ DG.CVD_INF_MIO + DG.INSUF_CARD
####      variables d'ajust     ##############
#
variables[variables$v.ajust==1,]$camp
variables[variables$v.ajust==1,]$descripcio

kable(variables[variables$v.ajust==1,]$descripcio)

#
# CANVI DE CATEGORIA DE REFERENCIA  #######

#############################################################
#############################################################
##

HRadj(x="crude",event="exitus",t="temps",d=dades,c="")

#################   HR Ajustats EVENTS I HO COLOCO EN UNA MATRIU        #####
#   PER CADA EVENT (5 EVENTS)
#   Cru, ajustat, OT, ITT 

##### Ajustar-ho per clusters 

clusters<-"idp"

#   1. Exitus  #
hr_exitus.resum<-data.frame() 
hr_exitus.resum<-rbind(hr_exitus.resum,HRadj(x="crude",event="exitus",t="temps",d=dades,c=clusters))
hr_exitus.resum<-rbind(hr_exitus.resum,HRadj(x="v.ajust",event="exitus",t="temps",d=dades,c=clusters))
hr_exitus.resum<-rbind(hr_exitus.resum,HRadj(x="crude",event="exitus.otd",t="temps.otd",c=clusters,d=dades))
hr_exitus.resum<-rbind(hr_exitus.resum,HRadj(x="v.ajust",event="exitus.otd",t="temps.otd",c=clusters,d=dades))
rownames(hr_exitus.resum)<-c(Hmisc::label(dades$exitus),"ITT adjusted","OT crude","OT adjusted")
hr_exitus.resum


# 2. EV.INSUF_CARD 
hr_ic.resum<-data.frame() 
hr_ic.resum<-rbind(hr_ic.resum,HRadj(x="crude",event="EV.INSUF_CARD",t="tmp_insuf_card",d=dades,c=clusters))
hr_ic.resum<-rbind(hr_ic.resum,HRadj(x="v.ajust",event="EV.INSUF_CARD",t="tmp_insuf_card",d=dades,c=clusters))
hr_ic.resum<-rbind(hr_ic.resum,HRadj(x="crude",event="EV.INSUF_CARD.OTD",t="tmp_insuf_card.OTD",d=dades,c=clusters))
hr_ic.resum<-rbind(hr_ic.resum,HRadj(x="v.ajust",event="EV.INSUF_CARD.OTD",t="tmp_insuf_card.OTD",d=dades,c=clusters))
rownames(hr_ic.resum)<-c(Hmisc::label(dades$EV.INSUF_CARD),"ITT adjusted","OT crude","OT adjusted")
hr_ic.resum

# 3. EV.FA
hr_fa.resum<-data.frame() 
hr_fa.resum<-rbind(hr_fa.resum,HRadj(x="crude",event="EV.FA",t="tmp_fa",d=dades,c=clusters))
hr_fa.resum<-rbind(hr_fa.resum,HRadj(x="v.ajust",event="EV.FA",t="tmp_fa",d=dades,c=clusters))
hr_fa.resum<-rbind(hr_fa.resum,HRadj(x="crude",event="EV.FA.OTD",t="tmp_fa.OTD",d=dades,c=clusters))
hr_fa.resum<-rbind(hr_fa.resum,HRadj(x="v.ajust",event="EV.FA.OTD",t="tmp_fa.OTD",d=dades,c=clusters))
rownames(hr_fa.resum)<-c(Hmisc::label(dades$EV.FA),"ITT adjusted","OT crude","OT adjusted")
hr_fa.resum


#EV.ICTUS.OTD.estrat<-HRestratificats(event="EV.ICTUS.OTD",t="tmp_ictus.OTD",tipo="v.ajust")

# 4. EV.ICTUS
hr_ictus.resum<-data.frame() 
hr_ictus.resum<-rbind(hr_ictus.resum,HRadj(x="crude",event="EV.ICTUS",t="tmp_ictus",d=dades,c=clusters))
hr_ictus.resum<-rbind(hr_ictus.resum,HRadj(x="v.ajust",event="EV.ICTUS",t="tmp_ictus",d=dades,c=clusters))
hr_ictus.resum<-rbind(hr_ictus.resum,HRadj(x="crude",event="EV.ICTUS.OTD",t="tmp_ictus.OTD",d=dades,c=clusters))
hr_ictus.resum<-rbind(hr_ictus.resum,HRadj(x="v.ajust",event="EV.ICTUS.OTD",t="tmp_ictus.OTD",d=dades,c=clusters))
rownames(hr_ictus.resum)<-c(Hmisc::label(dades$EV.ICTUS),"ITT adjusted","OT crude","OT adjusted")
hr_ictus.resum

# 5. EV.CVD_INF_MIO

hr_mi.resum<-data.frame() 
hr_mi.resum<-rbind(hr_mi.resum,HRadj(x="crude",event="EV.CVD_INF_MIO",t="tmp_CVD_INF_MIO",d=dades,c=clusters))
hr_mi.resum<-rbind(hr_mi.resum,HRadj(x="v.ajust",event="EV.CVD_INF_MIO",t="tmp_CVD_INF_MIO",d=dades,c=clusters))
hr_mi.resum<-rbind(hr_mi.resum,HRadj(x="crude",event="EV.CVD_INF_MIO.OTD",t="tmp_cvd_inf_mio.OTD",d=dades,c=clusters))
hr_mi.resum<-rbind(hr_mi.resum,HRadj(x="v.ajust",event="EV.CVD_INF_MIO.OTD",t="tmp_cvd_inf_mio.OTD",d=dades,c=clusters))
rownames(hr_mi.resum)<-c(Hmisc::label(dades$EV.CVD_INF_MIO),"ITT adjusted","OT crude","OT adjusted")
hr_mi.resum
##

HRTOTALS<-rbind(hr_exitus.resum,hr_ic.resum,hr_fa.resum,hr_ictus.resum,hr_mi.resum)


#####   Altres 4 events   ########
# 6. EV.EXITUS.HF
hr_exitusHF.resum<-data.frame() 
hr_exitusHF.resum<-rbind(hr_exitusHF.resum,HRadj(x="crude",event="EV.EXITUS.HF",t="tmp_exitus.hf",d=dades,c=clusters))
hr_exitusHF.resum<-rbind(hr_exitusHF.resum,HRadj(x="v.ajust",event="EV.EXITUS.HF",t="tmp_exitus.hf",d=dades,c=clusters))
hr_exitusHF.resum<-rbind(hr_exitusHF.resum,HRadj(x="crude",event="EV.EXITUS.HF.OTD",t="tmp_exitus.hf.otd",d=dades,c=clusters))
hr_exitusHF.resum<-rbind(hr_exitusHF.resum,HRadj(x="v.ajust",event="EV.EXITUS.HF.OTD",t="tmp_exitus.hf.otd",d=dades,c=clusters))
rownames(hr_exitusHF.resum)<-c(Hmisc::label(dades$EV.EXITUS.HF),"ITT adjusted","OT crude","OT adjusted")
hr_exitusHF.resum

#####   Altres 4 events   ########
# 7. EV.ICTUS_ISQ
hr_ictus.isq.resum<-data.frame() 
hr_ictus.isq.resum<-rbind(hr_ictus.isq.resum,HRadj(x="crude",event="EV.ICTUS_ISQ",t="tmp_ictus.isq",d=dades,c=clusters))
hr_ictus.isq.resum<-rbind(hr_ictus.isq.resum,HRadj(x="v.ajust",event="EV.ICTUS_ISQ",t="tmp_ictus.isq",d=dades,c=clusters))
hr_ictus.isq.resum<-rbind(hr_ictus.isq.resum,HRadj(x="crude",event="EV.ISQ.ICTUS.OTD",t="tmp_ictus.isq.otd",d=dades,c=clusters))
hr_ictus.isq.resum<-rbind(hr_ictus.isq.resum,HRadj(x="v.ajust",event="EV.ISQ.ICTUS.OTD",t="tmp_ictus.isq.otd",d=dades,c=clusters))
rownames(hr_ictus.isq.resum)<-c(Hmisc::label(dades$EV.ICTUS_ISQ),"ITT adjusted","OT crude","OT adjusted")
hr_ictus.isq.resum

#####   Altres 4 events   ########
# 8. EV.MACE.MOD
hr_mace.mod.resum<-data.frame() 
hr_mace.mod.resum<-rbind(hr_mace.mod.resum,HRadj(x="crude",event="EV.MACE.MOD",t="tmp.ev.mace.mod",d=dades,c=clusters))
hr_mace.mod.resum<-rbind(hr_mace.mod.resum,HRadj(x="v.ajust",event="EV.MACE.MOD",t="tmp.ev.mace.mod",d=dades,c=clusters))
hr_mace.mod.resum<-rbind(hr_mace.mod.resum,HRadj(x="crude",event="EV.MACE.MOD.OTD",t="tmp.ev.mace.mod.otd",d=dades,c=clusters))
hr_mace.mod.resum<-rbind(hr_mace.mod.resum,HRadj(x="v.ajust",event="EV.MACE.MOD.OTD",t="tmp.ev.mace.mod.otd",d=dades,c=clusters))
rownames(hr_mace.mod.resum)<-c(Hmisc::label(dades$EV.MACE.MOD),"ITT adjusted","OT crude","OT adjusted")
hr_mace.mod.resum

#####   Altres 4 events   ########
# 9. EV.CKD
hr_ckd.resum<-data.frame() 
hr_ckd.resum<-rbind(hr_ckd.resum,HRadj(x="crude",event="EV.CKD",t="tmp_ckd",d=dades,c=clusters))
hr_ckd.resum<-rbind(hr_ckd.resum,HRadj(x="v.ajust",event="EV.CKD",t="tmp_ckd",d=dades,c=clusters))
hr_ckd.resum<-rbind(hr_ckd.resum,HRadj(x="crude",event="EV.CKD.OTD",t="tmp_ckd.otd",d=dades,c=clusters))
hr_ckd.resum<-rbind(hr_ckd.resum,HRadj(x="v.ajust",event="EV.CKD.OTD",t="tmp_ckd.otd",d=dades,c=clusters))
rownames(hr_ckd.resum)<-c(Hmisc::label(dades$EV.CKD),"ITT adjusted","OT crude","OT adjusted")
hr_ckd.resum
# 

HRTOTALS_NOUS<-rbind(hr_exitusHF.resum,hr_ictus.isq.resum,hr_mace.mod.resum,hr_ckd.resum)

#
#############################      FI TAULES RESUM DE HR             ########

coxph(formulaCOX(x="crude",event="exitus",temps="temps",c="idp"),data=dadesDF)

#######       MODELS COMPLETS  #########################
coxph(formulaCOX(x="v.ajust",event="exitus",temps="temps"),data=dadesDF)
coxph(formulaCOX(x="v.ajust",event="exitus",temps="temps",c=clusters),data=dadesDF)
#
coxph(formulaCOX(x="v.ajust",event="EV.INSUF_CARD",temps="tmp_insuf_card", e=""),data=dades)
coxph(formulaCOX(x="v.ajust",event="EV.INSUF_CARD",temps="tmp_insuf_card", e="",c=clusters),data=dadesDF)
#
coxph(formulaCOX(x="v.ajust",event="EV.FA",temps="tmp_fa"),data=dades)
coxph(formulaCOX(x="v.ajust",event="EV.FA",temps="tmp_fa",c=clusters),data=dades)
#
coxph(formulaCOX(x="v.ajust",event="EV.ICTUS",temps="tmp_ictus"),data=dades)
coxph(formulaCOX(x="v.ajust",event="EV.ICTUS",temps="tmp_ictus",c=clusters),data=dades)
#
coxph(formulaCOX(x="v.ajust",event="EV.CVD_INF_MIO",temps="tmp_CVD_INF_MIO"),data=dades)
coxph(formulaCOX(x="v.ajust",event="EV.CVD_INF_MIO",temps="tmp_CVD_INF_MIO",c=clusters),data=dades)
#
#############################################################################################
#
###############       ANALISIS DE SUBGRUPS (ADJUSTED  EN OT)           #####################
#
###   variables per estratificar 
variables[variables$estratificat==1,]$camp
variables[variables$estratificat==1,]$descripcio
####                     4 ANALISIS ESTRATIFICATS OT            ########################
#####             FER ANALISIS DE SUBGRUPS NOM?S D'OTD    (ojo nom?s estratificat per dos grups)                    #############

###########################         ANALISIS DE SUBGRUPS NOM?S ADJUSTED - OTD      ######


############################      ARA GENERAR HR ESTRATIFICATS     #####################
### OT
# event="exitus.otd","temps.otd",dadesDF)
# event="EV.INSUF_CARD.OTD","tmp_insuf_card.OTD",dadesDF))
# event="EV.FA.OTD","tmp_fa.OTD",dadesDF))
# event="EV.ICTUS.OTD","tmp_ictus.OTD",dadesDF))
# event="EV.CVD_INF_MIO.OTD","tmp_cvd_inf_mio.OTD",dadesDF))
# c("Exitus OT","EV.INSUF_CARD OT","EV.FA OT","EV.ICTUS OT","EV.CVD_INF_MIO OTD")

###############       LLISTAT DE TOTS ELS EVENTS                ########### 

####  far? una llista de noms de variables surv i temps      ##############
llistaevents<-list(
             exit.otd=c("temps.otd","exitus.otd"),
             exit.itt=c("temps","exitus"),
             ic.otd=c("tmp_insuf_card.OTD","EV.INSUF_CARD.OTD"),
             ic.itt=c("tmp_insuf_card","EV.INSUF_CARD"),
             fa.otd=c("tmp_fa.OTD","EV.FA.OTD"),
             fa.itt=c("tmp_fa","EV.FA"),
             ictus.otd=c("tmp_ictus.OTD","EV.ICTUS.OTD"),
             ictus.itt=c("tmp_ictus","EV.ICTUS"),
             mi.otd=c("tmp_cvd_inf_mio.OTD","EV.CVD_INF_MIO.OTD"),
             mi.itt=c("tmp_CVD_INF_MIO","EV.CVD_INF_MIO"),
             exitushf.otd=c("tmp_exitus.hf.otd","EV.EXITUS.HF.OTD"),
             exitushf.itt=c("tmp_exitus.hf","EV.EXITUS.HF"),
             ictus.isq.otd=c("tmp_ictus.isq.otd","EV.ISQ.ICTUS.OTD"),
             ictus.isq.itt=c("tmp_ictus.isq","EV.ICTUS_ISQ"),
             mace.mod.otd=  c("tmp.ev.mace.mod.otd","EV.MACE.MOD.OTD"),
             mace.mod.itt= c("tmp.ev.mace.mod","EV.MACE.MOD"),
             ckd.mod.otd=c("tmp_ckd.otd","EV.CKD.OTD"),
             ckd.mod.itt=c("tmp_ckd","EV.CKD"))
#
dtframe_events<-tibble::tibble(event=extreure.variables(taula="event",taulavariables=here::here('VARIABLES_R.xls')),
              temps=extreure.variables(taula="temps",taulavariables=here::here('VARIABLES_R.xls')))

# afegire la resta d'events 8 mes 

###           SI HO VULL PROGRAMAR PER EVENT PUC GENERAR UN BUCLE QUE PER CADA EVENTS FACI UN ANALISIS   ######

####      event --> llistaevents[[1]][1] , temps --> llistaevents[[1]][2]
# exitus.ot.estrat<-HRestratificats(event=llistaevents[[1]][2],t=llistaevents[[1]][1],tipo="v.ajust")

exitus.ot.estrat<-HRestratificats(event="exitus.otd",t="temps.otd",tipo="v.ajust",c=clusters)
EV.INSUF_CARD.ot.estrat<-HRestratificats(event="EV.INSUF_CARD.OTD",t="tmp_insuf_card.OTD",tipo="v.ajust",c=clusters)
EV.FA.OTD.estrat<-HRestratificats(event="EV.FA.OTD",t="tmp_fa.OTD",tipo="v.ajust",c=clusters)
EV.ICTUS.OTD.estrat<-HRestratificats(event="EV.ICTUS.OTD",t="tmp_ictus.OTD",tipo="v.ajust",c=clusters)
EV.CVD_INF_MIO.OTD.estrat<-HRestratificats(event="EV.CVD_INF_MIO.OTD",t="tmp_cvd_inf_mio.OTD",tipo="v.ajust",c=clusters)

#   Buble que genera taules  estratificades en base a les llistes dels events

HR.ESTR.TOTAL<-llist("d",labels=T)
for (i in 1:length(llistaevents)) {
  HR.ESTR.TOTAL[[i]]<-HRestratificats(event=llistaevents[[i]][2],t=llistaevents[[i]][1],tipo="v.ajust",c=clusters)}

# Fer el mateix amb un map
# purrr::map2(dtframe_events$event,dtframe_events$temps,~HRestratificats(.x,.y,tipo="v-ajust",c=clusters))


###########################################################################################
#################       FALTA FIGURES K.M i taules resum    amb temps de seguiment i poblaci?

####    llista d'events utilitzats    #####

#########                     GENERO OBJECTES surv                        ########################
#
#
exitus.surv<-Surv(dadesDF$temps.otd,as.integer(dadesDF$exitus.otd=="Yes"))
IC.surv<-Surv(dades$tmp_insuf_card.OTD, as.integer(dades$EV.INSUF_CARD.OTD=="Yes"))
FA.surv<-Surv(dadesDF$tmp_fa.OTD, as.integer(dadesDF$EV.FA.OTD=="Yes"))
ICTUS.surv<-Surv(dadesDF$tmp_ictus.OTD, as.integer(dadesDF$EV.ICTUS.OTD=="Yes"))
MI.surv<-Surv(dadesDF$tmp_cvd_inf_mio.OTD, as.integer(dadesDF$EV.CVD_INF_MIO.OTD=="Yes"))
#   nous events 
exitushf.surv<-Surv(dades$tmp_exitus.hf.otd, as.integer(dades$EV.EXITUS.HF.OTD=="Yes"))
ictus.isq.surv<-Surv(dades$tmp_ictus.isq.otd, as.integer(dades$EV.ISQ.ICTUS.OTD=="Yes"))
mace.mod.surv<-Surv(dades$tmp.ev.mace.mod.otd, as.integer(dades$EV.MACE.MOD.OTD=="Yes"))
ckd.mod.surv<-Surv(dades$tmp_ckd.otd, as.integer(dades$EV.CKD.OTD=="Yes"))

#
#

##############      K-M   plot     #####
plotKM=function(y=exitus.surv,grup=grup,d=dades,caption="") {
  
  fit<- survfit(y ~ grup, data = d) 
  # Basic survival curves
  p <- ggsurvplot(fit, data = d,
                  main = "Survival curve",
                  title= caption,
                  size = 0.5,
                  ylim = c(0.95,1),
                  xlim = c(0,365),
                  break.x.by=90,
                  xlab = "Time in days",
                  risk.table = F,
                  censor.shape="|", censor.size = 1,
                  legend.labs=c("SGLT-2","oGLD")) 
  p
}
########################################################

####                        K.M DE TOTS ELS EVENTS                  ####
y=exitus.surv
plotKM(y=exitus.surv,grup=dades$grup,d=dades, caption=Hmisc::label(dades$exitus.otd))
plotKM(y=IC.surv,grup=dades$grup,d=dades,caption=Hmisc::label(dades$EV.INSUF_CARD.OTD))
plotKM(y=FA.surv,grup=dades$grup,d=dades,caption=Hmisc::label(dades$EV.FA.OTD))
plotKM(y=ICTUS.surv,grup=dades$grup,d=dades,caption=Hmisc::label(dades$EV.ICTUS.OTD))
plotKM(y=MI.surv,grup=dades$grup,d=dades,caption=Hmisc::label(dades$EV.CVD_INF_MIO.OTD))
#
########################################################     
#

fit<- survfit(exitus.surv ~ grup, data = dades)
# Basic survival curves
p <- ggsurvplot(fit, data = dades,
                main = "Survival curve",
                size = 0.5,
                ylim = c(0.95,1),
                xlim = c(0,365),
                break.x.by=90,
                xlab = "Time in days",
                risk.table = F,
                censor.shape="|", censor.size = 1,
                legend.labs=c("SGLT-2","oGLD")) 
p


#
########################################################
#
####    temps de seguiment en funci? del Fx index     ###

FU_ISGLT2<- ddply(dades, c('FX.ISGTL2_SUBTIPUS'), summarise,
               N    = length(temps),
               sum   = sum(temps/365.25),
               mean = mean(temps/365.25),
               sd   = sd(temps/365.25),
               max   = max(temps/365.25))

FU_OAD<- ddply(dades, c('GRUPFX.INDEX'), summarise,
                  N    = length(temps),
                  sum   = sum(temps/365.25),
                  mean = mean(temps/365.25),
                  sd   = sd(temps/365.25),
                  max   = max(temps/365.25))


FU_ISGLT2.OT<- ddply(dades, c('FX.ISGTL2_SUBTIPUS'), summarise,
                  N    = length(temps.otd),
                  sum   = sum(temps.otd/365.25),
                  mean = mean(temps.otd/365.25),
                  sd   = sd(temps.otd/365.25),
                  max   = max(temps.otd/365.25))

FU_OAD.OT<- ddply(dades, c('GRUPFX.INDEX'), summarise,
               N    = length(temps.otd),
               sum   = sum(temps.otd/365.25),
               mean = mean(temps.otd/365.25),
               sd   = sd(temps.otd/365.25),
               max   = max(temps.otd/365.25))




####################  FER LA TAULA RESUM D'EVENTS ######################

##  per cada event 

resumtotal<-data.frame()

for (i in 1:length(llistaevents)) {
  
  pepito<-paste("dades$",llistaevents[[i]][2],sep="")
  eventname=Hmisc::label(eval(parse(text=paste("dades$",llistaevents[[i]][2],sep=""))))
  print(eventname)
  text.temps<-llistaevents[[i]][1]
  text.event<-llistaevents[[i]][2]
  
  resum<- ddply(dades, c('grup'),summarise,
                
                Event=eventname,
                Patiens = length(eval(parse(text=text.temps))),
                PYear = sum(eval(parse(text=text.temps)))/365.25,
                N.Events= sum(eval(parse(text=text.event))=="Yes"),
                Event.rate=(N.Events/PYear)*100
                )
  
  resum
  
  ######################################################################
  
  resumtotal<-rbind(resumtotal,resum)
  
}

resumtotal


####    SALVO IMATGE    #####
save.image(here::here("CVD_REAL_OPCIO2_v11.RData"))
#





