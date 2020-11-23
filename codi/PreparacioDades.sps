*Set working directory.
*************         DIRECTORI: T2DM16_20170817_094618. 
CD 'C:\Users\43728088M\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
cd 'G:\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
cd 'C:\Users\jreal\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
cd 'C:\Users\Jordi\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.


*****************************************         RECODES I CALCULS DE VARIABLES                   *******************************.
*.
GET
  FILE='DBPACIENTS_N.sav'.
DATASET NAME PACIENTS WINDOW=FRONT.
*.

SORT CASES BY idp AGR2 DATAINDEX . 

******            FUSIONO ELS STOPS                ***.
DATASET ACTIVATE PACIENTS.
MATCH FILES /FILE=*
  /TABLE='Conjunto_de_datos4'
  /RENAME (CTIME.DAYS DATA_STOP TIPOSTOP = d0 d1 d2 ) 
  /BY idp AGR2 DATAINDEX
  /DROP= d0 d1 d2 .
EXECUTE.




****   INCLUSION CRITERIA  DM2  **************.
*–	T2DM diagnosis on or prior to the index date. 
*o	As it can be assumed that all patients treated with ATC: A10 medication are in fact diabetes patients, this criteria might be relaxed if type 1 diabetes patients can be excluded in a proper way 
*–	 >= 18 years old at index date
*–	 > 1 year data history in the database prior to the index date


*************    OJO NOMÉS S'exclou PER EDAT I PER ANTIGUITAT A SIDIAP i perDYALISIS                                     ************.

COMPUTE excl_age=0.
if age<18 excl_age=1.

COMPUTE excl_DM2=0.
if missing(DG.E11) AND missing(EV.E11) excl_DM2=1.
if DG.E11=0 AND EV.E11=0 excl_DM2=1.

COMPUTE excl_sidiap=0.
IF CTIME.DAYS(dataindex-DATE.DMY(NUMBER(SUBSTRING(entrada,7,2),F2.0),NUMBER(SUBSTRING(entrada,5,2),F2.0),NUMBER(SUBSTRING(entrada,1,4),F4.0)))<=365 excl_sidiap=1.

COMPUTE excl_dialisis=0.
if not missing(DG.DIALISIS) excl_dialisis=1.
freq excl_dialisis.

compute excl_periode=0.
if dataindex<date.dmy(01,12,2013) excl_periode=1.
freq excl_periode.

compute iyear=xdate.year(dataindex).
freq var= iyear.

compute exclusio=0.
if excl_age | excl_sidiap | excl_dialisis | excl_periode exclusio=1.

FREQ excl_age excl_DM2 excl_sidiap excl_periode EXCLUSIO excl_dialisis. 


CROSSTABS EXCLUSIO by excl_age  excl_DM2 excl_sidiap excl_dialisis excl_periode.

string grup (A8).
if agr="OAD" grup="oGLD" . 
if agr="ISGTL2" grup="SGLT-2" . 

CROSSTABS iyear by grup .


*****************               PACIENTS AMB DIALISIS QUE FER?   EN ALGUN LLOC ESPECIFICA QUE S'HA D'EXCLOURE          *****.
*****************               PACIENTS AMB DIALISIS QUE FER?   EN ALGUN LLOC ESPECIFICA QUE S'HA D'EXCLOURE          *****.
*****************               PACIENTS AMB DIALISIS QUE FER?   EN ALGUN LLOC ESPECIFICA QUE S'HA D'EXCLOURE          *****.
freq EXCLUSIO.

********      SELECT IF  ***************************.
USE ALL. 
SELECT IF (EXCLUSIO=0).

*****************************************************           RECODES    I EVENTS                                                                                                 *****.
*****************************************************           RECODES    I EVENTS                                                                                                 *****.
*****************************************************           RECODES    I EVENTS                                                                                                 *****.
*.
**********                     RECODES i CALCULS                ***********************.
*.
**********************                  tempsdesde DM                      ******.
if missing(dm.year) dm.year=xdate.year(dataindex).
compute temps_dm=ctime.days(dataindex-DG.E11)/365.25.
DESCRIPTIVES temps_dm.

if missing(temps_dm) | temps_dm<0 temps_dm=0.

RECODE  temps_dm (LO THRU 4.999999=1)  (5 THRU 10.9999=2) (11 THRU 20.9999=3) (21 THRU HI=4) INTO tempsdm_cat4. 
VALUE LABEL tempsdm_cat4  1 "<5 años" 2 "5-10 años" 3 "11-20 años" 4 ">20 años" . 
FREQ tempsdm_cat4.

****         GRUPS D'EDAT          *****.
recode age (lo thru 64.999999=0) (65 thru hi=1) into age.cat2.
value label age.cat2 0 "Age<65"  1 "Age>=65" . 
FREQUENCIES age.cat2. 

****         ARB/ACEi         *.
if FX.ARA | FX.IECA FX.ARAIECA=1.
RECODE FX.ARAIECA (1=1) (SYSMIS=0).
VALUE LABEL FX.ARAIECA 1 "Yes" 0 "No".
freq FX.ARAIECA.

***********************************************           VARIABLES QUE NECESSITO LES DADES DELS EVENTS             *********************.
*.
**********        MORTALITAT          *************************.
COMPUTE datasortida=DATE.DMY(NUMBER(SUBSTRING(sortida,7,2),F2.0),NUMBER(SUBSTRING(sortida,5,2),F2.0),NUMBER(SUBSTRING(sortida,1,4),F4.0)).
compute exitus=0.
if SITUACIO="D" exitus=1. 
value label exitus 0 "No" 1 "Yes".

freq exitus.
***********      arreglar data de sortida       ****.

* 1. Ha de ser sempre superior a data index * (si es anterior arreglarla hi poso la data maxima entre ).

DESCRIPTIVES datasortida.

**********         SI LA DATA SORTIDA ES ANTERIOR A DATA INDEX LI POSO LA MAXIMA ENTRE TOTES LES POSSIBLES DATES, SINO LA MATEIXA DATA INDEX  *. 
**************       SI LA DATA ULTIMA ES ANTERIOR A LA INCLUSIÓ REASIGNO DATA DE SORTIDA MES ENDAVANT                     ***.
**** Depurar temps temps de seguiment  ************.
if datasortida<dataindex datasortida=max(dataindex, EV.ANG_INEST, EV.ANG_PEC, EV.ART_PERI, EV.ATT_ISQ_TRANS, EV.CABG, EV.CANCER, EV.COMPDIABNoESp, 
    EV.CVD_INF_MIO, EV.DIALISIS, EV.E10, EV.E11, EV.EPOC, EV.FA, EV.HEM_GRAV, EV.HIPO, EV.ICTUS, EV.ICTUS_HEMO, 
    EV.ICTUS_ISQ, EV.INSUF_CARD, EV.IRC, EV.KETO, EV.NEUDIAB, EV.NEURDIAB, EV.O24, EV.PIEDIAB, EV.RENDIAB, 
    EV.RETIDIAB).

DESCRIPTIVES datasortida.

**********           temps de seguiment /  vida       **************************.
**********           temps de seguiment /  vida       **************************.
compute temps=ctime.days(datasortida- dataindex).
*.
DESCRIPTIVES temps.


********               CALCUL DE EVENTS I TEMPS FINS EVENTS          ******************************************************************.

* EV.CVD_INF_MIO 
EV.FA
EV.ICTUS
EV.INSUF_CARD
exitus
EV.EXITUS.HF
EV.ICTUS_ISQ
EV.MACE.MOD
EV.CKD .

***********      EV.INSUF_CARD      .
COMPUTE tmp_insuf_card=ctime.days(EV.INSUF_CARD-dataindex).
IF missing(EV.INSUF_CARD)  tmp_insuf_card =temps.
COMPUTE DT.EV.INSUF_CARD=EV.INSUF_CARD.
***********      EV.ICTUS       .
COMPUTE tmp_ictus=ctime.days(EV.ICTUS-dataindex).
IF missing(tmp_ictus) tmp_ictus=temps.
COMPUTE DT.EV.ICTUS=EV.ICTUS.
***********      EV.FA                  ****                        ***.
COMPUTE tmp_fa=ctime.days(EV.FA-dataindex).
IF missing(tmp_fa) tmp_fa=temps.
COMPUTE DT.EV.FA=EV.FA.
***********      EV.CVD_INF_MIO                             ***.
COMPUTE tmp_CVD_INF_MIO=ctime.days(EV.CVD_INF_MIO-dataindex).
IF missing(tmp_CVD_INF_MIO) tmp_CVD_INF_MIO=temps.
COMPUTE DT.EV.CVD_INF_MIO=EV.CVD_INF_MIO.
*.
*********            EVENTS QUE FALTEN : EV.EXITUS.HF EV.ISQ.ICTUS EV.MACE.MOD EV.CKD         *.
*.
* EV.EXITUS.HF , * EV.ISQ.ICTUS, * EV.MACE.MOD , * EV.CKD *.
*.
****************   EV.EXITUS.HF  tmp_exitus.hf        .
COMPUTE tmp_exitus.hf=min(temps,tmp_insuf_card) .
if EV.INSUF_CARD>0 | exitus EV.EXITUS.HF=1.
recode EV.EXITUS.HF (SYSMIS=0) (1=1).
freq EV.EXITUS.HF.
*.
means tmp_exitus.hf by EV.EXITUS.HF.
*.
**************      EV.ICTUS_ISQ tmp_isq.ictus      *.
if EV.ICTUS_ISQ>0  tmp_ictus.isq=tmp_ictus .
if missing(EV.ICTUS_ISQ) tmp_ictus.isq=temps.
***************                                                       *.

**************        EV.MACE.MOD tmp_mace.mod   *.
*.
if exitus | EV.ICTUS>0 | EV.CVD_INF_MIO>0 EV.MACE.MOD=1.
recode EV.MACE.MOD (SYSMIS=0) (1=1) . 
*.
if EV.MACE.MOD tmp.ev.mace.mod = min (tmp_ictus,temps,tmp_CVD_INF_MIO) .
if EV.MACE.MOD=0 tmp.ev.mace.mod=temps. 

****************       EV.CKD    tmp_CKD                        *.
compute DT.EV.CKD=EV.CKD. 
descriptives DT.EV.CKD.
*.
if DT.EV.CKD>= dataindex EV.CKD=1.
if DT.EV.CKD< dataindex | missing(DT.EV.CKD) EV.CKD=0.
freq EV.CKD.

*****.
***********      EV.CKD                             ***.
if EV.CKD tmp_ckd=ctime.days(DT.EV.CKD-dataindex).
DESCRIPTIVES tmp_ckd.
if EV.CKD=0 tmp_ckd=temps.
DESCRIPTIVES tmp_ckd.
***.
*************************************************************  2    ANALISIS OTD   On treatment DISPENSED                                          *********.                                  
*********************************************   TEMPS DE TRACTAMENT PER ANALISIS OT      DT_STOP    ***.
****               CALCULAR DATA FI DE SEGUIMENT             *.
***                                                                                      *.
***********************         calcular la DATA FI DE SEGUIMENT  OTD                 ******.
****.                        SITUACIÓ 9. NO CONSTA DISPENSACIO (DATA_FI = DATA_INDEX) .
IF MISSING(TIPOSTOP)  DT_FI_FXFAC= dataindex .
****                        SITUACIÓ 2. DATA 1A DISPENSACIÓ >75 DIES       ****.
IF TIPOSTOP=2  DT_FI_FXFAC= dataindex .
****                        SITUACIÓ 1/0. Si consta algun STOP la data del Stop més recent li sumem 45 dies de gracia (1 mes + 15 dies de marge per facturació).
IF TIPOSTOP=1 or TIPOSTOP=0 DT_FI_FXFAC=DATESUM(min (DATA_STOP2, DATA_STOP3),45,"days", 'closest').
exe. 

DESCRIPTIVES DT_FI_FXFAC.

** 1. La data de tractament ha de ser 31/12/2016 màxim    ***.
** 2. Ha de ser igual o menor que data datasortida            ***.
if DT_FI_FXFAC>datasortida DT_FI_FXFAC=datasortida.

DESCRIPTIVES DT_FI_FXFAC.

**********           temps de seguiment amb  tractament **************************.
compute temps.otd=ctime.days(DT_FI_FXFAC- dataindex).
*.
DESCRIPTIVES temps.otd.
*********            MORTALITAT     exitus.ot        ********.
compute exitus.otd=0.
if SITUACIO="D" and datasortida<=DT_FI_FXFAC  exitus.otd=1. 
value label exitus.otd 0 "No" 1 "Yes".

FREQUENCIES exitus.otd.
CROSSTABS exitus.otd BY exitus.

*.   DT.INSUF_CARD  DT.ICTUS  DT.FA  DT.ICTUS_HEMO  DT.ICTUS_ISQ  .
COMPUTE DT.INSUF_CARD=EV.INSUF_CARD.
COMPUTE DT.ICTUS=EV.ICTUS.
COMPUTE DT.FA=EV.FA.
COMPUTE DT.ICTUS_HEMO=EV.ICTUS_HEMO.
COMPUTE DT.ICTUS_ISQ=EV.ICTUS_ISQ.
COMPUTE DT.CVD_INF_MIO=EV.CVD_INF_MIO.
*.
***********      EV.INSUF_CARD.OTD     .
IF DT.INSUF_CARD<=DT_FI_FXFAC EV.INSUF_CARD.OTD=1.
IF DT.INSUF_CARD>=DT_FI_FXFAC OR MISSING(DT.INSUF_CARD) EV.INSUF_CARD.OTD=0.

if EV.INSUF_CARD.OTD tmp_insuf_card.OTD=ctime.days(DT.INSUF_CARD-dataindex).
IF EV.INSUF_CARD.OTD=0 tmp_insuf_card.OTD =temps.OTD.

FREQUENCIES EV.INSUF_CARD.OTD.
CROSSTABS EV.INSUF_CARD.OTD BY EV.INSUF_CARD.
*.
***********      EV.ICTUS.OTD       .
IF EV.ICTUS<=DT_FI_FXFAC EV.ICTUS.OTD=1.
IF EV.ICTUS>=DT_FI_FXFAC OR MISSING(EV.ICTUS) EV.ICTUS.OTD=0.

if EV.ICTUS.OTD tmp_ictus.OTD=ctime.days(EV.ICTUS-dataindex).
IF EV.ICTUS.OTD=0 tmp_ictus.OTD =temps.OTD.

freq EV.ICTUS.OTD.
CROSSTABS EV.ICTUS.OTD BY EV.ICTUS.
***********      EV.FA.OTD                  ****                        ***.
IF DT.FA<=DT_FI_FXFAC EV.FA.OTD=1.
IF DT.FA>=DT_FI_FXFAC OR MISSING(DT.FA) EV.FA.OTD=0.

if EV.FA.OTD tmp_fa.OTD=ctime.days(DT.FA-dataindex).
IF EV.FA.OTD=0 tmp_fa.OTD =temps.OTD.

freq EV.FA.OTD.
CROSSTABS EV.FA.OTD BY EV.FA.

***********      EV.CVD_INF_MIO.OTD            DT.CVD_INF_MIO                  ***.
IF DT.CVD_INF_MIO<=DT_FI_FXFAC EV.CVD_INF_MIO.OTD =1.
IF DT.CVD_INF_MIO>=DT_FI_FXFAC OR MISSING(DT.CVD_INF_MIO) EV.CVD_INF_MIO.OTD =0.

if EV.CVD_INF_MIO.OTD  tmp_cvd_inf_mio.OTD=ctime.days(DT.CVD_INF_MIO-dataindex).
if EV.CVD_INF_MIO.OTD =0 tmp_cvd_inf_mio.OTD =temps.OTD.

freq EV.CVD_INF_MIO.OTD.
CROSSTABS EV.CVD_INF_MIO.OTD BY EV.CVD_INF_MIO.

*****************.
****************   EV.EXITUS.HF.OTD  tmp_exitus.hf.otd        .

FREQUENCIES EV.INSUF_CARD.OTD  EV.EXITUS.HF.OTD exitus.otd.

COMPUTE tmp_exitus.hf.otd=min(temps.otd,tmp_insuf_card.otd) .
if EV.INSUF_CARD.OTD | exitus.otd EV.EXITUS.HF.OTD=1.
recode EV.EXITUS.HF.OTD (SYSMIS=0) (1=1).
means tmp_exitus.hf.otd by EV.EXITUS.HF.OTD .
*.
***********      EV.ISQ.ICTUS.OTD       .
IF DT.ICTUS_ISQ<=DT_FI_FXFAC EV.ISQ.ICTUS.OTD=1.
IF DT.ICTUS_ISQ>=DT_FI_FXFAC OR MISSING(DT.ICTUS_ISQ) EV.ISQ.ICTUS.OTD=0.

IF EV.ISQ.ICTUS.OTD tmp_ictus.isq.otd=ctime.days(DT.ICTUS_ISQ-dataindex).
IF EV.ISQ.ICTUS.OTD=0 tmp_ictus.isq.otd =temps.otd.

freq EV.ISQ.ICTUS.OTD.
CROSSTABS EV.ISQ.ICTUS.OTD BY EV.ICTUS_ISQ.

**************        EV.MACE.MOD.OTD tmp_mace.mod.otd   *.
*.
FREQ exitus.OTD EV.ICTUS.OTD EV.CVD_INF_MIO.OTD .

if exitus.OTD | EV.ICTUS.OTD | EV.CVD_INF_MIO.OTD EV.MACE.MOD.OTD=1.
recode EV.MACE.MOD.OTD (SYSMIS=0) (1=1) . 
*.
freq EV.MACE.MOD.OTD.
*.
if EV.MACE.MOD.otd tmp.ev.mace.mod.otd = min (tmp_ictus.otd,temps.otd,tmp_CVD_INF_MIO.otd) .
if EV.MACE.MOD.OTD=0 tmp.ev.mace.mod.otd=temps.otd. 

*.
**************           EV.CKD.OTD   tmp_ckd.otd            *****.
***********      EV.FA.OTD                  ****                        ***.
IF DT.EV.CKD<=DT_FI_FXFAC EV.CKD.OTD=1.
IF DT.EV.CKD>=DT_FI_FXFAC OR MISSING(DT.EV.CKD) EV.CKD.OTD=0.
if DT.EV.CKD< dataindex | missing(DT.EV.CKD) EV.CKD.OTD=0.
*.
if EV.CKD.OTD tmp_ckd.otd=ctime.days(DT.EV.CKD-dataindex).
IF EV.CKD.OTD=0 tmp_ckd.otd =temps.OTD.
*.
freq EV.CKD.OTD.
CROSSTABS EV.CKD.OTD BY EV.CKD.

************  .        

recode  EV.CVD_INF_MIO EV.FA EV.ICTUS EV.INSUF_CARD exitus EV.EXITUS.HF EV.ICTUS_ISQ EV.MACE.MOD EV.CKD (sysmis=0) (0.00001 THRU HI=1).
RECODE EV.EXITUS.HF.OTD EV.ISQ.ICTUS.OTD EV.MACE.MOD.OTD EV.CKD.OTD exitus.otd EV.INSUF_CARD.OTD EV.ICTUS.OTD EV.FA.OTD EV.CVD_INF_MIO.OTD (sysmis=0) (0.00001 THRU HI =1).

DESCRIPTIVES DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO.

recode  DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO 
    DG.ICTUS_ISQ DG.INSUF_CARD DG.IRC DG.KETO DG.NEUDIAB DG.NEURDIAB DG.O24 DG.PIEDIAB DG.RENDIAB DG.HTA
    DG.RETIDIAB EV.ANG_INEST EV.ANG_PEC EV.ART_PERI EV.ATT_ISQ_TRANS EV.CABG EV.CANCER EV.COMPDIABNoESp 
    EV.CVD_INF_MIO EV.DIALISIS EV.E10 EV.E11 EV.EPOC EV.FA EV.HEM_GRAV EV.HIPO EV.ICTUS EV.ICTUS_HEMO 
    EV.ICTUS_ISQ EV.INSUF_CARD EV.IRC EV.KETO EV.NEUDIAB EV.NEURDIAB EV.O24 EV.PIEDIAB EV.RENDIAB 
    EV.RETIDIAB (sysmis=0) (0.00001 THRU HI =1).


*.

VALUE LABEL DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO DG.INSUF_CARD 1 "Yes" 0 "No".

VALUE LABEL EV.CVD_INF_MIO EV.FA EV.ICTUS EV.INSUF_CARD exitus EV.EXITUS.HF EV.ICTUS_ISQ EV.MACE.MOD EV.CKD 1 "Yes" 0 "No".
*.
VALUE LABEL EV.EXITUS.HF.OTD EV.ISQ.ICTUS.OTD EV.MACE.MOD.OTD EV.CKD.OTD exitus.otd EV.INSUF_CARD.OTD EV.ICTUS.OTD EV.FA.OTD EV.CVD_INF_MIO.OTD 1 "Yes" 0 "No".

*****.

freq DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO DG.INSUF_CARD.

*.
FREQ EV.CVD_INF_MIO EV.FA EV.ICTUS EV.INSUF_CARD exitus EV.EXITUS.HF EV.ICTUS_ISQ EV.MACE.MOD EV.CKD.
FREQ EV.EXITUS.HF.OTD EV.ISQ.ICTUS.OTD EV.MACE.MOD.OTD EV.CKD.OTD exitus.otd EV.INSUF_CARD.OTD EV.ICTUS.OTD EV.FA.OTD EV.CVD_INF_MIO.OTD.

means temps by exitus.
means tmp_insuf_card by EV.INSUF_CARD.
means tmp_ictus by  EV.ICTUS    .
means tmp_CVD_INF_MIO by  EV.CVD_INF_MIO .
means tmp_fa by EV.FA.


*****.

CROSSTABS EV.CVD_INF_MIO BY EV.CVD_INF_MIO.OTD.
CROSSTABS EV.ICTUS BY EV.ICTUS.OTD.
CROSSTABS EV.INSUF_CARD BY EV.INSUF_CARD.OTD.
CROSSTABS EV.CKD  BY EV.CKD.OTD.
CROSSTABS EV.EXITUS.HF BY EV.EXITUS.HF.OTD.
CROSSTABS EXITUS BY EXITUS.OTD.
CROSSTABS EV.MACE.MOD  BY EV.MACE.MOD.OTD.



*.
***********************************************************************************.

DESCRIPTIVES DG.E11 EV.E11.

FREQ EXCLUSIO DG.E11 EV.E11. 
CROSSTABS DG.E11 BY EV.E11.
***      Si es event de DM es prevalent en data index ja que ja porta DM.
IF EV.E11 DG.E11=1.
freq DG.E11.

************************.
**********          iyearDM       **************.
* Calendar year at diabetes diagnosis.

**********************               CKDEPI                   ***************************.
recode   val_last.CKDEPI (lo thru 59.99999=1) (60 thru hi=0) into  CKDEPI_cat2.
value label  CKDEPI_cat2 1 "CKD-EPI<60" 0 "No" . 
*.
recode   val_last.CKDEPI (lo thru 59.99999=1) (60 thru hi=0) (SYSMIS=-1) into  CKDEPIPS_cat2.
value label  CKDEPIPS_cat2 -1 "NA "1 "CKD-EPI<60" 0 "No" . 
FREQ CKDEPI_cat2 CKDEPIPS_cat2.

**********************         CKD MANEL EN FUNCIÓ DE CKDEPI I CAC    ***************************.
*** CKDEPI_cat2=1 or  val_last.CAC>30      .
compute CKD.COMBI=0.
IF CKDEPI_cat2=1 or  val_last.CAC>30 CKD.COMBI=1.

VALUE LABEL CKD.COMBI 0 "No" 1 "CKD (FG<60 or CAC>30)".

freq CKDEPI_cat2 CKD.COMBI  DG.IRC . 

CROSSTABS DG.IRC by CKD.COMBI.



******************************************************************************************************.



*********************                  HBA1C                  ***************************.
**************************             HBA1C                                     *****.
 * HbA1c:<8% ; >=8% -10% ; >10%                                           ***.
recode val_last.HBA1C (lo thru 6.9999999=0) (7 thru hi=1)  (sysmis=-1) into HBA1CPS_cat3.
variable label  HBA1CPS_cat3 "HbA1c (%)". 
value label HBA1CPS_cat3 -1 "NA" 0 "<7%" 1 "7% or more".
freq HBA1CPS_cat3.
*.
***************************         IMC                                       *******.
recode  val_last.IMC (lo thru 24.9999=1) (25 thru 29.9999=2)  (30 thru hi=3) (SYSMIS=4) into IMC_cat4.
variable label IMC_cat4 "Obesidad (IMC)".
value labels IMC_cat4 1 "<25" 2 "25-29,9" 3 ">=30" 4 "NA" .
**.
***************************         IMC                                       *******.
recode  val_last.IMC (lo thru 29.9999=1) (30 thru hi=2) (SYSMIS=0) into IMC_cat3.
variable label IMC_cat3 "Obesidad (IMC)".
value labels IMC_cat3 1 "<30" 2  ">=30" 0 "NA" .

**********           HTA_PScat3	Hipertension (PAS/PAD>140/90)    .
IF val_last.TAS>=140 OR val_last.TAD>90 HTA_PScat3=1. 
IF val_last.TAS<140 AND val_last.TAD<=90 HTA_PScat3=0.
IF MISSING(val_last.TAS) AND MISSING(val_last.TAD) HTA_PScat3=-1.
VALUE LABELS HTA_PScat3 1 "Yes" 0 "No" -1 "NA".


**********           Presion Arterial Diastolica_PS	Hipertension (PAS/PAD>140/90)    .
IF val_last.TAD>90 PAD_cat2=1. 
IF val_last.TAD<=90 PAD_cat2=0.
IF MISSING(val_last.TAD) PAD_cat2=-1.
VALUE LABELS PAD_cat2 1 "Yes" 0 "No" -1 "NA".

freq PAD_cat2.

DESCRIPTIVES val_last.TAD.


**.
**********         iyear             *************.
compute iyear=xdate.year(dataindex).         
AUTORECODE VARIABLES=iyear 
  /INTO temporal
  /PRINT.
DELETE VARIABLES iyear.
rename variables (temporal=iyear).
freq iyear.
********       GENRAR iyearsem      SEMESTER         **. 
compute iquarter=XDATE.QUARTER(dataindex).
string isem(A2).
if iquarter<=2 isem="S1".
if iquarter>=3 isem="S2".
freq isem. 
string temp2(A4). 
compute temp2=string(xdate.year(dataindex),F4).         
freq temp2. 
string iyearsem (A6).
compute iyearsem=concatenate(temp2,isem).

*.
CROSSTABS iyearsem BY GRUP.

*.
***********               MEDEA                               ******************.
variable label qmedea "Socioeconomic factor (MEDEA index)". 
RECODE qmedea ('R'='NA') ('U'='NA') (ELSE=COPY).
EXECUTE.
value label qmedea 
'R' "Rural"
'U1' "Q1" 
'U2' "Q2" 
'U3' "Q3" 
'U4' "Q4" 
'U5' "Q5" .

freq qmedea.

*************         TABAC             **********.

FREQ  val_tabac.

*****         Etiquetes valors *.
value label val_tabac
0 "Non smoker"
1 "Smoker"
2 "Former smoker".

*.
COMPUTE smoker_ps=0.
IF val_tabac=1 smoker_ps=1.
value label smoker_ps 1 "Yes" 0 "No".
FREQ smoker_ps.

***      SGLT-2 /  oGLD      *.
freq var grup.
value label grup "ISGTL2" "SGLT-2"  "OAD" "oGLD" .
freq var grup.

***      Grup propensity       *******.
if grup="oGLD" cohort=0.
if grup="SGLT-2" cohort=1.
freq cohort.

*.
***      Microvascular            *******.
COMPUTE DG.MICROCV=0.
IF DG.NEURDIAB | DG.PIEDIAB | DG.RENDIAB | DG.RETIDIAB DG.MICROCV=1. 

*.
***      CVD (MI, ischemic heart disease, cerebrovascular disease, and peripheral arterial disease) ?.
compute DG.CVD=0.
IF  DG.INSUF_CARD | DG.CVD_INF_MIO | DG.FA | DG.HEM_GRAV | DG.ICTUS | DG.ANG_INEST | DG.ANG_PEC | DG.ART_PERI | DG.ATT_ISQ_TRANS | DG.CABG  DG.CVD=1.
freq DG.CVD.

freq dg.cvd.

*.
***   .   FX.INSULIN_AI FX.INSULIN_AP FX.INSULIN_AR FX.insulinapre .
IF FX.INSULIN_AI | FX.INSULIN_AP | FX.INSULIN_AR | FX.insulinapre FX_ANYINSULIN=1.
RECODE FX_ANYINSULIN (SYSMIS=0) (1=1).
VALUE LABEL FX_ANYINSULIN 1 "Yes" 0 "No".
FREQ FX_ANYINSULIN. 

CROSSTABS FX.INSULIN_AI FX.INSULIN_AP FX.INSULIN_AR FX.insulinapre BY FX_ANYINSULIN .

**********.


*.
***************         DG  i EV              *********************.

VALUE LABEL  DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO 
    DG.ICTUS_ISQ DG.INSUF_CARD DG.IRC DG.KETO DG.NEUDIAB DG.NEURDIAB DG.O24 DG.PIEDIAB DG.RENDIAB 
    DG.RETIDIAB DG.MICROCV DG.CVD DG.HTA 1 "Yes" 0 "No".

VALUE LABEL EV.ANG_INEST EV.ANG_PEC EV.ART_PERI EV.ATT_ISQ_TRANS EV.CABG EV.CANCER EV.COMPDIABNoESp 
    EV.CVD_INF_MIO EV.DIALISIS EV.E10 EV.E11 EV.EPOC EV.FA EV.HEM_GRAV EV.HIPO EV.ICTUS EV.ICTUS_HEMO 
    EV.ICTUS_ISQ EV.INSUF_CARD EV.IRC EV.KETO EV.NEUDIAB EV.NEURDIAB EV.O24 EV.PIEDIAB EV.RENDIAB 
    EV.RETIDIAB DG.MICROCV 1 "Yes" 0 "No".

VALUE LABEL exitus DG.MICROCV  DG.CVD 1 "Yes" 0 "No".



VALUE LABEL  FX.AAS FX.Acarbose FX.ANTALD FX.ANTIOBES FX.ARA FX.AVK FX.BB FX.BBCCARD FX.BBCVASC FX.CORTIS 
    FX.DIUTIAZ FX.DTA FX.ESTATINAS FX.glitazmesu FX.Glitazones FX.GLP1RA FX.IAP FX.IDPP4 FX.IECA 
    FX.INSULIN_AI FX.INSULIN_AP FX.INSULIN_AR FX.insulinapre FX.ISGTL2 FX.met_pio_DPP4 FX.MET_SGTL2 
    FX.METF FX.METIG FX.Metiglinides FX.SU FX.TZD 1 "Yes" 0 "No".

*****************  Els sysmis --> 0            ***.

recode   FX.AAS FX.Acarbose FX.ANTALD FX.ANTIOBES FX.ARA FX.AVK FX.BB FX.BBCCARD FX.BBCVASC FX.CORTIS 
    FX.DIUTIAZ FX.DTA FX.ESTATINAS FX.glitazmesu FX.Glitazones FX.GLP1RA FX.IAP FX.IDPP4 FX.IECA 
    FX.INSULIN_AI FX.INSULIN_AP FX.INSULIN_AR FX.insulinapre FX.ISGTL2 FX.met_pio_DPP4 FX.MET_SGTL2 
    FX.METF FX.METIG FX.Metiglinides FX.SU FX.TZD (sysmis=0) (else=copy).

descriptives FX.AAS FX.Acarbose FX.ANTALD FX.ANTIOBES FX.ARA FX.AVK FX.BB FX.BBCCARD FX.BBCVASC FX.CORTIS 
    FX.DIUTIAZ FX.DTA FX.ESTATINAS FX.glitazmesu FX.Glitazones FX.GLP1RA FX.IAP FX.IDPP4 FX.IECA 
    FX.INSULIN_AI FX.INSULIN_AP FX.INSULIN_AR FX.insulinapre FX.ISGTL2 FX.met_pio_DPP4 FX.MET_SGTL2 
    FX.METF FX.METIG FX.Metiglinides FX.SU FX.TZD.



value label sexe "D" "Female" "H" "Male".
freq sexe.




freq var DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO 
    DG.ICTUS_ISQ DG.INSUF_CARD DG.IRC DG.KETO DG.NEUDIAB DG.NEURDIAB DG.O24 DG.PIEDIAB DG.RENDIAB 
    DG.RETIDIAB. 


freq var  EV.ANG_INEST EV.ANG_PEC EV.ART_PERI EV.ATT_ISQ_TRANS EV.CABG EV.COMPDIABNoESp 
    EV.CVD_INF_MIO  EV.HEM_GRAV EV.HIPO EV.HTA EV.HTA_O EV.ICTUS 
    EV.ICTUS_HEMO EV.ICTUS_ISQ EV.INSUF_CARD EV.IRC EV.KETO EV.NEUDIAB EV.NEURDIAB EV.O24 EV.PIEDIAB 
    EV.RENDIAB EV.RETIDIAB. 


STRING GRUPFX.INDEX (a12).
COMPUTE GRUPFX.INDEX=AGR2.

VALUE LABEL GRUPFX.INDEX 
"Acarbose" "Acarbose"
"ANY_INSULIN" "Insulin" 
"GLP1RA" "GLP1RA"
"IDPP4" "DPP-4 inhibitors"
"ISGTL2" "SGLT-2 inhibitors" 
"METF" "Metformin" 
"METIG" "Metiglinides"
"SU" "Sulfonylurea" 
"TZD" "TZD".

freq GRUPFX.INDEX.


**********************************        SENSITIVITY ANALISIS        .                           ***.
IF GRUPFX.INDEX="TZD" IFX.TZD=1.
IF GRUPFX.INDEX="TZD" |  GRUPFX.INDEX="ANY_INSULIN" IFX.TZDINSU=1.
IF GRUPFX.INDEX="TZD" |  GRUPFX.INDEX="ANY_INSULIN" |  GRUPFX.INDEX="SU" IFX.TZD.INSU.SU=1.
IF GRUPFX.INDEX="GLP1RA" IFX.GLP1=1.
EXE. 

RECODE  IFX.TZD IFX.TZDINSU IFX.TZD.INSU.SU IFX.GLP1 (SYSMIS=0) (1=1). 

CROSSTABS GRUPFX.INDEX BY IFX.TZD IFX.TZDINSU IFX.TZD.INSU.SU IFX.GLP1  .


VALUE LABELS IFX.TZD
IFX.TZDINSU
IFX.TZD.INSU.SU
IFX.GLP1  1 "Yes" 0 "No".

 





*****************************************************************************************************.
*****************         TIPUS DE ISGTL2         *********.
STRING FX.ISGTL2_SUBTIPUS (A7).
IF cohort FX.ISGTL2_SUBTIPUS= codindex.
IF cohort=0 FX.ISGTL2_SUBTIPUS= "OAD".
FREQ FX.ISGTL2_SUBTIPUS.

CROSSTABS FX.ISGTL2_SUBTIPUS BY  grup  .

freq FX.ISGTL2_SUBTIPUS.

recode FX.ISGTL2_SUBTIPUS ("A10BD15"="A10BX09") ("A10BD16"="A10BX11") ("A10BD20"="A10BX12") (else=copy).
freq FX.ISGTL2_SUBTIPUS.

value label FX.ISGTL2_SUBTIPUS
"A10BX09" "Dapagliflozin"
"A10BX11" "Canagliflozin"
"A10BX12" "Empagliflozin"
"OAD" "oGLD" .
freq FX.ISGTL2_SUBTIPUS.


****************          TIPUS ANTIHTA       ***************.
IF FX.IECA | FX.BB | FX.BBCCARD | FX.BBCVASC | FX.ARA |FX.AVK FX.ANTIHTA=1. 
RECODE FX.ANTIHTA (SYSMIS=0) (1=1).
VALUE LABEL FX.ANTIHTA 1 "Yes" 0 "No".
FREQ FX.ANTIHTA. 

*.
IF FX.ARA | FX.BB | FX.BBCCARD | FX.BBCVASC | FX.DIUTIAZ | FX.DTA |  FX.IECA | FX.ANTALD  FX.ANTIHTA=1.
RECODE FX.ANTIHTA (SYSMIS=0) (1=1).
VALUE LABEL FX.ANTIHTA 1 "Yes" 0 "No".
FREQ FX.ANTIHTA. 

*.

CROSSTABS FX.ARA FX.BB FX.BBCCARD FX.BBCVASC FX.DIUTIAZ FX.DTA FX.IECA FX.ANTALD BY FX.ANTIHTA  .

*.

FREQUENCIES grup.
if agr="OAD" tempo=0 .
if agr="ISGTL2" tempo=1. 
value label tempo 0 "oGLD" 1 "SGLT2" .
freq var tempo.
DELETE VARIABLES grup. 
rename variables (tempo=grup). 
freq var grup.


SAVE OUTFILE='DBPACIENTSN239733.sav'
  /COMPRESSED.


********************      EXCLOC PACIENTS QUE INICIEN PREVIAMENT A 1/12/2013         ****.
****      RECODIFICO VARIABLE ANTECEDENTS_NOUTRACTAMENT       *******************.
* New Users de otros antidiabéticos antes de ser considerados New Users de iSGLT-2?
 Es decir, de los 13216 New Users de iSGLT-2, ¿cuántos son también New Users de otros oGLD?.

RECODE  NEW.USER (0=1) (1=0) INTO ANT_NEWUSERS.
VALUE LABEL 1 "" 

******.
IF ORDRE=1 ANT_NEWUSERS=0  .
IF ORDRE>1 ANT_NEWUSERS=1  .
VALUE LABELS ANT_NEWUSERS 0 "No" 1 "Yes" .
freq var=ANT_NEWUSERS.


CROSSTABS grup by  ANT_NEWUSERS.



***********************************************************************************************************.


**********************************               COX                ***.

DESCRIPTIVES temps.ot.

freq exitus.otd.

DATASET ACTIVATE Conjunto_de_datos1.
COXREG temps.ot
  /STATUS=exitus.otd(1)
  /CONTRAST (grup)=Indicator
  /METHOD=ENTER grup 
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).





***************            FX            ***********************.


***************         Crosstabs                         *********************.


CROSSTABS
  /TABLES=iyear BY grup
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


CROSSTABS
  /TABLES= FX.Acarbose  FX.ISGTL2 BY grup
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL. 


CROSSTABS
  /TABLES=   FX.MET_SGTL2  BY grup
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL. 

CROSSTABS
  /TABLES=    FX.met_pio_DPP4  FX.METF  FX.METIG FX.SU  BY grup
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL. 


CROSSTABS
  /TABLES= DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO 
    DG.ICTUS_ISQ DG.INSUF_CARD DG.IRC DG.KETO DG.NEUDIAB DG.NEURDIAB DG.O24 DG.PIEDIAB DG.RENDIAB 
    DG.RETIDIAB BY grup
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COL
  /COUNT ROUND CELL.


CROSSTABS / TABLES= EV.CVD_INF_MIO EV.ICTUS EV.INSUF_CARD BY GRUP
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COL
  /COUNT ROUND .




DATASET ACTIVATE PACIENTS.
* Definir conjuntos de respuestas múltiples.

* Definir conjuntos de respuestas múltiples.
MRSETS
  /MDGROUP NAME=$DIAGNOSTICOS CATEGORYLABELS=VARLABELS VARIABLES=DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CABG DG.CANCER DG.COMPDIABNoESp 
    DG.CVD_INF_MIO DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO 
    DG.ICTUS_ISQ DG.INSUF_CARD DG.IRC DG.KETO DG.NEUDIAB DG.NEURDIAB DG.O24 DG.PIEDIAB DG.RENDIAB 
    DG.RETIDIAB VALUE=1
  /DISPLAY NAME=[$DIAGNOSTICOS].




MRSETS
  /MDGROUP NAME=$EVENTOS CATEGORYLABELS=VARLABELS VARIABLES=EV.CVD_INF_MIO EV.ICTUS EV.INSUF_CARD VALUE=1
  /DISPLAY NAME=[$EVENTOS].




* Tablas personalizadas.
CTABLES
  /VLABELS VARIABLES=$DIAGNOSTICOS GRUP DISPLAY=LABEL
  /TABLE $DIAGNOSTICOS [C] BY GRUP [C][COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=$DIAGNOSTICOS  EMPTY=INCLUDE
  /CATEGORIES VARIABLES=GRUP ORDER=A KEY=VALUE EMPTY=EXCLUDE.


* Tablas personalizadas.
CTABLES
  /VLABELS VARIABLES=$EVENTOS GRUP DISPLAY=LABEL
  /TABLE $EVENTOS [C] BY GRUP [C][COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=$EVENTOS  EMPTY=INCLUDE
  /CATEGORIES VARIABLES=GRUP ORDER=A KEY=VALUE EMPTY=EXCLUDE.



