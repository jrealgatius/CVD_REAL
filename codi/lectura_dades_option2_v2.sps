* Me comenta que cada columna se refiere cada análisis (1, 2, 3 y 4). 
* En nuestro caso, el 1 ya lo tendríamos hecho a falta de los composite. 
* Del resto, los prioritarios por hacer son el 2 y 3, siendo el 4 opcional si finalmente diese tiempo.

*****************************                             GENERAR BASE DADES INDEX OPCIO 2                                                                      ******************.
*****************************                             GENERAR BASE DADES INDEX OPCIO 2                                                                      ******************.
*****************************                             GENERAR BASE DADES INDEX OPCIO 2                                                                      ******************.
*****************************                             GENERAR BASE DADES INDEX OPCIO 2                                                                      ******************.

CD 'C:\Users\43728088M\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
CD 'G:\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
CD 'C:\Users\jreal\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
CD 'C:\Users\Jordi\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
**.
*********************   OBRIR BASE DE DADES   MARCUS / 1. SELECCIONAR OPCIO 2 / GENERAR CAMP CLAU I VINCULAR AMB LES TAULES A AGREGAR *.
**.
************************.
*.
GET
  FILE='BD_MARCUS.sav'.
DATASET NAME BDMARCUS WINDOW=FRONT.
*.
*.
************************************                  selecciono OPCIO 2                *****************.
DATASET ACTIVATE BDMARCUS.
DATASET COPY  option2.
DATASET ACTIVATE  option2.
FILTER OFF.
USE ALL.
SELECT IF (Option2).
EXECUTE.
DATASET ACTIVATE option2.

DATASET CLOSE BDMARCUS.

DATASET ACTIVATE option2.

***.
*****   NUMERO DE REPETICIONS          *.
DELETE VARIABLES  Option1 Option2 Option3 Option4 dataindex1 dataindex3 dataindex4  dataindex2  env  selected_max datafi DATAFI1 prescripcio dataindex_max.
**.
**.
RENAME VARIABLES (dataini=dataindex).
RENAME VARIABLES (cod=codindex).
*.
***************            genero numero correlatio corresponent a les diferents dates index possibles       ***.
SORT CASES BY idp dataindex.
compute repe=1. 
if idp=lag(idp) and dataindex<>lag(dataindex) repe=1+ lag(repe). 
freq var=repe.

************         AGREGO PER TENIR UNA UNICA DATA PER PACIENT          *****.
DATASET ACTIVATE option2. 
DATASET DECLARE DINDEX_OPCIO2. 
AGGREGATE 
  /OUTFILE='DINDEX_OPCIO2' 
  /BREAK=idp repe 
  /dataindex=FIRST(dataindex).

*.
**********            AGREGO PER TENIR FILA - PACIENTS                            ***.
DATASET ACTIVATE option2. 
DATASET DECLARE DINDEX_PACIENT. 
AGGREGATE 
  /OUTFILE='DINDEX_PACIENT' 
  /BREAK=idp  
  /dataindex=FIRST(dataindex).
DATASET ACTIVATE DINDEX_PACIENT. 
DELETE VARIABLES dataindex.
COMPUTE PACIENT_SELECT=1. 
EXE.


*************         DIRECTORI: T2DM16_20170817_094618. 
CD 'C:\Users\43728088M\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'G:\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'C:\Users\jreal\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'C:\Users\Jordi\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.



*.
*.
********************************************************************      FI GENERAR BASE DE DADES INDEX OPCIÓ 2                                          ******************.
********************************************************************      FI GENERAR BASE DE DADES INDEX OPCIÓ 2                                          ******************.
********************************************************************      FI GENERAR BASE DE DADES INDEX OPCIÓ 2                                          ******************.
********************************************************************      FI GENERAR BASE DE DADES INDEX OPCIÓ 2                                          ******************.
********************************************************************      FI GENERAR BASE DE DADES INDEX OPCIÓ 2                                          ******************.

*****       HE GENERAT 3 FITXERS : 
            1.  OPTION2 (BD PACIENTS REPETITS PER DATA + AGR2 ) 
            2. DINDEX_PACIENT (174413 PACIENTS DIFERENTS)
            3. D_INDEX_OPTION2 (230004 PACIENTS - DATA  DIFERENTS)  ---> DATA INDEX .
*.
*.
*.
*.
**********************************            VARIABLES       --->                         variables_id                                                ***.
*.
*.
**********************************            VARIABLES       --->                         variables_id                                                ***.
**********************************            VARIABLES       --->                         variables_id                                                ***.
**********************************            VARIABLES       --->                         variables_id                                                ***.
*********************************************      PER FUSIONAR CADA BDADES S'HA DE GENERAR (REPLICAR) REGISTRES ABANS DE 
 FUSIONAR            ***************.
*.
*******. 
*Set working directory.               VARIABLES             ****.
*************         DIRECTORI: T2DM16_20170817_094618. 
CD 'C:\Users\43728088M\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'G:\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'C:\Users\jreal\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'C:\Users\Jordi\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.


*********************   VARIABLES               ********************.

GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_variables_20171114_150217.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=all
  /VARIABLES=
  idp A40
  cod A9
  dat A8
  val COMMA9.2.
CACHE.
EXECUTE.

DATASET NAME VARIABLES  WINDOW=FRONT.


*****************            LABORATORI         CVDREAL_entregable_laboratori_20171116_124443            ***************.
GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_laboratori_20171116_124443.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=all
  /VARIABLES=
  idp A40
  cod A9
  dat A8
  val COMMA9.2
  val_txt A1.
CACHE.
EXECUTE.

DATASET NAME LABORATORI  WINDOW=FRONT.

DATASET ACTIVATE LABORATORI.
ADD FILES /FILE=*
  /FILE='VARIABLES'.
EXECUTE.
*.
SORT CASES BY idp . 
*.

******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
*.
*******************            SELECCIONAR PACIENTS ACTIUS DE I ELIMINAR ***.
MATCH FILES /FILE=*
  /TABLE='DINDEX_PACIENT'
  /BY idp .

SELECT IF (PACIENT_SELECT).
EXE.
DELETE VARIABLES PACIENT_SELECT. 
*********************************************************************************************.
*.
****************       GENERAR COPIES I NUMERO CORRELATIU NUM      ***** **********.
COMPUTE REPE=1. 
*.
DATASET COPY LABORATORI2.
DATASET ACTIVATE  LABORATORI2.
COMPUTE REPE=2. 
*.
DATASET COPY LABORATORI3.
DATASET ACTIVATE  LABORATORI3.
COMPUTE REPE=3. 
*.
DATASET COPY LABORATORI4.
DATASET ACTIVATE  LABORATORI4.
COMPUTE REPE=4. 
*.
DATASET COPY LABORATORI5.
DATASET ACTIVATE  LABORATORI5.
COMPUTE REPE=5. 
*.
DATASET COPY LABORATORI6.
DATASET ACTIVATE  LABORATORI6.
COMPUTE REPE=6. 

***************         fusionar *.
DATASET ACTIVATE LABORATORI.
ADD FILES /FILE=*   /FILE='LABORATORI2'.
ADD FILES /FILE=*   /FILE='LABORATORI3'.
ADD FILES /FILE=*   /FILE='LABORATORI4'.
ADD FILES /FILE=*   /FILE='LABORATORI5'.
ADD FILES /FILE=*   /FILE='LABORATORI6'.
******************************************************************************************************.
freq repe.
*.
sort cases by idp repe .
**** CAPTURAR DATA INDEX  REPE  ***.
****.
*.
FORMATS  idp (A40).

********************            CAPTURO DATA INDEX NOVA ****.
*.
MATCH FILES /FILE=*
  /TABLE='DINDEX_OPCIO2'
  /BY idp repe.
EXECUTE.
*.

*******            ELIMINAR REGISTRES MISSINGS                      **********.
SELECT IF (dataindex>0).
*.
*******            TANCAR DATA SETS                      **********.
DATASET CLOSE LABORATORI2 .
DATASET CLOSE LABORATORI3 .
DATASET CLOSE LABORATORI4 .
DATASET CLOSE LABORATORI5 .
DATASET CLOSE LABORATORI6 .
**********************.
DATASET CLOSE VARIABLES. 


DATASET ACTIVATE LABORATORI.

COMPUTE datavalor=DATE.DMY(NUMBER(SUBSTRING(dat,7,2),F2.0),NUMBER(SUBSTRING(dat,5,2),F2.0),NUMBER(SUBSTRING(dat,1,4),F4.0)).

MEANS TABLES=val BY cod
  /CELLS=MIN MAX.

****************   Agregem per valor Variable-id  (Mean i ultim valor i data ultim valor)           ******************.

*** 1. Seleccionem solament les xifres previes a data index 1 ANY PREVI   *.
select if (datavalor<=dataindex and datavalor>=DATESUM(dataindex, -1, "year", 'closest')).

*.
*** 2.  Agregem per valor Variable-id  (ultim valor i data ultim valor)           ******************.
sort cases by idp repe cod datavalor.
DATASET DECLARE variables_id.
AGGREGATE
  /OUTFILE='variables_id' 
  /BREAK=idp repe cod
  /val_last=LAST(val) 
  /datavalor_last=LAST(datavalor).

***********      Reestructuro       **.
DATASET ACTIVATE variables_id.
SORT CASES BY idp repe cod.
CASESTOVARS
  /ID=idp repe
  /INDEX=cod
  /GROUPBY=VARIABLE.

DELETE VARIABLES  datavalor_last.CAC datavalor_last.CKDEPI datavalor_last.COLHDL datavalor_last.COLLDL 
    datavalor_last.COLTOT datavalor_last.CREAT datavalor_last.GLIC datavalor_last.IMC 
    datavalor_last.TAD datavalor_last.TAS datavalor_last.TG.

**********************************          FI VARIABLES       --->                         variables_id                                                ***.
**********************************          FI  VARIABLES       --->                         variables_id                                                ***.
**********************************          FI  VARIABLES       --->                         variables_id                                                ***.




*****************            TABAQUISME                     ***************.
*****************            TABAQUISME                     ***************.
*****************            TABAQUISME                     ***************.
* CVDREAL_entregable_tabaquisme_20171114_150217 .

GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_tabaquisme_20171114_150217.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=all
  /VARIABLES=
  idp A40
  dat A8
  dbaixa A8
  val F1.0.
CACHE.
EXECUTE.
DATASET NAME TABAQUISME WINDOW=FRONT.

******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
*.
*******************            SELECCIONAR PACIENTS ACTIUS DE I ELIMINAR ***.
MATCH FILES /FILE=*
  /TABLE='DINDEX_PACIENT'
  /BY idp .

SELECT IF (PACIENT_SELECT).
EXE.
DELETE VARIABLES PACIENT_SELECT. 
*********************************************************************************************.
*.
****************       GENERAR COPIES I NUMERO CORRELATIU NUM      ***** **********.
COMPUTE REPE=1. 
*.
DATASET COPY TABAQUISME2.
DATASET ACTIVATE  TABAQUISME2.
COMPUTE REPE=2. 
*.
DATASET COPY TABAQUISME3.
DATASET ACTIVATE  TABAQUISME3.
COMPUTE REPE=3. 
*.
DATASET COPY TABAQUISME4.
DATASET ACTIVATE  TABAQUISME4.
COMPUTE REPE=4. 
*.
DATASET COPY TABAQUISME5.
DATASET ACTIVATE  TABAQUISME5.
COMPUTE REPE=5. 
*.
DATASET COPY TABAQUISME6.
DATASET ACTIVATE  TABAQUISME6.
COMPUTE REPE=6. 

***************         fusionar *.
DATASET ACTIVATE TABAQUISME.
ADD FILES /FILE=*   /FILE='TABAQUISME2'.
ADD FILES /FILE=*   /FILE='TABAQUISME3'.
ADD FILES /FILE=*   /FILE='TABAQUISME4'.
ADD FILES /FILE=*   /FILE='TABAQUISME5'.
ADD FILES /FILE=*   /FILE='TABAQUISME6'.
******************************************************************************************************.
*.
sort cases by idp repe .
**** CAPTURAR DATA INDEX  REPE  ***.
********************            CAPTURO DATA INDEX NOVA ****.
*.
MATCH FILES /FILE=*
  /TABLE='DINDEX_OPCIO2'
  /BY idp repe.
EXECUTE.
*.

*******            ELIMINAR REGISTRES MISSINGS                      **********.
SELECT IF (dataindex>0).
*.
*******            TANCAR DATA SETS                      **********.
DATASET CLOSE TABAQUISME2 .
DATASET CLOSE TABAQUISME3 .
DATASET CLOSE TABAQUISME4 .
DATASET CLOSE TABAQUISME5 .
DATASET CLOSE TABAQUISME6 .
**********************.

COMPUTE datavalor=DATE.DMY(NUMBER(SUBSTRING(dat,7,2),F2.0),NUMBER(SUBSTRING(dat,5,2),F2.0),NUMBER(SUBSTRING(dat,1,4),F4.0)).

****************   Agregem per valor Variable-id  (Mean i ultim valor i data ultim valor)           ******************.
sort cases by idp repe datavalor.
DATASET DECLARE tabac_id.
AGGREGATE
  /OUTFILE='tabac_id' 
  /BREAK=idp repe
  /val_tabac=LAST(val) 
  /datatabac_last=LAST(datavalor).


DATASET CLOSE TABAQUISME .


***************************************************         FI tabac_id                                                                      **************.
***************************************************         FI tabac_id                                                                      **************.
***************************************************         FI tabac_id                                                                      **************.
***************************************************         FI tabac_id                                                                      **************.
***************************************************         FI tabac_id                                                                      **************.


***********************                                    PACIENTS                                                                         ****************.
***********************                                    PACIENTS                                                                         ****************.
***********************                                    PACIENTS                                                                         ****************.
***********************                                    PACIENTS                                                                         ****************.

***************************             PACIENTS             ************************               ***. CVDREAL_entregable_20171107_182911.

GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_pacients_20171114_150217.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  idp A40
  dnaix A8
  sexe A1
  situacio A1
  entrada A8
  sortida A8
  ruralitat A1
  qmedea A2
  up_sidiap_h F1.0.
CACHE.
EXECUTE.
DATASET NAME PACIENTS  WINDOW=FRONT.

**********.
********         FUSIONAR INFORMACIÓ DE PACIENTS A OPTION2       ****.
sort cases by idp. 
DATASET ACTIVATE option2.
MATCH FILES /FILE=*
  /TABLE='PACIENTS'
  /BY idp.
EXECUTE.


COMPUTE datanaix=DATE.DMY(NUMBER(SUBSTRING(dnaix,7,2),F2.0),NUMBER(SUBSTRING(dnaix,5,2),F2.0),NUMBER(SUBSTRING(dnaix,1,4),F4.0)).
compute age=ctime.days(dataindex-datanaix)/ 365.25.

DATASET CLOSE PACIENTS. 

DATASET COPY PACIENTS.


*********************************************                        FI PACIENTS                                                    ***********************.
****************************************************************************************************************************************************.


************************************         PROBLEMES ---> problemes_id                                                            ********************.
************************************         PROBLEMES ---> problemes_id                                                            ********************.
************************************         PROBLEMES ---> problemes_id                                                            ********************.
************************************         PROBLEMES ---> problemes_id                                                            ********************.
************************************         PROBLEMES ---> problemes_id                                                            ********************.


**********************              PROBLEMES                      *************************. 

GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_problemes_20171121_161635.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE= all
  /VARIABLES=
  idp A40
  cod A6
  dat A8
  dbaixa A8
  agr A13.
CACHE.
EXECUTE.

DATASET NAME PROBLEMES  WINDOW=FRONT.


*******************            CMBDH                                  ***************.
GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_cmbd_dx_20171123_142202.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=all
  /VARIABLES=
  idp A40
  dat A8
  dalta A8
  cod A6
  agr A13.
CACHE.
EXECUTE.
DATASET NAME CMBDH WINDOW=FRONT.


DATASET ACTIVATE CMBDH.
ADD FILES /FILE=*
  /FILE='PROBLEMES'.
EXECUTE.

DATASET CLOSE PROBLEMES. 

******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
*.
*******************            SELECCIONAR PACIENTS ACTIUS DE I ELIMINAR ***.
sort cases by idp. 
MATCH FILES /FILE=*
  /TABLE='DINDEX_PACIENT'
  /BY idp .

SELECT IF (PACIENT_SELECT).
EXE.
DELETE VARIABLES PACIENT_SELECT. 
*********************************************************************************************.
*.
****************       GENERAR COPIES I NUMERO CORRELATIU NUM      ***** **********.
COMPUTE REPE=1. 
*.
DATASET COPY CMBDH2.
DATASET ACTIVATE  CMBDH2.
COMPUTE REPE=2. 
*.
DATASET COPY CMBDH3.
DATASET ACTIVATE  CMBDH3.
COMPUTE REPE=3. 
*.
DATASET COPY CMBDH4.
DATASET ACTIVATE  CMBDH4.
COMPUTE REPE=4. 
*.
DATASET COPY CMBDH5.
DATASET ACTIVATE  CMBDH5.
COMPUTE REPE=5. 
*.
DATASET COPY CMBDH6.
DATASET ACTIVATE  CMBDH6.
COMPUTE REPE=6. 
*.
***************         fusionar *.
DATASET ACTIVATE CMBDH.
ADD FILES /FILE=*   /FILE='CMBDH2'.
ADD FILES /FILE=*   /FILE='CMBDH3'.
ADD FILES /FILE=*   /FILE='CMBDH4'.
ADD FILES /FILE=*   /FILE='CMBDH5'.
ADD FILES /FILE=*   /FILE='CMBDH6'.
*.
******************************************************************************************************.

*.
sort cases by idp repe .
**** CAPTURAR DATA INDEX  REPE  ***.
********************            CAPTURO DATA INDEX NOVA ****.
*.
MATCH FILES /FILE=*
  /TABLE='DINDEX_OPCIO2'
  /BY idp repe.
EXECUTE.
*.
*******            ELIMINAR REGISTRES MISSINGS                      **********.
SELECT IF (dataindex>0).
*.
*******            TANCAR DATA SETS                      **********.
DATASET CLOSE CMBDH2 .
DATASET CLOSE CMBDH3 .
DATASET CLOSE CMBDH4 .
DATASET CLOSE CMBDH5 .
DATASET CLOSE CMBDH6 .
**********************.


COMPUTE dataalta=DATE.DMY(NUMBER(SUBSTRING(dat,7,2),F2.0),NUMBER(SUBSTRING(dat,5,2),F2.0),NUMBER(SUBSTRING(dat,1,4),F4.0)).
COMPUTE databaixa=DATE.DMY(NUMBER(SUBSTRING(dbaixa,7,2),F2.0),NUMBER(SUBSTRING(dbaixa,5,2),F2.0),NUMBER(SUBSTRING(dbaixa,1,4),F4.0)).
DESCRIPTIVES dataalta databaixa.

if dataalta<=dataindex DG=dataalta.
if dataalta>dataindex EV=dataalta.

******               OJO ELIMINO PROBLEMES CLASSIFICATS COM A CKD (IRC/RETIDIAB) QUE NO HO SON .
*****         CKD es IRC or RETIDIAB (a excepció de E13.3)         ***.
*. 
compute ELIMINAR=0. 
if cod="E11.3" and  agr="RETIDIAB" ELIMINAR=1.
if cod="E11.3" and  agr="IRC" ELIMINAR=1.
freq agr ELIMINAR.

* selecciono pacientes .
select if (ELIMINAR=0).

freq agr ELIMINAR.

******.

**** Reestructurar per agregats de problemes de salut *****************.

*******         eliminar duplicats agregant per ID agr          ******************************. 
**.
DATASET DECLARE problemes_id.
AGGREGATE
  /OUTFILE='problemes_id'
  /BREAK=idp repe agr
  /DG=MIN(DG) 
  /EV=MIN(EV).

*******.
*******.
DATASET ACTIVATE problemes_id.
SORT CASES BY idp repe agr.
CASESTOVARS
  /ID=idp repe
  /INDEX=agr
  /GROUPBY=VARIABLE.


DATASET ACTIVATE problemes_id.

*******************         GENERO EVENT CKD                ***************.
* EV.RENDIAB  EV.IRC      *. 

IF EV.RENDIAB>0 | EV.IRC>0 EV.CKD=MIN(EV.RENDIAB,EV.IRC).

DESCRIPTIVES EV.RENDIAB EV.IRC  EV.CKD. 


compute dm.year=xdate.year(DG.E11).

* RECODE   DG.ANG_INEST DG.ANG_PEC DG.ART_PERI DG.ATT_ISQ_TRANS DG.CANCER DG.COMPDIABNoESp DG.CVD_INF_MIO 
    DG.DIALISIS DG.E10 DG.E11 DG.EPOC DG.FA DG.HEM_GRAV DG.HIPO DG.ICTUS DG.ICTUS_HEMO DG.ICTUS_ISQ 
    DG.INSUF_CARD DG.IRC DG.KETO DG.NEURDIAB DG.O24 DG.PIEDIAB DG.RENDIAB DG.RETIDIAB EV.ANG_INEST 
    EV.ANG_PEC EV.ART_PERI EV.ATT_ISQ_TRANS EV.CANCER EV.COMPDIABNoESp EV.CVD_INF_MIO EV.DIALISIS 
    EV.E10 EV.E11 EV.EPOC EV.FA EV.HEM_GRAV EV.HIPO EV.ICTUS EV.ICTUS_HEMO EV.ICTUS_ISQ EV.INSUF_CARD 
    EV.IRC EV.KETO EV.NEURDIAB EV.O24 EV.PIEDIAB EV.RENDIAB EV.RETIDIAB (0 thru hi=1) (SYSMIS=0).

*.
DATASET CLOSE CMBDH. 

*.
*****************************************************************.
*.
***************************************************         FI problemes_id                                                                      **************.
***************************************************         FI problemes_id                                                                      **************.
***************************************************         FI problemes_id                                                                      **************.
***************************************************         FI problemes_id                                                                      **************.

*******************************            FARMACS ---> FX_AGREGAT_ID                                 ***************************.
*******************************            FARMACS ---> FX_AGREGAT_ID                                 ***************************.
*******************************            FARMACS ---> FX_AGREGAT_ID                                 ***************************.
*******************************            FARMACS ---> FX_AGREGAT_ID                                 ***************************.

************************************************************************************                  GENERAR PRESCRIPCIO                                                         ****************.
*********************************************************            FACTURACIO                ******************************.
GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_facturacions_20171114_150217.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  idp A40
  dat A6
  cod A7
  agr A20
  env F1.0.
CACHE.
EXECUTE.
DATASET NAME FACTURACIO WINDOW=FRONT.

Compute facturacio=env.

*************            generar primera fecha de dispensación por grupo de farmaco *************************.
***      data de primera facturació i final de facturacio : .
DATASET ACTIVATE FACTURACIO.
compute data=DATE.DMY(15,NUMBER(SUBSTRING(dat,5,2),F2.0),NUMBER(SUBSTRING(dat,1,4),F4.0)).
compute databaixa=DATESUM(data, env, "months", 'closest').

***********************        PRESCRIPCIO                                                             ***.

* T2DM16_entregable_prescripcio_20170817_094618.

GET DATA  /TYPE=TXT
  /FILE="CVDREAL_entregable_prescripcions_20171114_150217.txt"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS="|"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  idp A40
  dat A8
  dbaixa A8
  cod A7
  agr A20.
CACHE.
EXECUTE.
DATASET NAME PRESCRIPCIO WINDOW=FRONT.

compute prescripcio=1.

***************        generar primera prescripción por grupos de farmacos      **************.
COMPUTE data=DATE.DMY(NUMBER(SUBSTRING(dat,7,2),F2.0),NUMBER(SUBSTRING(dat,5,2),F2.0),NUMBER(SUBSTRING(dat,1,4),F4.0)).
COMPUTE databaixa=DATE.DMY(NUMBER(SUBSTRING(dbaixa,7,2),F2.0),NUMBER(SUBSTRING(dbaixa,5,2),F2.0),NUMBER(SUBSTRING(dbaixa,1,4),F4.0)).


*******************         FUSIONAR        *****************.
ADD FILES /FILE=*
  /RENAME (dat dbaixa=d0 d1)
  /FILE='FACTURACIO'
  /RENAME (dat env=d2 d3)
  /DROP=d0 d1 d2 d3.
EXECUTE.



****   ordeno per idp i data **********.
sort cases by idp data databaixa.
***.
***********************            OBRIR AGREGADORS DE FARMACS                ************************.
***********************            OBRIR AGREGADORS DE FARMACS                ************************.
***********************            OBRIR AGREGADORS DE FARMACS                ************************.
GET DATA 
  /TYPE=XLS 
  /FILE='agregadors_FX.xls' 
  /SHEET=name 'AGREGADORS_FX' 
  /CELLRANGE=full 
  /READNAMES=on 
  /ASSUMEDSTRWIDTH=32767. 
EXECUTE. 
DATASET NAME agregadors_fx WINDOW=FRONT.

RECODE  AAS Acarbose ANTALD ANTIOBES ARA AVK BB BBCCARD BBCVASC CORTIS DIUTIAZ DTA ESTATINAS glitazmesu 
    Glitazones GLP1RA IAP IDPP4 IECA INSULIN_AI INSULIN_AP INSULIN_AR insulinapre ISGTL2 met_pio_DPP4 
    MET_SGTL2 METF METIG Metiglinides SU TZD (1=1) (SYSMIS=0).

rename variables (cod=temp).
string cod(a7).
compute cod=temp.
**** AGREGAR PER TENIR FILA - CODI       ***********.

DATASET DECLARE idagregadors.
AGGREGATE
  /OUTFILE='idagregadors'
  /BREAK=cod
  /AAS=SUM(AAS) 
  /Acarbose=SUM(Acarbose) 
  /ANTALD=SUM(ANTALD) 
  /ANTIOBES=SUM(ANTIOBES) 
  /ARA=SUM(ARA) 
  /AVK=SUM(AVK) 
  /BB=SUM(BB) 
  /BBCCARD=SUM(BBCCARD) 
  /BBCVASC=SUM(BBCVASC) 
  /CORTIS=SUM(CORTIS) 
  /DIUTIAZ=SUM(DIUTIAZ) 
  /DTA=SUM(DTA) 
  /ESTATINAS=SUM(ESTATINAS) 
  /glitazmesu=SUM(glitazmesu) 
  /Glitazones=SUM(Glitazones) 
  /GLP1RA=SUM(GLP1RA) 
  /IAP=SUM(IAP) 
  /IDPP4=SUM(IDPP4) 
  /IECA=SUM(IECA) 
  /INSULIN_AI=SUM(INSULIN_AI) 
  /INSULIN_AP=SUM(INSULIN_AP) 
  /INSULIN_AR=SUM(INSULIN_AR) 
  /insulinapre=SUM(insulinapre) 
  /ANY_INSULIN=SUM(ANY_INSULIN) 
  /ISGTL2=SUM(ISGTL2) 
  /met_pio_DPP4=SUM(met_pio_DPP4) 
  /MET_SGTL2=SUM(MET_SGTL2) 
  /METF=SUM(METF) 
  /METIG=SUM(METIG) 
  /Metiglinides=SUM(Metiglinides) 
  /SU=SUM(SU) 
  /TZD=SUM(TZD).
*.
********************************************************************************************************************************.
*.
DATASET ACTIVATE PRESCRIPCIO.
SORT CASES by cod.

MATCH FILES /FILE=*
  /TABLE='idagregadors'
  /BY cod.
EXECUTE.

* IDENTIFICAR GRUP OAD / ISGLT2       *.
IF Acarbose | GLP1RA | IDPP4 | INSULIN_AI | INSULIN_AP | INSULIN_AR | insulinapre | METF | METIG | SU | TZD agr="OAD".
if ISGTL2 agr="ISGTL2".
*.

SORT CASES by idp.

***.
***.
*********************************************************************************                   PRESCRIPCIO                ********************************************.


*.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
******************************            GENERAR NOVA DATA INDEX TENINT EN COMPTE REPETICIONS                                  ****.
*.
*******************            SELECCIONAR PACIENTS ACTIUS DE I ELIMINAR ***.
** SELECCIONO PACIENTS 

******************         GENERO INDICADORA DE REPETICIONS DE PACIENTS 

DATASET ACTIVATE option2.
DATASET DECLARE repeticions.
AGGREGATE
  /OUTFILE='repeticions'
  /BREAK=idp
  /repe_max=MAX(repe).
*** fusionar amb prescripcions  .
DATASET ACTIVATE PRESCRIPCIO.
MATCH FILES /FILE=*
  /TABLE='repeticions'
  /BY idp .
EXECUTE.
***** SELECT IF REPETICIONS       *.
SELECT IF (repe_max>0).
EXE.
*********************************************************************************************.
*.
****************       GENERAR COPIES I NUMERO CORRELATIU NUM      ***** **********.
DATASET COPY PRESCRIPCIOTOTAL.
DATASET ACTIVATE  PRESCRIPCIOTOTAL.
COMPUTE REPE=1. 
*.
DATASET ACTIVATE PRESCRIPCIO.
select if repe_max>=2. 
COMPUTE REPE=2. 
DATASET ACTIVATE PRESCRIPCIOTOTAL.
ADD FILES /FILE=*   /FILE='PRESCRIPCIO'.

*.
DATASET ACTIVATE PRESCRIPCIO.
select if repe_max>=3. 
COMPUTE REPE=3. 
DATASET ACTIVATE PRESCRIPCIOTOTAL.
ADD FILES /FILE=*   /FILE='PRESCRIPCIO'.

*.
DATASET ACTIVATE PRESCRIPCIO.
select if repe_max>=4. 
COMPUTE REPE=4. 
DATASET ACTIVATE PRESCRIPCIOTOTAL.
ADD FILES /FILE=*   /FILE='PRESCRIPCIO'.

*.
DATASET ACTIVATE PRESCRIPCIO.
select if repe_max>=5. 
COMPUTE REPE=5. 
DATASET ACTIVATE PRESCRIPCIOTOTAL.
ADD FILES /FILE=*   /FILE='PRESCRIPCIO'.

*.
DATASET ACTIVATE PRESCRIPCIO.
select if repe_max>=6. 
COMPUTE REPE=6. 
DATASET ACTIVATE PRESCRIPCIOTOTAL.
ADD FILES /FILE=*   /FILE='PRESCRIPCIO'.

*******            TANCAR DATA SETS                      **********.
DATASET CLOSE PRESCRIPCIO.
**********************.
**** CAPTURAR DATA INDEX  REPE  ***.
********************            CAPTURO DATA INDEX NOVA ****.
*.
sort cases by idp repe .
MATCH FILES /FILE=*
  /TABLE='DINDEX_OPCIO2'
  /BY idp repe.
EXECUTE.
*.
*******            ELIMINAR REGISTRES MISSINGS                      **********.
SELECT IF (dataindex>0).
*.

***.
**************************************************************************************************.
***.
*****************         Genero indicadores d'antecedents de FX ***************************.
**********      Farmac actiu en data index o any previ  ********.
**********      selecciono només FX prescrits en data index +- 1 any  *.
*****   Fx actiu a data index        ***.
compute dataalta=data.
*.
compute FXactiu=0.
IF (dataalta>=DATESUM(dataindex, -1, "years", 'closest') and dataalta<=DATESUM(dataindex, -1, "day", 'closest')) FXactiu=1. 
IF (databaixa>=DATESUM(dataindex, -1, "years", 'closest') and databaixa<=DATESUM(dataindex, -1, "day", 'closest')) FXactiu=1. 
IF (dataalta<=DATESUM(dataindex, -1, "years", 'closest') and databaixa>=DATESUM(dataindex, -1, "day", 'closest')) FXactiu=1. 

SELECT IF (FXactiu=1).

***************************************            AGREGO TAULA DE DE FARMACS AGREGATS *.
*.
DATASET ACTIVATE PRESCRIPCIOTOTAL.
DATASET DECLARE FX_AGREGAT_ID.
AGGREGATE
  /OUTFILE='FX_AGREGAT_ID'
  /BREAK=idp REPE
  /FX.AAS=MAX(AAS) 
  /FX.Acarbose=MAX(Acarbose) 
  /FX.ANTALD=MAX(ANTALD) 
  /FX.ANTIOBES=MAX(ANTIOBES) 
  /FX.ARA=MAX(ARA) 
  /FX.AVK=MAX(AVK) 
  /FX.BB=MAX(BB) 
  /FX.BBCCARD=MAX(BBCCARD) 
  /FX.BBCVASC=MAX(BBCVASC) 
  /FX.CORTIS=MAX(CORTIS) 
  /FX.DIUTIAZ=MAX(DIUTIAZ) 
  /FX.DTA=MAX(DTA) 
  /FX.ESTATINAS=MAX(ESTATINAS) 
  /FX.glitazmesu=MAX(glitazmesu) 
  /FX.Glitazones=MAX(Glitazones) 
  /FX.GLP1RA=MAX(GLP1RA) 
  /FX.IAP=MAX(IAP) 
  /FX.IDPP4=MAX(IDPP4) 
  /FX.IECA=MAX(IECA) 
  /FX.INSULIN_AI=MAX(INSULIN_AI) 
  /FX.INSULIN_AP=MAX(INSULIN_AP) 
  /FX.INSULIN_AR=MAX(INSULIN_AR) 
  /FX.insulinapre=MAX(insulinapre) 
  /FX.ISGTL2=MAX(ISGTL2) 
  /FX.met_pio_DPP4=MAX(met_pio_DPP4) 
  /FX.MET_SGTL2=MAX(MET_SGTL2) 
  /FX.METF=MAX(METF) 
  /FX.METIG=MAX(METIG) 
  /FX.Metiglinides=MAX(Metiglinides) 
  /FX.SU=MAX(SU) 
  /FX.TZD=MAX(TZD).

***********************************************               CAPTURA RESTA DE TAULES AFEGIR DATA INDEX I AGREGAR       **.
******************************************         FI FX_AGREGAT_ID                                                                                    ********.
******************************************         FI FX_AGREGAT_ID                                                                                    ********.
******************************************         FI FX_AGREGAT_ID                                                                                    ********.
******************************************         FI FX_AGREGAT_ID                                                                                    ********.
******************************************         FI FX_AGREGAT_ID                                                                                    ********.




*************************************************               FUSIONAR TAULES  PER CONSTRUIR TAULA DEFINITIVA        **********************************************************.

*****      S'HA DE FUSIONAR AMB LA TAULA = OPTION2       **.
**************************************************************            FUSIONEM BASE DE DADES             *****************.
*****      PACIENTS + VARIABLES + TABAC + PROBLEMES DE SALUT + CMBDH + FARMACS    ************************.

DATASET ACTIVATE PACIENTS.
sort cases by idp repe.

MATCH FILES /FILE=*
  /TABLE='variables_id'
  /BY idp repe.
EXECUTE.

MATCH FILES /FILE=*
  /TABLE='tabac_id'
  /BY idp repe.
EXECUTE.

MATCH FILES /FILE=*
  /TABLE='problemes_id'
  /BY idp repe.
EXECUTE.


MATCH FILES /FILE=*
  /TABLE='FX_AGREGAT_ID'
  /BY idp repe.
EXECUTE.

**** S'HA DE CANVIAR DE DIRECTORI .


*.
CD 'C:\Users\43728088M\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
CD 'G:\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
CD 'C:\Users\jreal\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
CD 'C:\Users\Jordi\Google Drive\CIBERDEM\CVD_REAL\OPTION2'.
**.

DATASET CLOSE OPTION2 .
DATASET CLOSE FACTURACIO.
DATASET CLOSE PRESCRIPCIOTOTAL .
DATASET CLOSE DINDEX_OPCIO2.
DATASET CLOSE IDAGREGADORS.
DATASET CLOSE FX_AGREGAT_ID.

SAVE OUTFILE='DBPACIENTS_N.sav'
  /COMPRESSED.








