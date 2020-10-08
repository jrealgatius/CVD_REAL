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

* GENERO IDP2 ---> IDP AGR + DATA . 
compute temp=dataindex. 
string tempo (A11).
COMPUTE tempo=STRING(temp,f11).
string IDP2 (A60).
compute idp2=CONCAT(idp,".",tempo,".",agr2).
exe.

DELETE VARIABLES temp tempo. 

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


**************         TREBALLO AMB LA BASE DE DADES DEL MARCUS DE FARMACS PRESCRITS I DISPENSATS 
**************         TREBALLO AMB LA BASE DE DADES DEL MARCUS DE FARMACS PRESCRITS I DISPENSATS 
**************         TREBALLO AMB LA BASE DE DADES DEL MARCUS DE FARMACS PRESCRITS I DISPENSATS 
**************         TREBALLO AMB LA BASE DE DADES DEL MARCUS DE FARMACS PRESCRITS I DISPENSATS 
**.

*************         DIRECTORI: T2DM16_20170817_094618. 
CD 'C:\Users\43728088M\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'G:\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'C:\Users\jreal\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.
cd 'C:\Users\Jordi\Google Drive\CIBERDEM\CVD_REAL\CVDREAL_entregable_20171114'.

*.
**************         BASE DE DADES DEL MARCUS Historic de FARMACS ISGLT2/OAD PRESCRITS I DISPENSATS 
*.
GET 
  FILE='BD_DRUGS_MARCUS.sav'. 
DATASET NAME FARMACS_INDEX WINDOW=FRONT. 
FILTER OFF. 
*.
*****         SELECCIONO NOMES ENVASOS  DISPENSATS       **.
*SELECT IF (env > 0). 
*EXECUTE.
*.
DELETE VARIABLES  selected  dataindex  FXindex  GRUPFX.INDEX  Druggroupindex prescripcio.
*.
*.
****            CANVIAR EL FORMAT DE IDP / AGR2      ****.
string temp (a12).
compute temp=AGR2. 
exe.
DELETE VARIABLES AGR2.
rename variables (temp=AGR2). 
*******.
string temp (a40).
compute temp=idp. 
exe.
DELETE VARIABLES idp.
rename variables (temp=idp). 
**************************************************.


***      faig una copia de data ini que seà la que lligarà amb dataindex       **.

compute dataindex=dataini. 
*.
****************         CAPTURAR DICOTOMICA PER MARCAR QUINS FARMACS SON INDEX I LA DATA     ***.
***      CANVIAR FORMAT AGR2    *.
DATASET ACTIVATE option2.
string temp (a12).
compute temp=AGR2. 
EXECUTE.
DELETE VARIABLES AGR2.
rename variables (temp=AGR2). 
*******.

******************         REPLICAR DRUGS EN BASE A LES REPETICIONS DEL MATEIX FX DINS DE CADA IDP *.
******         1. identificar id's - agr2 duplicats i replicar bd historic de drugs    (es la unica manera)       **.
**** marcar duplicar per id's - agr2  **.
DATASET ACTIVATE option2.
sort cases by idp agr2 dataindex.
compute replica=1.
if idp=lag(idp) and agr2=lag(agr2) replica=1+lag(replica). 
freq replica. 

****      agrego per marcar aquells registres que s'han de replicar  **.
DATASET ACTIVATE option2.
DATASET DECLARE TEMPO.
AGGREGATE
  /OUTFILE='TEMPO'
  /BREAK=idp AGR2
  /replica_max=MAX(replica).

* 2. en DRUGs capturor replica_max per replicar registres  .

DATASET ACTIVATE FARMACS_INDEX.
MATCH FILES /FILE=*
  /TABLE='TEMPO'
  /BY idp AGR2.
EXECUTE.

compute replica=1.

*   REPLICAR REGISTRES REPETITS 1=1 , 2=2, 3=3   ***.

DATASET COPY FARMACS_INDEX2.
DATASET ACTIVATE FARMACS_INDEX2.
SELECT IF  replica_max>1. 
compute replica=2.

DATASET ACTIVATE FARMACS_INDEX.

ADD FILES /FILE=*
  /FILE='FARMACS_INDEX2'.
EXECUTE.

DATASET ACTIVATE FARMACS_INDEX.
DATASET COPY FARMACS_INDEX3.
DATASET ACTIVATE FARMACS_INDEX3.
SELECT IF  replica_max>2. 
compute replica=3.

DATASET ACTIVATE FARMACS_INDEX.
ADD FILES /FILE=*
  /FILE='FARMACS_INDEX3'.
EXECUTE.


DATASET CLOSE FARMACS_INDEX3 .
DATASET CLOSE FARMACS_INDEX2 .
**************************************************************************************************************.

DATASET ACTIVATE option2.
DELETE VARIABLES replica.
sort cases by  idp2. 
******      AFEGIR IDENTIFICADOR DE DATA INDEX i FARMAC INDEX    ****.
DATASET ACTIVATE FARMACS_INDEX.         
sort cases by  idp  dataindex AGR2. 

MATCH FILES /FILE=*
  /TABLE='option2'
  /RENAME (agr codindex = d0 d1) 
  /BY idp dataindex AGR2
  /DROP= d0 d1.
EXECUTE.


****         REPE INDENTIFICA COMBINACIO INDEX (IDP DATAINDEX AGR2).
string agr2_index (A12). 
if repe>0 agr2_index=AGR2.
if repe>0 data_index=dataindex.
exe.
*.
****      AGREGO PER SELECCIONAR HISTORICS NOMÉS DELS FX INDEX (AGR2)    *.
sort cases by idp agr2 dataini. 
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=idp AGR2
  /dataindex_first=FIRST(data_index)
  /agr2_first=FIRST(agr2_index).


*** Ara selecciono només hitorics de FX index (AGR2).
*****               SELECCIONO HISTORIC NOMÉS de FX index i A PARTIR DE LA DATA D'INCLUSIÓ .
select if (agr2_first= agr2) and (dataini >= dataindex_first). 

*****               SELECCIONO HISTORIC NOMÉS A PARTIR DE LA DATA D'INCLUSIÓ 

USE ALL. 
SELECT IF (dataini >= dataindex). 
EXECUTE. 

*****         SELECCIONO NOMES ENVASOS  DISPENSATS       **.
SELECT IF (env > 0). 
EXECUTE.



***         ORDENO PER DATA INICIAL   **.
*. SORT CASES BY idp(A) agr2(A) dataini(A). 
*****AGREGO PER TENIR PER IDP + AGR2 --->PRIMERA DATA INDEX  *******.

***************** genero numero correlatiu *****************.
**** inicialitzo si es el mateix farmac .
sort cases by idp replica agr2 dataini.
compute NUM=1.
if IDP=lag(IDP) and replica=lag(replica) and AGR2=lag(AGR2)  NUM=LAG(NUM)+1.
exe.

***  TEMPS FINS PRIMERA DISPENSACIO DES DE DATA INDEX (NOMES EN 1)  ****.
IF NUM=1 temps0 = DATEDIFF(dataini, dataindex ,"days") .

*.
***               SITUACIO 1: MOLT DE TEMPS ENTRE LA INCLUSIÓ I PRIMERA DATA DE DISPENSACIO .
* SI HA PASSAT MOLT DE TEMPS (>60 DIES) + 15 DIES (DE RETARD PEL DESCONEIXEMENT DE LA DATA DE DISPENSACIO) ENTRE INCICI I PRIMERA DISPENSACIÓ DATA_STOP1=DATAINDEX   *.
*.
*         NOU CRITERI DE STOP A 75 DIES PER CONSIDERAR 2 MESOS (60 DIES ) + 15 DIES DE MARGE EN FACTURACIÓ .
*.

IF temps0>75 DATA_STOP1=dataindex. 
IF temps0>75 and NUM=1 TIPOSTOP=2. 
VALUE LABEL TIPOSTOP 2 "1a Dispensació (>75 dies) de la data de prescripció".
FREQ TIPOSTOP.

*.
***                 SITUACIO 2: TEMPS CURT ENTRE PRIMERA DISPENSACIÓ 
I  EXISTEIX ALGUN MOMENT EN QUE HI HA UN ESPAI >60 DIES

*  ARA CALCULO EL TEMPS ENTRE DATAINI I ANTERIOR FI DINS DE CADA PACIENT-FX_INDEX  *.

** identificador nou de pacient combinacio de idp + AGR2

IF IDP=LAG(IDP) and AGR2=lag(AGR2) temps2 = DATEDIFF(dataini,lag(datafi),"days") .


**************      CALCULO LA DATA STOP2 .
if TEMPS2>75 GAP75=1.
if IDP=lag(IDP)  and AGR2=lag(AGR2) and  TEMPS2>75 DATA_STOP2=LAG(datafi).
if IDP=lag(IDP)  and AGR2=lag(AGR2) and  TEMPS2>75 TIPOSTOP=1 .


RECODE TIPOSTOP (SYSMIS=0) (ELSE=COPY).
VALUE LABEL TIPOSTOP 2 "1a Dispensació (>75 dies) de la data de prescripció" 1 "Temps entre dispensacións >75 dies" 0 "No consta stop".
FREQ TIPOSTOP.


***               SITUACIO 3: NO EXISTEIX GAPS LLARGS FINS LA ULTIMA DISPENSACIÓ  *.
**         LLAVORS : DATA_STOP3 = ULTIMA_DATA DE DISPENSACIÓ (DATA_FI)    ****. 

**************         AGREGO PER PACIENT-AGR2 PER CAPTURAR INFORMACIÓ  DE LES TRES SITUACIONS POSSIBLES   ****. 
*.
*DATASET ACTIVATE HISTORICDRUGS_DISPE.
DATASET DECLARE BDIDP_DATASTOPS.
AGGREGATE
  /OUTFILE='BDIDP_DATASTOPS'
  /BREAK=idp AGR2
  /DATAINDEX=FIRST(dataindex) 
  /DATA_STOP1=FIRST(DATA_STOP1) 
  /DATA_STOP2=FIRST(DATA_STOP2) 
  /DATA_STOP3=LAST(datafi)
  /TIPOSTOP=MAX(TIPOSTOP).

*.
DATASET ACTIVATE BDIDP_DATASTOPS.
*.
*            SELECCIONO LA PRIMER PARON       *..
COMPUTE DATA_STOP=MIN(DATA_STOP1, DATA_STOP2) .
* SI NO HI HA PARON LA ULTIMA DATAFI  *.
IF MISSING(DATA_STOP) DATA_STOP=DATA_STOP3.

***              SITUACIO 4: NO EXISTEIX CAP DISPENSACIÓ DURANT EL SEGUIMENT   **.
**            LLAVORS DATA_STOP4= DATAINDEX 


***.

***         CAPTURO DE DATA INDEX PER IDENTIFICAR AQUELLS QUE NO TENEN DISPENSACIONS ***.
************         DIRECTORI: T2DM16_20170817_094618. 


****            ADD to PACIENTS la INFORMACIÓ DE DATES     ***.

DATASET ACTIVATE option2.
sort cases by idp agr2 dataindex. 


MATCH FILES /FILE=*
  /TABLE='BDIDP_DATASTOPS'
  /RENAME (DATAINDEX = d0) 
  /BY idp agr2
  /DROP= d0.
EXECUTE.

*****.
FREQ TIPOSTOP.
RECODE TIPOSTOP (SYSMIS=9) (ELSE=COPY).



*****.
FREQ TIPOSTOP.
RECODE TIPOSTOP (SYSMIS=9) (ELSE=COPY).

VALUE LABEL TIPOSTOP 2 "1a Dispensació (>75 dies) de la data de prescripció" 1 "Temps entre dispensacións >75 dies" 0 "No consta stop" 9 "No consta dispensació".
FREQ TIPOSTOP.

*.
***********************         calcular la DATA FI DE SEGUIMENT  OTD                 ******.
****                        SITUACIÓ 9. NO CONSTA DISPENSACIO (DATA_FI = DATA_INDEX) .
IF TIPOSTOP=9  DT_FI_FXFAC= dataindex .

****                        SITUACIÓ 2. DATA 1A DISPENSACIÓ >75 DIES       ****.
IF TIPOSTOP=2  DT_FI_FXFAC= dataindex .

****                        SITUACIÓ 1/0. Si consta algun STOP la data del Stop més recent li sumem 45 dies de gracia (1 mes + 15 dies de marge per facturació).
IF TIPOSTOP=1 or TIPOSTOP=0 DT_FI_FXFAC=DATESUM(min (DATA_STOP2, DATA_STOP3),45,"days", 'closest').
exe. 


*.
*******************************************************************************************************.
*.











