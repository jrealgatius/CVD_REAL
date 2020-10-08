
#  Etiquetar les variables de les dades      #####
###
etiquetar<-function(d=dadestotal,taulavariables="variables_R.xls",camp_descripcio="descripcio") {
  
  #  Llegir etiquetes i variables a analitzar ####
  variables<-read_conductor(taulavariables)
  variables<-variables %>% dplyr::filter(!is.na(camp))
  
  # selecciono els camps necessaris (camp i descripcio) i amb etiqueta
  camp_descripcio<-sym(camp_descripcio)
  
  variables<-variables %>% dplyr::select(camp,descripcio=!!camp_descripcio) 
  
  # Els que no tenen etiqueta assignar el mateix nom del camp
  variables<-variables %>% mutate(descripcio=as.character(descripcio))
  variables<-variables %>% mutate(descripcio=ifelse(descripcio=="0" | is.na(descripcio),camp,descripcio)) 
  
  # Etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  descripcio<- as.vector(seleccio$descripcio) #
  ### etiquetar variables seleccionades     #
  for (i in 1:length(descripcio)){if (any(colnames(d) == camp[i])) {Hmisc::label(d[[camp[i]]]) <- descripcio[i]}}
  d
}


#' Read conductor file different formats txt o rds o xls xlsx o data_frame tibble
#' @param fitxer Character as a file name and path or data.frame
#' 
read_conductor<-function(fitxer,...) {
  
  # Si el fitxer es un data_frame saltar
  if (any(class(fitxer) %in% c("tbl_df","tbl","data.frame"))) 
    
    dt <- tibble::as_tibble(fitxer) 
  
  else { 
    
    if (stringr::str_detect(fitxer,"\\.txt$")){
      
      dt<-data.table::fread(fitxer) %>% as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.rds$")) {
      
      dt<-readRDS(fitxer,...) %>% as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.xls$")) {
      
      dt<-readxl::read_excel(fitxer,...) %>% tidyr::as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.xlsx$")) {
      
      dt<-readxl::read_excel(fitxer,...) %>% tidyr::as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.sav$")) {
      
      dt<-foreign::read.spss(fitxer,use.value.labels = T,to.data.frame = T,...) %>% tidyr::as_tibble()
    } 
    else {stop("Data format no reconegut ")}
  }
  
}


####        FUNCIO FORMULA GENERA FORMULA A PARTIR DE VARIABLES       #######################
#####       hi envio la columna de variables amb que vull generar la formula pel compare ###########
formula=function(x="taula1") {
  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp','grup')]",sep="")
  llistataula<-eval(parse(text=pepito))
  y<-as.formula(paste("grup", paste(llistataula, collapse=" + "), sep=" ~ "))
}

#######################
#  Selector de Variables      -------
#
selectorvariables=function(taula="table1",taulavariables="variables_R.xls",dt=dadestotal) {
  
  # taula = "dades_imputacio2" 
  # taulavariables="variables_v2.xls"
  # dt=dades_test 
  
  vector_variables<-extreure.variables(taula=taula,taulavariables = taulavariables)
  
  # Selecciono les que no existeixen en DT 
  variables.no.existeixen<-vector_variables[!is.element(vector_variables,names(dt))]
  
  # Elimino les que no existeixen
  vector_variables<-vector_variables[is.element(vector_variables,names(dt))]
  moco<-dt %>% dplyr::select_at(vector_variables)
  
  message(paste0("Llista de variables que no existeixen en el dataset:",paste0(variables.no.existeixen ,collapse = ", ")))
  
  moco
}

#
extreure.variables=function(taula="table1",taulavariables="variables_R.xls",variable_camp="camp",dt=NA,...) {
  
  # taula="dates_excel"
  # taulavariables = conductor_variables
  # variable_camp="camp"
  # dt=dades
  
  ####  Llegir etiquetes i variables a analitzar ####
  # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() %>% dplyr::select(!!variable_camp,!!taula)
  variables <- read_conductor(taulavariables,...) %>% dplyr::select(!!variable_camp,!!taula)
  taula_sym<-rlang::sym(taula)
  variables<-variables %>% dplyr::filter(!is.na(!!taula_sym))
  
  # Verificar si columnes del conductor estan en dt
  if (is.data.frame(dt)) {
    vars_not_dt<-variables %>% anti_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value")) %>% pull("camp")
    variables<-variables %>% semi_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }
  
  # filtratge 
  kk<-variables %>% dplyr::arrange(!!taula_sym) %>% dplyr::filter(!!taula_sym>0) %>% dplyr::select(!!variable_camp) %>% as.vector()
  kk<-as.vector(kk[[1]])
  purrr::set_names(kk,kk)
  
}













###             formula COX ajustat per event="Yes"           ########
#formulaCOX=function(x="v.ajust",event="event",temps="temps") {
#  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp')]",sep="")
#  llistataula<-eval(parse(text=pepito))
#  resposta<-paste("Surv(",temps,", as.integer(",event,"=='Yes'))")
#  y<-as.formula(paste(resposta, paste(llistataula, collapse=" + "), sep=" ~ "))
#}


###             formula COX ajustat per event="Yes"           ########
#
###       incorpora efecte cluster        ###

formulaCOX=function(x="v.ajust",event="event",temps="temps",elimina="",cluster="") {
  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp')]",sep="")
  llistataula<-eval(parse(text=pepito))
  resposta<-paste("Surv(",temps,", as.integer(",event,"=='Yes'))")
  #
  if (cluster!="") kk<-paste(paste(llistataula,collapse=" + "),paste("cluster(",cluster,")",sep=""),sep="+")
  if (cluster=="") kk<-paste(llistataula,collapse=" + ")
  #
  # y<-as.formula(paste(resposta, paste(llistataula, collapse=" + "), sep=" ~ "))
  if (sum(elimina==llistataula)>0) y<-as.formula(paste(paste(resposta, kk , sep=" ~ "),elimina,sep=" - "))
  if (sum(elimina==llistataula)==0) y<-as.formula(paste(resposta, kk , sep=" ~ "))
  y
  
}


####      funci? que retorna Ngran, Events, coef, HR, IC951, IC952, se.coef, p 

HRadj=function(x="v.ajust",event="EV.INSUF_CARD",t="tmp_insuf_card",e="",c="",d=dadesDF) { 
  pepito<-paste("sum(d$",t,")",sep="")
  PT<-eval(parse(text=pepito))
  
  if (c=="") posicio_p=5
  if (c!="") posicio_p=6
  
  result=tryCatch({
    pp<-coxph(formulaCOX(x=x,event=event,temps=t,elimina=e,cluster=c),data=d)    
    
    cbind(PT.Year=PT/365.25,
          N=pp$n,
          EVENTS=pp$nevent,
          coef=summary(pp)$coef[1,1],
          HR=summary(pp)$coef[1,2],
          IC951=summary(pp)$conf.int[1,3],
          IC952=summary(pp)$conf.int[1,4],
          se.coef=summary(pp)$coef[1,3],
          p=summary(pp)$coef[1,posicio_p])}
    
    ,error = function(e)  {
      cbind(PT.Year=PT/365.25,
            N=0,
            EVENTS=0,
            coef=NA,
            HR=NA,
            IC951=NA,
            IC952=NA,
            se.coef=NA,
            p=NA)}
    
  )
  result
}


###   FUNCI? QUE LLAN?O event, temps adjusted i em retorna un data frame amb tot global+ estratificat  ###
#############     ENVIO exitus, temps i dades i em retorna data frame amb estratificats
####    camp estratificat conte variables estratificades tipo="v.ajust" / "crude"


HRestratificats<-function(event="exitus",t="temps",tipo="v.ajust",c="") {
  
  HRestratificats=data.frame()
  outDf<-data.frame(Subgroup="Total",HRadj(x=tipo,event=event,t=t,d=dades,c=c))
  
  # row.names(outDf)<-Hmisc::label(dades$exitus)
  row.names(outDf)<-eval(parse(text=paste("Hmisc::label(dades$",event,")",sep="")))
  
  HRestratificats <-rbind(HRestratificats,outDf)
  
  for (i in 1:length(variables[variables$estratificat==1,]$camp)) {
    outDf <-ddply(dades, variables[variables$estratificat==1,]$camp[i], function(df)  HRadj(x=tipo,event=event,t=t,d=df,c=c))
    
    row.names(outDf)<-c(paste(Hmisc::label(eval(parse(text=paste("dades$",names(outDf)[1],sep="")))),"Adj1",sep=""),
                        paste(Hmisc::label(eval(parse(text=paste("dades$",names(outDf)[1],sep="")))),"Adj2",sep=""))
    names(outDf)[1]<-paste("Subgroup")  
    
    HRestratificats <-rbind(HRestratificats,outDf)
    
  }
  #   retorna 
  HRestratificats
}
############################      FI GENERAR FUNCION           ######################
