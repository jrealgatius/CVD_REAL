
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


etiquetar_taula<-function(taula=resumtotal,camp="variable",taulavariables="variables_R.xls",camp_descripcio="descripcio",idcamp="camp") {
  
  # taula=porca
  # taulavariables=conductor
  # camp="Parameter"
  # camp_descripcio="desc_model"
  # idcamp="camp2"
  
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- read_conductor(taulavariables)
  camp_sym<-sym(camp)
  idcamp_sym<-sym(camp_sym)
  
  # Canviar nom de camp de variables al de la taula 
  # colnames(variables)[colnames(variables)=="camp"] <- camp
  colnames(variables)[colnames(variables)==idcamp] <- camp
  
  # Canviar arguments per ser evaluats
  camp_eval<-sym(camp)
  camp_descripcio_eval<-sym(camp_descripcio)
  # Canviar el format de la taula 
  taula %>% left_join(dplyr::select(variables,c(!!camp_eval,camp_descripcio)),by=quo_name(camp_eval)) %>% 
    dplyr::rename(descripcio:=!!camp_descripcio) %>% 
    mutate(!!camp_eval:=descripcio) %>% 
    dplyr::select(-descripcio)
  
}

# Cambia 

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

etiquetar_vector<-function(vector=vector_variables,camp="camp",taulavariables="variables_R.xls",camp_descripcio="descripcio",...) {
  
  # vector=v1
  # taulavariables=conductor_variables
  # camp="camp"
  # camp_descripcio="descripcio2"
  
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- read_conductor(taulavariables,...) 
  camp_sym<-sym(camp)
  variables<-variables %>% dplyr::filter(!is.na(!!camp_sym))
  
  # Canviar nom de camp de variables al de la taula 
  colnames(variables)[colnames(variables)=="camp"] <- camp
  # Canviar arguments per ser evaluats
  camp_eval<-sym(camp)
  camp_descripcio_eval<-sym(camp_descripcio)
  
  vectorX<-vector %>% tibble::tibble(value=.)  # Convertir vector a tibble i filtrar
  variablesX<-variables %>% dplyr::semi_join(vectorX, by=c("camp"="value")) # Filtrar només variables vector

  stats::setNames(object=dplyr::pull(variablesX,!!camp_descripcio_eval),variablesX$camp)
  
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


# Funció forest.plot 

forest.plot.HR<-function(dadesmodel=dt_estimacions,label="Categoria",mean="estimate",lower="Linf",upper="Lsup",label_X="OR (95% CI)",
                         intercept=0,
                         nivell="outcome", factor1="type",color=TRUE, label_Xvertical="Cardiovascular event") {
  
  # dadesmodel=dt_temp
  # label="grups"
  # mean="HR"
  # lower="IC951"
  # upper="IC952"
  # label_X="HR (95% CI)"
  # intercept=1
  # nivell="outcome"
  # factor1="Method"
  # color=F
  # label_Xvertical="Cardiovascular event"
  
  # Generar data set 
  dadesmodel <- dadesmodel %>% select(valor=!!mean,Linf=!!lower,Lsup=!!upper,nivell=!!nivell, factor1=!!factor1)
  
  ## Preparar taula (Genero etiqueta)
  taula_betas<-dadesmodel %>% mutate(etiqueta=paste0("     ",factor1),
                                     Group = paste0(factor1))
  
  # Afegir fila com un punt nivell per outcome i genero label de group
  taula_betas<-taula_betas %>% split(.$nivell) %>% 
    map_dfr(~add_row(.x,.before = 0),.id = "outcome" ) %>% 
    mutate (etiqueta2=if_else(is.na(etiqueta),outcome,"")) %>% 
    mutate (etiqueta=if_else(is.na(etiqueta),outcome,etiqueta))
  
  # AFegir etiqueta 3 mes centrada
  taula_betas<-taula_betas %>% mutate(etiqueta3=lag(etiqueta2),
                                      etiqueta3=if_else(is.na(etiqueta3),"",etiqueta3))
  
  # Reordenar outcomes segons origen de taula inicial
  dt_ordre<-dadesmodel %>% distinct(outcome=nivell) %>% mutate(seq=seq(1:n()))
  taula_betas<-taula_betas %>% left_join(dt_ordre,by="outcome") %>% arrange(seq)
  
  # Generar id 
  taula_betas<-taula_betas %>% mutate(id=seq(n())) %>% mutate(id=n()-id+1)
  
  # REomplir missings en factor1 i factor2
  taula_betas<-taula_betas %>% fill(c(factor1,Group),.direction="updown")
  
  # Relevel mateix ordre tal com surt taula   
  ordre_levels<-taula_betas %>% pull(Group) %>% unique()
  taula_betas$Group<-factor(taula_betas$Group, levels = ordre_levels)
  
  
  fp <- ggplot(data=taula_betas,aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    geom_pointrange(size=0.5) + 
    geom_hline(yintercept=intercept, lty=1,colour="grey") +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(label_Xvertical) + ylab(label_X) +
    scale_x_continuous(breaks=taula_betas %>% pull(id),labels=taula_betas %>% pull(etiqueta))
  
  fp<-fp + theme_minimal() + theme(axis.text.y = element_text(hjust = 0,vjust=0,size=10)) 
  
  # if (color) {fp<-fp + geom_point(aes(color=Group),size=3)} else 
  # {fp<-fp + geom_point(aes(shape=Group),size=3)}
  
  # Add banda d'error
  # fp<-fp + geom_hline(yintercept = c(intercept+0.1,intercept-0.1),linetype=2)
  
  fp 
  
}


prep_dades_m.out_covariate<-function(dt=m.out,vars_remove=NULL,etiquetar=F,...){
  
  # Preparar dades a plotejar
  dt_pre<-summary(dt,standardize = T)$sum.all %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Unmatched",id=dplyr::row_number())
  dt_post<-summary(dt,standardize = T)$sum.matched %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Matched",id=dplyr::row_number())
  
  # Preparar i ordenar per id
  dt_total<- 
    dt_pre %>% dplyr::bind_rows(dt_post) %>% 
    mutate (stat=`Std. Mean Diff.`) %>% 
    dplyr::filter(var!="distance") %>% 
    dplyr::filter(!is.na(stat)) %>% 
    mutate(var=factor(var,levels=rev(dt_pre$var)))   # Convertir a factor per que surti ordenat
  
  # Generar variables+nivells indexat
  llista_vars<-all.vars(stats::formula(m.out$model)[-2])
  vars_df<-
    llista_vars %>% set_names(llista_vars) %>% 
    purrr::map(~levels(dades[[.x]])) %>% 
    tibble::enframe() %>% 
    tidyr::unnest(cols = c(value))
  
  vars_df<-
    tibble::as_tibble(llista_vars) %>% dplyr::select(name=value) %>% 
    dplyr::left_join(vars_df,by="name") %>% 
    dplyr::mutate(
      value=ifelse(is.na(value) | value=="NA","",value),
      var=paste0(name,value))
  
  # Juntar noms de variables + levels
  dt_total<-dt_total %>% dplyr::left_join(vars_df,by="var")
  
  # Eliminar vars a eliminar
  dt_total<-dt_total %>% dplyr::filter(!name%in%vars_remove)
  
  # Etiquetar variables
  if (etiquetar) dt_total<-dt_total %>% etiquetar_taula(camp = "name", ...)
  
  # Afegir nivells exepte Yes / Si i eliminar cat No
  dt_total<-
    dt_total %>% 
    mutate(name=if_else(value=="" | value=="Yes" | value=="Si",
                        name,paste0(name,":",value))) %>% 
    filter(value!="No") 
  
  # Preque mantingui l'ordre  
  dt_total$name<- factor(dt_total$name, levels=rev(unique(dt_total$name)),ordered=T)
  
  dt_total
  
  }


covariate_plot_m.out<-function(dt=m.out,vars_remove=NULL,etiquetar=F,...) {
  
  # Preparar dades a plotejar
  dt_pre<-summary(dt,standardize = T)$sum.all %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Unmatched",id=dplyr::row_number())
  dt_post<-summary(dt,standardize = T)$sum.matched %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Matched",id=dplyr::row_number())
  
  # Preparar i ordenar per id
  dt_total<- 
    dt_pre %>% dplyr::bind_rows(dt_post) %>% 
    mutate (stat=`Std. Mean Diff.`) %>% 
    dplyr::filter(var!="distance") %>% 
    dplyr::filter(!is.na(stat)) %>% 
    mutate(var=factor(var,levels=rev(dt_pre$var)))   # Convertir a factor per que surti ordenat
  
  # Generar variables+nivells indexat
  llista_vars<-all.vars(stats::formula(m.out$model)[-2])
  vars_df<-
    llista_vars %>% set_names(llista_vars) %>% 
    purrr::map(~levels(dades[[.x]])) %>% 
    tibble::enframe() %>% 
    tidyr::unnest(cols = c(value))
  
  vars_df<-
    tibble::as_tibble(llista_vars) %>% dplyr::select(name=value) %>% 
    dplyr::left_join(vars_df,by="name") %>% 
    dplyr::mutate(
      value=ifelse(is.na(value) | value=="NA","",value),
      var=paste0(name,value))
 
  # Juntar noms de variables + levels
  dt_total<-dt_total %>% dplyr::left_join(vars_df,by="var")
  
  # Eliminar vars a eliminar
  dt_total<-dt_total %>% dplyr::filter(!name%in%vars_remove)
  
  # Etiquetar variables
  if (etiquetar) dt_total<-dt_total %>% etiquetar_taula(camp = "name", ...)
  
  # Afegir nivells exepte Yes / Si i eliminar cat No
  dt_total<-
    dt_total %>% 
    mutate(name=if_else(value=="" | value=="Yes" | value=="Si",
                        name,paste0(name,":",value))) %>% 
    filter(value!="No") 
  
  # Preque mantingui l'ordre  
  dt_total$name<- factor(dt_total$name, levels=rev(unique(dt_total$name)),ordered=T)
  
  covariate_plot_dades(dt_total)
  
  }


covariate_plot_dades<-function(dt=dt_total,var="name",stat="stat",title="Covariate plot \n oGLD vs SGLT-2i group", labx="Standardized mean difference") {
  
  # dt=dt_total
  # var="name"
  # stat="stat"
  # title="Covariate plot \n oGLD vs SGLT-2i group"
  # labx="Standardized mean difference"
  
  var=dplyr::sym(var)
  stat=dplyr::sym(stat)
  
  ggplot2::ggplot(aes(y = !!var, x = !!stat, group = Sample), data = dt) + 
    ggplot2::theme(panel.background = element_rect(fill = "white"),
                   axis.text.x = element_text(color = "black"),
                   axis.text.y = element_text(color = "black"),
                   panel.border = element_rect(fill = NA, color = "black"),
                   plot.background = element_blank(),
                   legend.background = element_blank(),
                   legend.key = element_blank()) + 
    geom_point(aes(colour=Sample),size=3) +
    
    ggplot2::geom_vline(xintercept = c(-0.1,0,0.1) , linetype = 2, color = "gray8")+
    ggplot2::theme(legend.position = "top")+
    ggplot2::labs(y = NULL, x = labx, title=title)+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  }


#### Funció que ploteja Covariate_plot d'un objecte matchit
covariate_plot<-function(dt=m.out,vars_remove=NULL, etiquetar=F,...) {
  
  # vars_remove<-c("age", "sexe","tempsdm_cat4", "iyearsem","qmedea")
  # vars_remove = c("qmedea","age")
  # etiquetar = T
  # dt=m.out
  # vars_remove=cols_a_borrar
  # etiquetar = T
  # taulavariables=conductor_variables
  
  
  # m.out,vars_remove = cols_a_borrar,etiquetar = T,taulavariables=conductor_variables,camp_descripcio="descripcio2"
  
  # Preparar dades a plotejar
  dt_pre<-summary(dt,standardize = T)$sum.all %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Unmatched",id=dplyr::row_number())
  dt_post<-summary(dt,standardize = T)$sum.matched %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Matched",id=dplyr::row_number())
  #
  # Preparar i ordenar per id
  dt_total<- 
    dt_pre %>% dplyr::bind_rows(dt_post) %>% 
    mutate (stat=`Std. Mean Diff.`) %>% 
    dplyr::filter(var!="distance") %>% 
    dplyr::filter(!is.na(stat)) %>% 
    mutate(var=factor(var,levels=rev(dt_pre$var)))   # Convertir a factor per que surti ordenat
  
  # Generar variables+nivells indexat
  llista_vars<-all.vars(stats::formula(m.out$model)[-2])
  vars_df<-
    llista_vars %>% set_names(llista_vars) %>% 
    purrr::map(~levels(dades[[.x]])) %>% 
    tibble::enframe() %>% 
    tidyr::unnest(cols = c(value))
  
  vars_df<-
    tibble::as_tibble(llista_vars) %>% dplyr::select(name=value) %>% 
    dplyr::left_join(vars_df,by="name") %>% 
    dplyr::mutate(
      value=ifelse(is.na(value) | value=="NA","",value),
      var=paste0(name,value))
  
  # Juntar noms de variables + levels
  dt_total<-dt_total %>% dplyr::left_join(vars_df,by="var")
  
  # Eliminar vars a eliminar
  dt_total<-dt_total %>% dplyr::filter(!name%in%vars_remove)
  
  # Etiquetar variables
  if (etiquetar) dt_total<-dt_total %>% etiquetar_taula(camp = "name", ...)
  
  
  # Afegir nivells exepte Yes / Si i eliminar cat No
  dt_total<-
    dt_total %>% 
    mutate(name=if_else(value=="" | value=="Yes" | value=="Si",
                        name,paste0(name,":",value))) %>% 
    filter(value!="No") 
  
  # Preque mantingui l'ordre  
  dt_total$name<- factor(dt_total$name, levels=rev(unique(dt_total$name)),ordered=T)
  
  ggplot2::ggplot(aes(y = name, x = stat, group = Sample), data = dt_total) + 
    ggplot2::theme(panel.background = element_rect(fill = "white"),
                   axis.text.x = element_text(color = "black"),
                   axis.text.y = element_text(color = "black"),
                   panel.border = element_rect(fill = NA, color = "black"),
                   plot.background = element_blank(),
                   legend.background = element_blank(),
                   legend.key = element_blank()) + 
    geom_point(aes(colour=Sample),size=3) +
    
    ggplot2::geom_vline(xintercept = c(-0.1,0,0.1) , linetype = 2, color = "gray8")+
    ggplot2::theme(legend.position = "top")+
    ggplot2::labs(y = NULL, x = "Standardized mean difference",
                  title="Covariate plot \n oGLD vs SGLT-2i group")+
    theme(plot.title = element_text(hjust = 0.5))
}


##############      K-M   plot     #####
plotKM=function(y=c("temps.otd","exitus.otd"),dt=dades) {
  
  # y=llistaevents[[2]]
  # dt=dades
  # y=c("temps.otd","exitus.otd")
  
  y<-unlist(y,use.names = F)
  
  temps=sym(y[[1]])
  event=sym(y[[2]])
  caption=Hmisc::label(dt[[y[2]]])
  
  dt<-dt %>% select(grup,temps=!!temps,event=!!event) %>% mutate(event=(event=="Yes") %>% as.numeric())
  
  fit<- survival::survfit(survival::Surv(temps,event) ~ grup, data = dt) 
  
  
  # Basic survival curves
  p <- survminer::ggsurvplot(fit, data = dt, fun="cumhaz" , 
                             conf.int = TRUE,
                             main = "Survival curve",
                             title= caption,
                             size = 0.5,
                             ylim = c(0,0.05),
                             xlim = c(0,365),
                             break.x.by=90,
                             xlab = "Time in days",
                             risk.table = F,
                             censor.shape="|", censor.size = 1,
                             legend.labs=c("oGLD","SGLT-2")) 
  p
  
  }








