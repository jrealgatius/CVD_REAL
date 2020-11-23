### Funcions 

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
  p <- survminer::ggsurvplot(fit, data = dt,
                             main = "Survival curve",
                             title= caption,
                             size = 0.5,
                             ylim = c(0.95,1),
                             xlim = c(0,365),
                             break.x.by=90,
                             xlab = "Time in days",
                             risk.table = F,
                             censor.shape="|", censor.size = 1,
                             legend.labs=c("oGLD","SGLT-2")) 
  p
}


