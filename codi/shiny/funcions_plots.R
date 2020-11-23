### Funcions 


# Funcio.forest.plot 

forest.plot.HR<-function(dadesmodel,label="Categoria",mean="estimate",lower="Linf",upper="Lsup",label_X="OR (95% CI)",
                         intercept=1,
                         nivell="outcome", factor1="type",color=F, label_Xvertical="Cardiovascular event",nolabels=TRUE) {
  
  # dadesmodel=dt_fig
  # label="label"
  # mean="HR"
  # lower="IC951"
  # upper="IC952"
  # label_X="Hazard ratio (95% CI)"
  # intercept=1
  # nivell="outcome"
  # factor1="grups"
  # label_Xvertical = "Subgroups"
  # color=F
  # nolabels=TRUE
  
  # Generar data set 
  dadesmodel <- dadesmodel %>% select(valor=!!mean,Linf=!!lower,Lsup=!!upper,nivell=!!nivell, factor1=!!factor1)
  
  ## Preparar taula (Genero etiqueta)
  taula_betas<-dadesmodel %>% mutate(etiqueta=paste0("   ",factor1),
                                     Group = paste0(factor1))

  # Afegir fila com un punt nivell per outcome i genero label de group
  taula_betas<-taula_betas %>% split(.$nivell) %>% 
    purrr::map_dfr(~add_row(.x,.before = 0),.id = "outcome" ) %>% 
    dplyr::mutate (etiqueta2=if_else(is.na(etiqueta),outcome,"")) %>% 
    dplyr::mutate (etiqueta=if_else(is.na(etiqueta),outcome,etiqueta))
  
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
  
  # per defecte agafo etiqueta 3 (Si no agafo etiqueta buida)
  if (nolabels) labels_scaleX=taula_betas %>% pull(etiqueta3) else labels_scaleX=taula_betas %>% pull(etiqueta)  
  
  #limits màxims d'eixos
  xmax=max(taula_betas$Lsup,na.rm = T) %>% max(2) 
  xmin=min(taula_betas$Linf,na.rm = T) %>% min(0.4)
  ymaxim=taula_betas %>% count() %>% as.numeric()
  
  fp <- ggplot(data=taula_betas,aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    geom_pointrange(size=0.6) + 
    geom_hline(yintercept=intercept, lty=1,colour="grey") +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    scale_x_continuous(breaks=taula_betas %>% pull(id),labels=labels_scaleX)  +
    ylim(xmin,xmax)

  fp<-fp + theme_minimal(base_size = 12) + theme(axis.text.y = element_text(hjust = 0,vjust=0,size=11)) +
    labs(title = "Forest plot of hazard hatios and confidence interval (95%CI)", x=label_Xvertical,y=label_X, col="Method \n") +
    theme(legend.position="top") +
    annotate("text", x=ymaxim+1,y=1,label="Favors SGLT-2        Favors oGLD-2", colour = "red",fontface = "bold")

  # caption = "SGLT-2: sodium-glucose co-transporter-2 inhibitors | oGLD-2 \n created by Jordi Real & Rai Puig ")
  
  if (color) {fp<-fp + geom_point(aes(color=Group),size=3)} 
  
  # Add banda d'error
  # fp<-fp + geom_hline(yintercept = c(intercept+0.1,intercept-0.1),linetype=2)
  
  fp 
  
  # plotly::ggplotly(fp) 
  
  
}


##############      K-M   plot     #####
plotKM=function(y=c("temps.otd","exitus.otd"),dt=dades) {
  
  # y=llistaevents[[2]]
  # dt=dades
  # y=c("temps.otd","exitus.otd")

  y<-unlist(y,use.names = F)
  
  temps=dplyr::sym(y[[1]])
  event=dplyr::sym(y[[2]])
  caption=Hmisc::label(dt[[y[2]]]) 
  
  dt<-dt %>% dplyr::select(grup,temps=!!temps,event=!!event) %>% mutate(event=(event=="Yes") %>% as.numeric())
  
  fit<- survival::survfit(survival::Surv(temps,event) ~ grup, data = dt) 
  
  
  # Basic survival curves
  
  p <- survminer::ggsurvplot(fit, data = dt,
                             main = "Survival curve",
                             title= caption,
                             conf.int = TRUE,
                             size = 0.7,
                             fun="cumhaz",
                             ylim = c(0,0.05),
                             xlim = c(0,365),
                             break.x.by=90,
                             xlab = "Time in days",
                             risk.table = F,
                             censor=F,
                             pval=TRUE,
                             legend.labs=c("oGLD","SGLT-2"),
                             ggtheme = ggplot2::theme_minimal(),
                             caption="Estimation and 95% confidence interval based on Kaplan-Meier method") 
              
  p +ggplot2::labs(title = paste0("Cumulative hazard curve of ",caption)) 
  
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

