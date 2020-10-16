############################################################################################
#
#                                         15.10.2020
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://rinterface.com/shiny/shinydashboardPlus/
#
#
#
#
#
#
#  

#

#  Llibreries necessaries

library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)


# Carrega de dades i funcions
# load("dades/data_shiny.Rdata")

load(here::here("shiny/dades","data_shiny.Rdata"))

source(here::here("shiny","funcions_plots.R"))

# 1. Funcio per triar metode --------------

# Parametres a triar forest plot global sensibility analisis: 
# All, ITT/ OT , unadjusted / adjusted

forest.plot.choice<-function(metode=c("ITT","OT","unadjusted")){

    #metode<-c("adjusted")
    dt_temp<-dt_HR %>% filter(stringr::str_detect(Method,paste0(metode,collapse = "|")))
    forest.plot.HR(dadesmodel=dt_temp,label="grups",mean="HR",lower="IC951",upper="IC952",label_X="Favors SGLT-2      HR (95% CI)       Favors oGLD-2",
                   intercept=1,nivell="outcome",factor1="Method",
                   label_Xvertical = "Outcome", color = T)  
        
    }
    

# 2. Funcio per triar plot OUTCOME i HR stratificats ------------

# Forest plot de HR estratificats

# generar llista de noms de HR.ESTR.TOTAL
vectordenoms<-HR.ESTR.TOTAL %>% purrr::map(~ rownames(.x)[1]) %>% unlist() 
# nomenar-los
HR.ESTR.TOTAL<-purrr::set_names(HR.ESTR.TOTAL,vectordenoms)

forest.plot.choice.strat<-function(outcome_nom="All-cause death OTD") {
    
    # outcome_nom="All-cause death OTD"
    
    #Graficar-lo 
    HR.ESTR.TOTAL[[outcome_nom]] %>% 
        transmute(outcome=outcome_nom,
                  label=rownames(.) %>% stringr::str_replace("Adj1|Adj2",""),
                  grups=stringr::str_remove(Subgroup,"Age|CKD"),
                  label=paste0(label,":",grups),
                  grups=label,
                  HR,IC951,IC952) %>%
        forest.plot.HR(label="label",mean="HR",lower="IC951",upper="IC952",label_X="Favors SGLT-2      HR (95% CI)       Favors oGLD-2",
                       intercept=1,nivell="outcome",factor1="grups",label_Xvertical = "Subgroups")

    }



# 3. Funcio per triar KM en funcio d'outcome  -----------------
llistaevents<-purrr::set_names(llistaevents,vectordenoms)

graficar_KM_outcome<-function(outcome_nom="All-cause death OTD") {
    
    #outcome_nom="All-cause death OTD"
    
    plotKM(llistaevents[[outcome_nom]])
    
    }


# Define UI for application that draws a histogram
ui <- fluidPage(
  
    titlePanel(title="Characteristics and cardiovascular and mortality outcomes in patients with type 2 diabetes mellitus initiating treatment with sodium-glucose co-transporter-2 inhibitors and other diabetic medications"),
   
    # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("metode1",label="Choice the method :", choices= c("ITT","OT","unadjusted"," adjusted"),multiple = F),
          
          selectInput("metode2",label="Choice the outcome :", choices= names(llistaevents)   ,multiple = F),
          
          img( src = "https://avatars2.githubusercontent.com/u/57066591?s=200&v=4", align = "right", width =  70, height = 50),
          
          img( src = "https://www.idiapjgol.org/images/logo.png", align = "center",width =  90, height = 40)
        
          ),
        
        
         
        mainPanel(
          
          tabsetPanel(
                    tabPanel("Forest plot by method", plotOutput( "distPlot0")),
                    tabPanel("Forest plot by outcome",plotOutput("distPlot1")),
                    tabPanel("Kaplan-Meier by outcome",plotOutput("distPlot2"))
                    
          ))
        
             
           
        ),  tags$footer("Developed by Jordi Real & Ray Puig",align="center",
                        style = "position:absolute;bottom:0;width:50%,width:100%,italic= TRUE")
    )




# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot0 <- renderPlot({forest.plot.choice(input$metode1)})
  output$distPlot1 <- renderPlot({forest.plot.choice.strat(input$metode2)})
  output$distPlot2 <- renderPlot({graficar_KM_outcome(input$metode2)})
  
    }

# Run the application 
shinyApp(ui = ui, server = server)
