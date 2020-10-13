#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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

source("funcions_plots.R")

# 1. Funció per triar metode --------------

# Parametres a triar forest plot global sensibility analisis: 
# All, ITT/ OT , unadjusted / adjusted

forest.plot.choice<-function(metode=c("ITT","OT","unadjusted")){

    # metode<-c("adjusted")
    dt_temp<-dt_HR %>% filter(stringr::str_detect(Method,paste0(metode,collapse = "|")))
    forest.plot.HR(dadesmodel=dt_temp,label="grups",mean="HR",lower="IC951",upper="IC952",label_X="HR (95% CI)",intercept=1,nivell="outcome",factor1="Method",
                   label_Xvertical = "Outcome", color = T)  
        
    }
    

# 2. Funció per triar plot OUTCOME i HR stratificats ------------

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
        forest.plot.HR(label="label",mean="HR",lower="IC951",upper="IC952",label_X="HR (95% CI)",intercept=1,nivell="outcome",factor1="grups",label_Xvertical = "Subgroups")

    }

# Funció per triar KM en funció d'outcome  -----------------
llistaevents<-purrr::set_names(llistaevents,vectordenoms)

graficar_KM_outcome<-function(outcome_nom="All-cause death OTD") {
    
    outcome_nom="All-cause death OTD"
    
    plotKM(llistaevents[[outcome_nom]])
    
    }


    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Characteristics and cardiovascular and mortality outcomes in patients with type 2 diabetes mellitus initiating treatment with sodium-glucose co-transporter-2 inhibitors and other diabetic medications"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
       
        selectInput("metode","Choice the method",c("ITT","OT","unadjusted"," adjusted"),multiple = T,selectize = T),
        
        #  sidebarPanel(
        #     sliderInput("bins",
        #                 "Number of bins:",
        #                 min = 1,
        #                 max = 50,
        #                 value = 30)
        # ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        # # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
        # Draw forest plot 
        
        forest.plot.choice(input$metode)

        
        })

    
    }

# Run the application 
shinyApp(ui = ui, server = server)
