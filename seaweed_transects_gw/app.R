#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### WORKSPACE SET UP ######
## load packages
library(shiny)
library(tidyverse)
library(plyr)
library(ggplot2);theme_set(theme(axis.text.x = element_text(face="bold"),
                                 axis.text.y = element_text(size=12, face="bold"),
                                 axis.title.x = element_text(size=15, face="bold"),
                                 axis.title.y = element_text(size=15, face="bold")))

## load data
algae = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/GW_seaweed_seasonality_transect_data.csv")

## plot items
pd=position_dodge(0.1)
color.month = c("dodgerblue4","dodgerblue","deepskyblue","cyan1","plum1","mediumorchid1","mediumpurple1",
                "purple1","purple4","magenta4","magenta2","violet","darkorchid1")


##### FORMAT DATA FOR ANALYSIS ####
## remove transects 1 and 5
algae = subset(algae, algae$transect_id!=1& algae$transect_id!=5)


## how many total columns -> makes it so you don't have to edit numbers as new algae are added
n=ncol(algae)

## make all algae abundance columns numeric 
algae[,7:n] <- sapply(algae[,c(7:n)], as.numeric)

## fill empty cells (instances of 0 percnet cover) with 0
algae[is.na(algae)]<-0


## pivot data. This is important for plotting and analysis later
algae.wide = algae %>% 
  pivot_longer(-c(1:6))

## rename value column
names(algae.wide)[names(algae.wide)=="value"]<-"percent_cover"

## separate out seaweed phylum info 
algae.wide = separate(data = algae.wide, 
                      col = name, 
                      into = c("seaweed_id","phylum"), 
                      sep = "__")

## summarize data across years for the same transect and month
algae.wide.grouped = ddply(algae.wide, c("transect_id","distance_along_transect_m","seaweed_id","phylum","month"), 
                           summarise,
                           N = length(percent_cover), ## sample size
                           mean = mean(percent_cover),
                           sd = sd(percent_cover)) %>%
  mutate(se = sd/sqrt(N), ## standard error
         lci = mean - 1.960*(se), ## lower bound of 95% confidence interval
         uci = mean + 1.960*(se))

## fill empty cells (instances of 0 percnet cover) with 0 (again)
algae.wide.grouped[is.na(algae.wide.grouped)]<-0

## set order of month
algae.wide.grouped$month = factor(algae.wide.grouped$month, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))

## get seaweed species list
speclist = as.data.frame(colnames(algae))
speclist = speclist[-c(1:6),]

#!# need to remove __phylum from here 

## subset by phylum
brown = subset(algae.wide.grouped, algae.wide.grouped$phylum=="brown")
green = subset(algae.wide.grouped, algae.wide.grouped$phylum=="green")
red = subset(algae.wide.grouped, algae.wide.grouped$phylum=="red")


###### UI CODE #######
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Girl in a Wetsuit (Stanley Park) Seaweed Survey"),
    
    # download button
    downloadLink('downloadData', 'Download the seaweed transect data'),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Selector for choosing dataset ----
        selectInput('seaweed_species',
                    label = 'Choose a seaweed to plot',
                    choices = c(speclist),
                    selected="selected_seaweed"),

      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        textOutput("seaweed_species"),
        plotOutput("distPlot")
      )
    )
)



##### SERVER CODE #####

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ## download seaweed seasonality data as .csv file
    output$downloadData <- downloadHandler(
      filename = function() {paste('algae-', Sys.Date(), '.csv', sep='')},
      content = function(con) {write.csv(algae, con)})
    
    ## tell them which algae species they have selected
    output$seaweed_species <- renderText({ 
      paste("You have selected to plot", input$seaweed_species)
    })
    
    
    ## make plot by user input
    output$distPlot <- renderPlot({
      
      #!# won't work until __phylum removed form algae list
      
      ## subset data from user input 
      df.subset <- subset(algae.wide.grouped, seaweed_id == input$seaweed_species)
      
      ## make heatmap
      ggplot(df.subset, aes(x=month, y=distance_along_transect_m))+
        geom_tile(color="grey85", size=0.3)  
      })
    
    

    
}



##### APP #####
# Run the application 
shinyApp(ui = ui, server = server)
