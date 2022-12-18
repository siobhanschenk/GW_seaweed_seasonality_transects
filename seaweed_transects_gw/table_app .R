## Code by Siobhan Schenk

##### WORKSPACE SET UP ######
## load packages
library(shiny)
library(tidyverse)
library(plyr)
library(ggplot2); theme_set(theme_bw()+
                              theme(panel.grid = element_blank(),
                                    strip.background = element_rect(fill="white"),
                                    axis.text = element_text(size = 15, face="bold"),
                                    axis.title = element_text(size=20, face="bold"),
                                    strip.text = element_text(size = 12, face="bold"),
                                    legend.text=element_text(size=15),
                                    legend.title=element_text(size=15, face="bold")))

## load data
algae = read.csv("./Data/proj.csv")


##### FORMAT DATA FOR ANALYSIS ####
## remove transects 1 and 5
algae = subset(algae, algae$transect_id!=1& algae$transect_id!=5)

## how many total columns -> makes it so you don't have to edit numbers as new algae are added
n=ncol(algae)

## make all algae abundance columns numeric 
algae[,9:n] <- sapply(algae[,c(9:n)], as.numeric)

## fill empty cells (instances of 0 percent cover) with 0
algae[is.na(algae)]<-0


## pivot data. This is important for plotting and analysis later
algae.wide = algae %>% 
  pivot_longer(-c(1:8))

## rename value column
names(algae.wide)[names(algae.wide)=="value"]<-"percent_cover"

##### SET UP TABLE OF DOMINANT TAXA #####

## group the data to get the most abundant taxa in the group of three transects for each height 
algae.height = ddply(algae.wide, c("year", "month", "distance_along_transect_m", "name"),
                     summarise,
                     mean.percent.cover = mean(percent_cover))

## pivot the data back 
algae.height.wide = algae.height %>%
  pivot_wider(names_from=name, values_from = mean.percent.cover)

## get end of otu table
nspecies = ncol(algae.height.wide)

## get otu dataframe
algae.otu = algae.height.wide[,c(4:nspecies)]

## get list of dominant taxa in otu dataframe 
algae.max = algae.otu %>%
  mutate(dominant_taxa = names(.)[max.col(.)])

## merge algae.max back with original algae dataset
algae.dom = full_join(algae.height.wide, algae.max)

## remove otu table
algae.dom = algae.dom[,-c(4:nspecies)]


###### UI CODE #######
# Define UI for application that draws a histogram
ui <- fluidPage(

      # Main panel for displaying outputs ----
      mainPanel(
        
        HTML('<h3>Table showing the dominant taxa at each height by month and year </h3>'),
        
        ## set up table
          # https://shiny.rstudio.com/gallery/basic-datatable.html 
        function(input, output){
          
          ## filter data based on selections
          output$table <- DT::renderDataTable(DT::datatable({
            
            data <- algae.dom
            
            data
            
          }))
        }
        
      ), ## end of mainPannel




##### SERVER CODE #####

# Define server logic required to draw a histogram
server <- function(input.tab, output.tab) {

    ## create table
    fluidRow(

      DT::dataTableOutput("table")
    )
}


##### APP #####
# Run the application 
shinyApp(ui = ui, server = server)
