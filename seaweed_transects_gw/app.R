## Code by Siobhan Schenk

##### WORKSPACE SET UP ######
## load packages
library(DT)
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

## remove phylum info
algae.dom$dominant_taxa <-gsub("__.*","",algae.dom$dominant_taxa)

## rename columns for human readability
names(algae.dom)[names(algae.dom)=="distance_along_transect_m"]<-"Quadrat Height (m)"
names(algae.dom)[names(algae.dom)=="dominant_taxa"]<-"Dominant Algae"
names(algae.dom)[names(algae.dom)=="year"]<-"Year"
names(algae.dom)[names(algae.dom)=="month"]<-"Month"

##### SET UP SPECIES ABUNDANCE PLOT #####

## separate out seaweed phylum info 
algae.wide = separate(data = algae.wide, 
                      col = name, 
                      into = c("seaweed_id","phylum"), 
                      sep = "__")

## summarize data across years for the same transect and month
algae.wide.grouped = ddply(algae.wide, c("transect_id","distance_along_transect_m","seaweed_id","phylum","month", "year"), 
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
speclist = algae.wide$seaweed_id
speclist = unique(speclist)


###### UI CODE #######
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Monthly Seaweed Survey at Girl in a Wetsuit (Stanley Park)"),
    
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(            

      # Sidebar panel for inputs ----
      sidebarPanel(
      HTML('<p>These montly seaweed seasonality transcts were initiated and currently organized by 
      Siobhan and Varoon from the 
      <a href="https://www.zoology.ubc.ca/~parfrey/parfrey_lab/">Parfrey</a> and 
      <a href="https://www3.botany.ubc.ca/martone/">Martone</a>
      labs at the Univeristy of British Columbia, Vancouver Campus.</p>',
      
      '<p>This long-term survey work 
      partnership with the 
      <a href="https://stanleyparkecology.ca/">Stanley Park Ecological Sciety</a>, 
      who was instrumental in volunteer recruitment for this project.</p>',
      
      '<p>If you would like to take part in our seaweed survey, please email 
      <i>seaweedsurvey@zoology.ubc.ca</i> </p>',
      
      '<p>You are free to use these data in your research as long as attribution is given.
      Please cite the data as:</p>
      <p><i>Long-term monitoring of macroalgal biodiversity in Stanley Park, Vancouver, British Columbia 
      Schenk, Siobhan; P. Supratya, Varoon; T. Martone, Patrick; W. Parfrey, Laura, (2022) 
      <a href="https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IKGB6E">https://doi.org/10.5683/SP3/IKGB6E, </a>Borealis</i> 
      This Borealis link will have the latest data release</p>'),
      
      # download button for raw data
      downloadLink('downloadRawData', 'Download the raw data for the app here'),
      
      HTML('<p>If you would like the sampling protocol please email us at <i>seaweedsurvey@zoology.ubc.ca</i> or visit the <a href="https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IKGB6E">Borealis page</a></p>')), ## end of sidebarPannel
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        
        HTML("<h3>The plot below shows the mean percent cover (accross samlpling years) of the select seaweed.</h3>"),
        
        # Input: Selector for choosing dataset ----
        selectInput('seaweed_species',
                    label = 'Choose a seaweed to plot',
                    choices = c(speclist),
                    selected="selected_seaweed"),
        
        textOutput("seaweed_species"),
        
        HTML('<p>The y-axis show the distance of the quadrat from the seawall. The x-axis shows the numeric month of sampling.
             The facets on the y-axis break up the data by transect number, since there are three transects.</p>',
             
             '<p> <i> Note: empty regions on the graph indicate that there is no data available for that transect for that month.
             This is because we could not sample due to the tide height, or other unforseen events.</i> </p>'),
        
        downloadButton('downloadPlot', 'Download Plot'),
        
        plotOutput("distPlot", width = "90%"),
        
        ## space out bottom of graph and dominant taxa table
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        
        HTML('<h3>Table showing the dominant taxa at each height by month and year </h3>'),
        
        ## minimal table
          # https://shiny.rstudio.com/gallery/basic-datatable.html
        DT::dataTableOutput("dominant")
        
      ) ## end of mainPannel
    ) ## end of sidebarLayout
) ## end of ui



##### SERVER CODE #####

# Define server logic required to draw a histogram
server <- function(input, output) {

    ## download seaweed seasonality data as .csv file
    output$downloadRawData <- downloadHandler(
      filename = function() {paste('algae-', Sys.Date(), '.csv', sep='')},
      content = function(con) {write.csv(algae, con)})
  
    
    ## make plot by user input
    output$distPlot <- renderPlot({
      
      ## subset data from user input 
      df.subset <- subset(algae.wide.grouped, seaweed_id == input$seaweed_species)
      
      ## set algal colors
      phylum_colors <- c("brown"="goldenrod2", "red"="red4","green"="springgreen3")
      
      ## make bubble plot of abundance
      ggplot(df.subset, aes(x=month, y=distance_along_transect_m, color=phylum))+
        geom_point(aes(size=mean))+
        labs(x="Sampling Month", y="Distance From Seawall (m)", 
             size="Mean Percent Cover", color="Algal Phylum")+
        facet_grid(transect_id~year+seaweed_id, scales="free")+
        scale_y_reverse()+
        scale_color_manual(values=phylum_colors, limits=names(phylum_colors))
      },height = 700, width = 700 )
    
    output$downloadPlot <- downloadHandler(
      filename = "bubble_abundance_transect_plot.png",
      content=function(file){
        device <-function(..., width, height) {
          grDevices::png(..., width=width, height=height, res=300, units="in")}
      ggsave(file, device=device)})
    
    output$dominant = DT::renderDataTable(algae.dom)
    
}




##### APP #####
# Run the application 
shinyApp(ui = ui, server = server)
