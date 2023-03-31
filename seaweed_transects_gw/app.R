## Code by Siobhan Schenk

##### WORKSPACE SET UP ######
## load packages
library(shiny)
library(tidyverse)
library(plyr)
library(stringi)
library(ggplot2); theme_set(theme_bw()+
                              theme(panel.grid = element_blank(),
                                    strip.background = element_rect(fill="white"),
                                    axis.text = element_text(size = 15, face="bold"),
                                    axis.title = element_text(size=20, face="bold"),
                                    strip.text = element_text(size = 12, face="bold"),
                                    legend.text=element_text(size=15),
                                    legend.title=element_text(size=15, face="bold"),
                                    plot.title = element_text(size=20, face="italic")))

## load data
algae = read.csv("./Data/GW_seaweed_seasonality_transect_data.csv")


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

## fill empty cells (instances of 0 percent cover) with 0 (again)
algae.wide.grouped$mean[is.na(algae.wide.grouped$mean)]<-0


## use gsub to fix lables (need to separate because jan and Feb replace the 1 and 2 in Nov and Dec)
algae.wide.grouped$month <- stri_replace_all_regex(algae.wide.grouped$month,
                                              pattern=c("3","4","5","6","7","8","9","10","11","12"),
                                              replacement=c("Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                                              vectorize=FALSE)
algae.wide.grouped$month <- stri_replace_all_regex(algae.wide.grouped$month,
                                                   pattern=c("1","2"),
                                                   replacement=c("Jan.", "Feb."),
                                                   vectorize=FALSE)

## set order of month
algae.wide.grouped$month = factor(algae.wide.grouped$month, levels=c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."))

## get seaweed species list
speclist = algae.wide$seaweed_id
speclist = unique(speclist)


## subset by phylum
#brown = subset(algae.wide.grouped, algae.wide.grouped$phylum=="brown")
#green = subset(algae.wide.grouped, algae.wide.grouped$phylum=="green")
#red = subset(algae.wide.grouped, algae.wide.grouped$phylum=="red")


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
      
      HTML('<p>If you would like the sampling protocol visit the <a href="https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IKGB6E">Borealis page</a> or email us at <i>seaweedsurvey@zoology.ubc.ca</i> </p>')), ## end of sidebarPannel
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        HTML("<p>The plot below shows the mean percent cover (accorss samlpling years) of the select seaweed.</p>") ,
        
        # Input: Selector for choosing dataset ----
        selectInput('seaweed_species',
                    label = 'Choose a seaweed to plot',
                    choices = c(speclist),
                    selected="selected_seaweed"),
        
        textOutput("seaweed_species"),
        
        
        HTML('<p>The y-axis show the distance of the quadrat from the seawall. The x-axis shows the month of sampling.
             The facets on the y-axis break up the data by transect number, since there are three transects.</p>',
             
             '<p> <i> Note: empty regions on the graph indicate that there is no data available for that transect for that month.
             This is because we could not sample due to the tide height, or other unforseen events. Grey boxes indicate that the algae was
             not found in the quadrat.</i></p>'),
        
        downloadButton('downloadPlot', 'Download Plot'),
        
        plotOutput("distPlot", width = "90%"),
        
        
      ) ## end of mainPannel
    ), ## end of sidebarLayout
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
      #phylum_colors <- c("brown"=c(low="wheat1",high="goldenrod4"), "red"="red4","green"="springgreen3")
      
      ## use to make the 0s white 
      df.subset$mean <- ifelse(df.subset$mean==0,NA,df.subset$mean)
      
      ## make bubble plot of abundance
      ggplot(df.subset, aes(x=month, y=distance_along_transect_m, fill=mean))+
        geom_tile(color = "gray")+
        labs(x="Sampling Month", y="Distance From Seawall (m)", 
             fill="Mean Percent Cover")+
        facet_grid(paste0("transect ", df.subset$transect_id)~year, scales="free", space="free")+
        scale_y_reverse()+
        scale_fill_gradient(low="lightskyblue1", high="navy", na.value="grey90", limits = c(0,100))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              strip.text.y = element_text(angle = 0))+
        ggtitle(paste0(df.subset$seaweed_id))
      
      
      },height = 700, width = 700 )
    
    
    output$downloadPlot <- downloadHandler(
      filename =  "seaweed_abundance_transect_plot.png",
      content=function(file){
        device <-function(..., width, height) {
          grDevices::png(..., width=width, height=height, res=500, 
                         units="in"
                         )}
      ggsave(file, device=device, width=12, height=8, units="in")})
    
}



##### APP #####
# Run the application 
shinyApp(ui = ui, server = server)
