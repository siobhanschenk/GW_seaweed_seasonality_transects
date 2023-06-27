## Code by Siobhan Schenk

##### WORKSPACE SET UP ######
## load packages
library(shiny)
library(tidyverse)
library(plyr)
library(stringi)
library(data.table)
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
algae.wide = read.csv("./Data/GW_seaweed_transects_data_cleaned.csv")
commonnames = read.csv("./Data/GW_common_names.csv")



##### FORMAT DATA FOR ANALYSIS ####

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



## add yes/no variable to remove seaweeds only counted 2 or less times
algae.wide.grouped$yn = ifelse(algae.wide.grouped$mean > 0, "1", "0")

keeplist = ddply(algae.wide.grouped, c("seaweed_id"),
                  summarize,
                  occurance = sum(as.numeric(yn)))

keeplist = subset(keeplist, keeplist$occurance>4)

keeplist = c(unique(keeplist$seaweed_id))


## subset out low occurance
algae.wide.grouped = subset(algae.wide.grouped, algae.wide.grouped$seaweed_id %in% c(keeplist))

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
speclist = algae.wide.grouped$seaweed_id
speclist = unique(speclist)


## subset by phylum
#brown = subset(algae.wide.grouped, algae.wide.grouped$phylum=="brown")
#green = subset(algae.wide.grouped, algae.wide.grouped$phylum=="green")
#red = subset(algae.wide.grouped, algae.wide.grouped$phylum=="red")


###### UI CODE #######
# Define UI for application that draws a histogram
ui <- fluidPage(

    ##### Application title ######
    titlePanel("Monthly Seaweed Survey at Girl in a Wetsuit (Stanley Park, Vancouver, BC)"),
    
    
    ##### Sidebar layout with input and output definitions ----
    sidebarLayout(            

      # Sidebar panel for inputs ----
      sidebarPanel(
      HTML('<h4>Data Information</h4>',
        '<p>These montly seaweed seasonality transcts were initiated and currently organized by 
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
      
      HTML('<p>If you would like the most up to date data with the sampling protocol visit the <a href="https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IKGB6E">Borealis page</a> or email us at <i>seaweedsurvey@zoology.ubc.ca</i> </p>'), 
      
      br(),
      
      HTML('<h4>Volunteers </h4>',
        '<p>We want to say a huge thank you to all the people who donated their time to help us collect these data. 
           In particular: 
           Andrea Jackman, Dr. Bridgette Clarkston, Connor Wardrop,
           Emma Jourdain, Emma Menchions, Evan Kohn,
           Garrett Ainsworth-Cruickshank, MJ Herrin, Reilly Perovich,
           Risa Ogushi, Ryan Ju, Tobin Sparling, Vincent Billy</p>'),
      br(),
      
      ),
      
      ## end of sidebarPannel
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        ##### more plot text ######
        HTML("<h4>The plot below shows the mean percent cover (accorss sampling years) of the select seaweed.</h4>") ,
        
        ##### Input: Selector for choosing dataset ----
        selectInput('seaweed_species',
                    label = 'Choose a seaweed to plot (see bottom of page for common names)',
                    choices = c(speclist),
                    selected="selected_seaweed"),
        
        textOutput("seaweed_species"),
        
        ##### plot text info ######
        
        HTML('<p>The y-axis show the distance of the quadrat from the seawall. The x-axis shows the month of sampling.
             The facets on the y-axis break up the data by transect number, since there are three transects.</p>',
             
             '<p> <i> Note (1): Below the plot, we show a photo of the selected seaweed (if we have one).</i></p>',
             
             '<p> <i> Note (2): <b>Empty</b> regions on the graph indicate that there is no data available for that transect for that month.
             This is because we could not sample due to the tide height, or other unforseen events. <b>Grey</b> boxes indicate that the algae was
             not found in the quadrat.</i></p>'),
      
        
        downloadButton('downloadPlot', 'Download Plot'),
        
        plotOutput("distPlot", width = "90%"),
        
        ##### move image #######
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

        HTML('<p>Below is a photo of the seaweed species being plotted (<i>photos by us</i>).</p>'),
        
        imageOutput(outputId="Imagen"),
        
        br(),
        br(),
        
        HTML('<h4>Table showing the latin binomial and common names for seaweeds</h4>'),
        
        ##### set up table ######
        tableOutput('table'),

        
        ##### leave space at the bottom of the page #####
        br(),
        br(),
        
        
      ) ## end of mainPannel
    ), ## end of sidebarLayout
) ## end of ui



##### SERVER CODE #####

# Define server logic required to draw a histogram
server <- function(input, output) {

    ###### download seaweed seasonality data as .csv file ######
    output$downloadRawData <- downloadHandler(
      filename = function() {paste('algae-', Sys.Date(), '.csv', sep='')},
      content = function(con) {write.csv(algae, con)})

    
    ##### make plot by user input #######
    output$distPlot <- renderPlot({
      
      
      ## subset data from user input 
      df.subset <- subset(algae.wide.grouped, seaweed_id == input$seaweed_species)
      
      ## set algal colors
      #phylum_colors <- c("brown"=c(low="wheat1",high="goldenrod4"), "red"="red4","green"="springgreen3")
      
      ## use to make the 0s white 
      df.subset$mean <- ifelse(df.subset$mean==0,NA,df.subset$mean)
      
      
      ##### make bubble plot of abundance ######
      ggplot(df.subset, aes(x=month, y=distance_along_transect_m, fill=mean))+
        geom_tile(color = "grey50", lwd = 0.5, linetype = 1)+
        labs(x="Sampling Month", y="Distance From Seawall (m)", 
             fill="Mean Percent Cover")+
        facet_grid(paste0("transect ", df.subset$transect_id)~year, scales="free", space="free")+
        scale_y_reverse()+
        scale_fill_gradient2(
          low=c(unique(df.subset$phylum)),
          high=c(unique(df.subset$phylum)),
      na.value="grey90", 
      limits = c(0,100))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              strip.text.y = element_text(angle = 0))+
        ggtitle(paste0(df.subset$seaweed_id))
      
      
      },height = 700, width = 700 )
    
    ###### downlaod and save plot ######
    output$downloadPlot <- downloadHandler(
      filename =  "seaweed_abundance_transect_plot.png",
      content=function(file){
        device <-function(..., width, height) {
          grDevices::png(..., width=width, height=height, res=500, 
                         units="in"
                         )}
      ggsave(file, device=device, width=12, height=8, units="in")})
    
    ##### render image of selected seaweed ######
    output$Imagen<- renderImage({
      Leg<-paste0("./Data/images/", print(c(unique(input$seaweed_species))), ".JPG")
          list(src=Leg, alt = "photo of selected algae (if we have a photo).", style="width: 500px")
    }, deleteFile = FALSE)   
    
    
    ####### create table of common names ######
    output$table = renderTable(commonnames)

    
}



##### APP #####
# Run the application 
shinyApp(ui = ui, server = server)
