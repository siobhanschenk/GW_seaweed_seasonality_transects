## Code by Siobhan Schenk

##### WORKSPACE SET UP ######
## load packages
library(shiny)
library(tidyverse)
library(plyr)
library(stringi)
library(data.table)
library(ggpubr)
library(ggplot2); theme_set(theme_bw()+
                              theme(panel.grid = element_blank(),
                                    strip.background = element_rect(fill="white"),
                                    axis.text = element_text(size = 15, face="bold"),
                                    axis.title = element_text(size=20, face="bold"),
                                    strip.text = element_text(size = 12, face="bold"),
                                    legend.text=element_text(size=12),
                                    legend.title=element_text(size=12, face="bold"),
                                    plot.title = element_text(size=20, face="italic")))

## load data from Borealis https://doi.org/10.5683/SP3/IKGB6E
algae.wide = read_csv("https://olrc2.scholarsportal.info/dataverse/10.5683/SP3/IKGB6E/18c07c7a8bf-293f36b60062?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27GW_seaweed_transects_data_cleaned.csv&response-content-type=text%2Fcsv&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20231230T234640Z&X-Amz-SignedHeaders=host&X-Amz-Expires=7200&X-Amz-Credential=33b141c798354e21a3394e9e4f546bbe%2F20231230%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=df90b4cdd53eac3296ad304a63daf6a7b94f6efe16ae24cf3b900c3f0ba3bdb3")
reproduction = read_csv("https://olrc2.scholarsportal.info/dataverse/10.5683/SP3/IKGB6E/18bfa3c70af-5413985a68af.orig?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27kelp_reproductive_timing.csv&response-content-type=text%2Fcsv&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20231230T234911Z&X-Amz-SignedHeaders=host&X-Amz-Expires=7200&X-Amz-Credential=33b141c798354e21a3394e9e4f546bbe%2F20231230%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=3cf267b18cab01f3aea0c0b2bfa28f2adc3f13595db953d92f67e1083f199aa7")

## read in local common names table
commonnames = read.csv("./Data/GW_common_names.csv")



##### FORMAT TRANSECT DATA FOR ANALYSIS ####
# round the quadrat heights to 0.5 m
algae.wide$quadrat_height_m = round_any(algae.wide$quadrat_height_m, 0.1)


## summarize data across years for the same transect and month
algae.wide.grouped = ddply(algae.wide, c("quadrat_height_m","seaweed_id","phylum","month", "year"), 
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
speclist = algae.wide.grouped$seaweed_id
speclist = unique(speclist)


##### FORMAT REPRODUCTION DATA FOR ANALYSIS #######
## replace NAs with 0 
reproduction[is.na(reproduction)] <- 0

## pivot longer to make seaweed_id column
repro.wide = pivot_longer(reproduction, cols = c(4:7),
                         names_to = "seaweed_id",
                         values_to ="reproductive_yn")

## make reproduction data be 0 or 1
repro.wide$reproductive_yn=if_else(repro.wide$reproductive_yn>0, "1", "0")

repro.wide$reproductive_yn = as.numeric(repro.wide$reproductive_yn)

## use gsub to fix lables (need to separate because jan and Feb replace the 1 and 2 in Nov and Dec)
repro.wide$month <- stri_replace_all_regex(repro.wide$month,
                                                   pattern=c("3","4","5","6","7","8","9","10","11","12"),
                                                   replacement=c("Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                                                   vectorize=FALSE)

repro.wide$month <- stri_replace_all_regex(repro.wide$month,
                                                   pattern=c("1","2"),
                                                   replacement=c("Jan.", "Feb."),
                                                   vectorize=FALSE)
## set order of month
repro.wide$month = factor(repro.wide$month, levels=c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."))

## calculate the mean reproductive occurence per month ###
repro.year.sum = ddply(repro.wide, c("seaweed_id", "month"),
                       summarise,
                       year.sum = sum(reproductive_yn),
                       year.N = length(year)) %>%
  mutate(percent_years = year.sum/year.N)




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
      <a href="https://www.zoology.ubc.ca/~parfrey/parfrey_lab/", target="_blank">Parfrey</a> and 
      <a href="https://www3.botany.ubc.ca/martone/", target="_blank">Martone</a>
      labs at the Univeristy of British Columbia, Vancouver Campus.</p>',
      
      '<p>If you would like to take part in our seaweed survey, please email 
      <i>seaweedsurvey@zoology.ubc.ca</i> </p>',
      
      '<p>You are free to use these data in your research as long as attribution is given.
      Please cite the data as:</p>
      <p><i>Long-term monitoring of macroalgal biodiversity in Stanley Park, Vancouver, British Columbia 
      Schenk, Siobhan; Supratya, Varoon P.; Martone, Patrick T.; Parfrey, Laura W. (2022) 
      <a href="https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IKGB6E", target="_blank">https://doi.org/10.5683/SP3/IKGB6E, </a>Borealis</i> 
      This Borealis link will have the latest data release</p>'),
      
      
      HTML('<p>If you would like the most up to date data with the sampling protocol visit the <a href="https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IKGB6E", target="_blank">Borealis page</a> or email us at <i>seaweedsurvey@zoology.ubc.ca</i> </p>',
           
           '<p><a href = "https://github.com/siobhanschenk/GW_seaweed_seasonality_transects", target="_blank">GitHub</a> with the app data and code</p>'),
      
      # download button for raw data
      #downloadLink('downloadRawData', 'Link to GitHub with the app data and code'),
      
      br(),
      
      HTML('<h4>Volunteers </h4>',
        '<p>We want to say a huge thank you to all the people who donated their time to help us collect these data. 
           In particular: 
           Andrea Jackman, Alex Choinski, Dr. Bridgette Clarkston, Connor Wardrop, 
        Emma Jourdain, Emma Menchions, Evan Kohn, Garrett Ainsworth-Cruickshank, 
        MJ Herrin, Reilly Perovich, Risa Ogush, Elliott Evans, 
        Tobin Sparling, Vincent Billy</p>'),
      br(),
      
      ),
      
      ## end of sidebarPannel
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        ##### more plot text ######
        HTML("<h3><b>Choose a seaweed to plot</b> *see bottom of page for common names and photos of the seaweed</h3>") ,
      
        ##### Input: Selector for choosing dataset ----
       
       selectInput('seaweed_species',
                    label = '',
                    choices = c(speclist),
                    selected="selected_seaweed"),
        
        textOutput("seaweed_species"),
        
        ##### plot text info ######
        
        HTML('<h4><b>Abundance Plot Tab: Shows the abundance of the selected seaweed by month and year</b></h4>',
             
             '<p>The y-axis show the tide height at which the quadrat is exposed to air. The x-axis shows the month of sampling.
             The facets on the y-axis break up the data by transect number, since there are three transects. We plot the chart datum quadrat height (m)
             on the y-axis, which is the same thing as measuring the tide height at which the quadrat is exposed to air. In essence, a larger chart datum number means higher up in the intertidal zone (shallower water).</p>',
             
             '<p> <i> Note (1):</i> <b>Grey</b> boxes indicate that the algae was
             not found in the quadrat. <b>Empty</b> regions (where the heatmap grid from 0 to n metres from the seawall ends) on the graph indicate that there is no data available for that transect for that month.
             This is because we could not sample due to the tide height, or other unforseen events.</p>' ,
             
             '<p> <i> Note (2):</i> The fill scale changes for each seaweed species. Make sure to check the color scale bar on the right of the plot when comparing relative abundances.</p>',
             
             '<p> <i> Note (3): <i> Below the plot, we show a photo of the selected seaweed (if we have one). *Photos by Varoon P. Supratya.</p>'),
       
       br(),
             
             
             HTML('<h4><b><i>Laminariales</i> (kelp) Reproductive Timing Plot Tab</b></h4>',
             '<p>Shows presence/abscence of reproductive <i>Laminariales</i> (kelp).
             From the start of data up to (and including) October 2023, we only recorded presence/abscence data.
             Starting in November 2023, we started counting the number of reproductive individuals during a time search but we only present presence/abscence data here. See Borealis for more details.
             With both types or reprdutive data, times where reproduction was not reccorded should not be regarded as a true abscence of reproductive individuals. 
             Reproductive status of other seaweeds was not recorded.</p>'
             ),
       
       br(),
       
       HTML('<h4><b>Site Gradient Tab</b></h4>',
            '<p>Shows the slope of each of the three transects individually. 
            We plot the distance from the seawall on the x-axis with the chart datum quadrat height (m) on the on the y-axis. 
            Each point on the line represents a quadrat.
            We indicate the approximate intertidal zones (Very High to Low), which we determined by the presence of particular seaweeds. 
            This plot links to the heatmap in the first tab by their y-axes, which are the same.</p>'
       ),
      
        
        downloadButton('downloadPlot', 'Download Plot'),
        
        tabsetPanel(type="tabs",
                    tabPanel("Abundance Plot", plotOutput("distPlot", width = "90%")),
                    tabPanel("Laminariales (kelp) Reproductive Timing Plot", plotOutput("reproPlot", width="90%")),
                    tabPanel("Site Gradient",imageOutput(outputId="TransImg"))),
       
       ## move the table down
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
       
       
       HTML('<h4>Table showing the latin binomial and common names for seaweeds</h4>'),
       
       ##### set up table ######
       tableOutput('table'),
        
        ##### move image #######
        br(),
        br(),


      #  HTML('<p>Below is a photo of the seaweed species being plotted (<i>photos by us</i>).</p>'),
        
        imageOutput(outputId="Imagen"),


        
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

    
    ##### ALGAE ABUNDANCE - plot by user input #######
    output$distPlot <- renderPlot({
      
      
      ## subset data from user input 
      df.subset <- subset(algae.wide.grouped, seaweed_id == input$seaweed_species)
      
      ## use gsub to change phylum colors to better color
      df.subset$phylum = gsub("brown", "darkgoldenrod4", df.subset$phylum)
      df.subset$phylum = gsub("red", "maroon4", df.subset$phylum)
      df.subset$phylum = gsub("green", "forestgreen", df.subset$phylum)
      
      ## use to make the 0s white 
      df.subset$mean <- ifelse(df.subset$mean==0,NA,df.subset$mean)
      
      
      ##### make heatmap  of abundance ######
      ggplot(df.subset, aes(x=month, y=as.factor(quadrat_height_m), fill=mean))+
        geom_tile(color = "grey50", lwd = 0.5, linetype = 1)+
        labs(x="Sampling Month", y="Chart datum quadrat height (m)", 
             fill="Mean Percent Cover")+
        facet_grid(.~year, scales="free", space="free")+
        scale_fill_gradient2(
          low=c(unique(df.subset$phylum)),
          high=c(unique(df.subset$phylum)),
      na.value="grey95", 
     # limits = c(0,100)
      )+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              strip.text.y = element_text(angle = 0))+
        ggtitle(paste0(df.subset$seaweed_id))
      
      
      },height = 700, width = 700 )
    
    ##### KELP REPRODUCTION - plot by user input #######
    output$reproPlot <- renderPlot({
      
      ## subset data from user input 
      repro.subset <- subset(repro.wide, seaweed_id == input$seaweed_species)
      
      ##### SET UP DESCISION TO PLOT REPRODUCTION DATAT OR NOT #####
      ## count occurence of reproduction data
      descision = sum(as.numeric(repro.subset$reproductive_yn))
      
      ## were any seaweeds reproductive?
      if(descision<1){
        ## no observations
        ggplot(repro.subset, aes(x=input$seaweed_species, y = reproductive_yn))+
          geom_point()+
          annotate("text", x = 0.1, y = 0.1, label = "No reproductive data available for this seaweed", size=10)+
          theme(axis.text = element_blank(), 
                axis.title = element_blank(),
                axis.ticks = element_blank())

      } ## end of if
      else {
        ## some observations (make plot)
      
      
      ##### MAKE PLOTS FOR REPRODUCTION #####
      ## replace 0 and 1 with no and yes for reproduction
      repro.subset$reproductive_yn <- stri_replace_all_regex(repro.subset$reproductive_yn,
                                                 pattern=c("0","1"),
                                                 replacement=c("No", "Yes"),
                                                 vectorize=FALSE)
      
      ##### make reproduction plot - for individual years ######
      singleyears = ggplot(repro.subset, aes(x=month, y=as.factor(year), fill=reproductive_yn))+
       geom_point(cex=5, pch=21)+
        #geom_tile()+
       # ggtitle(paste0(repro.subset$seaweed_id))+
       scale_fill_manual(values=c("grey95", "black"))+
        labs(y="Observation Year", x="Observation Month", fill="Kelp Reproductive?")
      
      # Extract the legend. Returns a gtable
      singleyearsleg <- get_legend(singleyears)
      # Convert to a ggplot and print
      singleyearsleg=as_ggplot(singleyearsleg)
      # remove legend for real plot
      singleyears = singleyears + theme(legend.position = "none")
      
      
      ##### make reproduction plot - for all years together ######
      
      ## subset data from user input 
      repro.gy <- subset(repro.year.sum, seaweed_id == input$seaweed_species)
      
      ## add placeholder year for plots to align
      repro.gy$year = "2022"
      
      allyears = ggplot(repro.gy, aes(x=month, y=year, fill=as.numeric(percent_years)))+
        geom_tile()+
        ggtitle(paste0(repro.gy$seaweed_id))+
        scale_fill_gradient(low="cornsilk", high="darkgoldenrod4")+
        labs(y="All Years", x=" ", 
             fill="Percent of survey years  
where reproductive 
individuals were 
observed (by month)")+
        theme(axis.text.y = element_text(color="white"),
              axis.text.x = element_blank())
      
      # Extract the legend. Returns a gtable
      allyearsleg <- get_legend(allyears)
      # Convert to a ggplot and print
      allyearsleg=as_ggplot(allyearsleg)
      # remove legend for real plot
      allyears = allyears + theme(legend.position = "none")
      
      ## arrange plots
      ggarrange(allyears, allyearsleg, singleyears, singleyearsleg,
                ncol=2, nrow=2, widths = c(0.75, 0.3, 0.75, 0.3), heights = c(0.1, 0.1, 1.5, 1.5))
      } ## end of else 
      
      ## end parentheses for  output$reproPlot <- renderPlot({
    },height = 500, width = 950
    )
    
    ###### downlaod and save plot ######
    output$downloadPlot <- downloadHandler(
      filename =  "seaweed_plot.png",
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
    
    
    ##### REMNDER IMAGE OF TRANSECT SLOPE PLOT #####
    output$TransImg <- renderImage({
      tr<-("./Data/transect_slope_image.jpg")
      list(src=tr, alt="Graph of slopes for all 3 transects", style="width: 1000px")
    }, deleteFile = FALSE)
    
    ####### create table of common names ######
    output$table = renderTable(commonnames)

    
}



##### APP #####
# Run the application 
shinyApp(ui = ui, server = server)
