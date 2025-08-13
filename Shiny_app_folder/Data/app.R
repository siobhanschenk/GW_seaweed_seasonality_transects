## Code by Siobhan Schenk

##### WORKSPACE SET UP ######
## load packages
library(shiny)
library(ggpubr)
library(plyr)

library(ggplot2); theme_set(theme_bw()+
                                theme(panel.grid = element_blank(),
                                      strip.background = element_rect(fill="white"),
                                      axis.text = element_text(size = 12),
                                      axis.title = element_text(size=20, face="bold"),
                                      strip.text = element_text(size = 12, face="bold"),
                                      legend.text=element_text(size=12),
                                      legend.title=element_text(size=12),
                                      plot.title = element_text(size=20, face="italic")))

## load data from Borealis https://doi.org/10.5683/SP3/IKGB6E
algae.wide.grouped = readRDS("./app_transect_data_formatted.RDS")
reproduction = readRDS("./app_repro_data_formatted.RDS")


##### MAKE ELEMENTS NEEDED FOR SERVER ######

## species list for dropdown selection
speclist = c(unique(algae.wide.grouped$seaweed_id))


###### UI CODE #######
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    ##### Application title ######
    titlePanel("Monthly Seaweed Survey at Girl in a Wetsuit (Stanley Park, Vancouver, BC)"),
    
    
    ##### Sidebar layout with input and output definitions ----
    sidebarLayout(            
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            HTML('<h3>Study Information</h3>',
                 '<p>These montly seaweed seasonality transcts were initiated and currently organized by 
      Siobhan and Varoon from the Parfrey</a> and Martone
      labs at the Univeristy of British Columbia, Vancouver Campus.</p>',
                 '<p>Now, Evan Kohn from the Angert lab at UBC has joined the transect leadership team since Siobhan  graduated from her PhD.</p>',
                 
                 '<p>For information about the study, see our <strong> <a href="https://doi.org/10.1139/cjb-2024-0109" target="_blank">published article in Botany</a></strong>. 
                 To download the raw data and access other study-related material, visit <a href="https://doi.org/10.5683/SP3/IKGB6E" target="_blank">Borealis</a>.</p>',
                 
                 '<p>To see how this app was built, visit <a href = "https://github.com/siobhanschenk/GW_seaweed_seasonality_transects", target="_blank">GitHub</a>.</p>',
                 
                 '<p>If you would like to take part in our seaweed survey, please email us! 
      <i>varoonp@student.ubc.ca</i>, <i>evankohn@student.ubc.ca</i>, and <i>siobhanschenk@gmail.com</i></p>'),

            
            br(),
            
            HTML('<h3>Acknowledgements </h3>',
                 '<p>Dr. B. Clarkston for her invaluable help to identify macroalgae and suggestions on how to improve the study and data usability. </p>
                <p> Volunteers who donated their time to help us collect these data. In particular: A. Choinski, A. Jackman, A. Palacios, A. Simon, B. Clarkston, C. Wardrop, E. Evans,
                 E. Jourdain, E. Menchions, E. Kohn, E. Porcher, G. Ainsworth-Cruickshank, N. Salland, M. Herrin, P. Lund, P. Schenk, R. Burns, R. Perovich, R. Ogushi, V. Billy</p>'),
            br(),
            
        ),
        
        ## end of sidebarPannel
        
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            ##### more plot text ######
            HTML("<h3><b>Choose a seaweed to plot</b> </h3>") ,
            
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
                 
                 '<p> <i> Note for the heatmap (1):</i> <b>Grey</b> boxes indicate that the algae was
             not found in the quadrat. <b>Empty</b> regions (where the heatmap grid from 0 to n metres from the seawall ends) on the graph indicate that there is no data available for that transect for that month.
             This is because we could not sample due to the tide height, or other unforseen events.</p>' ,
                 
                 '<p> <i> Note for the bubble plot (2):</i> We present a bubble plot for algae where the difference between the maximum and minimum relative abundance reccorded was less than 3.</p>' ,
                 
                 '<p> <i> Note (3):</i> The fill scale changes for each seaweed species. Make sure to check the color scale bar on the right of the plot when comparing relative abundances.</p>'),
            
            br(),
            
            
            HTML('<h4><b><i>Laminariales</i> (kelp) Reproductive Timing Plot Tab</b></h4>',
                 '<p>Shows presence/abscence of reproductive <i>Laminariales</i> (kelp).
             From the start of data up to (and including) October 2023, we only recorded presence/abscence data.
             Starting in November 2023, we started counting the number of reproductive individuals during a time search but we only present presence/abscence data here. See Borealis for more details.
             With both types or reprdutive data, times where reproduction was not reccorded should not be regarded as a true abscence of reproductive individuals. 
             Reproductive status of other seaweeds was not recorded.</p>'
            ),
            
            
            tabsetPanel(type="tabs",
                        tabPanel("Abundance Plot", plotOutput("distPlot", width = "90%")),
                        tabPanel("Laminariales (kelp) Reproductive Timing Plot", plotOutput("reproPlot", width="90%"))),
            
            
            
            ##### leave space at the bottom of the page #####
            br(),
            br(),
            
            
        ) ## end of mainPannel
    ), ## end of sidebarLayout
) ## end of ui



##### SERVER CODE #####

# Define server logic required to draw a histogram
server <- function(input, output){

    
    
    ##### ALGAE ABUNDANCE - plot by user input #######
    output$distPlot <- renderPlot({
        
        
        ## subset data from user input 
        df.subset <- subset(algae.wide.grouped, seaweed_id == input$seaweed_species)
      
        
        ## use to make the 0s white 
        df.subset$mean <- ifelse(df.subset$mean==0,NA,df.subset$mean)
        
        ## count number of entries for the seaweed
        count.algae = subset(df.subset, df.subset$mean>0)
        count.algae = ddply(count.algae, c("seaweed_id"), summarise, 
                            n_counts = length(mean), 
                            max_mean = max(mean),
                            min_mean = min(mean)) |> 
          mutate(range = max_mean - min_mean)
        
        if(count.algae$range>3){
          
          ## use gsub to change phylum colors to better color
          df.subset$phylum = gsub("brown", "darkgoldenrod4", df.subset$phylum)
          df.subset$phylum = gsub("red", "maroon4", df.subset$phylum)
          df.subset$phylum = gsub("green", "forestgreen", df.subset$phylum)

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
            )+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  strip.text.y = element_text(angle = 0))+
            ggtitle(paste0(df.subset$seaweed_id))}
        else{
          ### bubble plot if have less than 3 in mean ra difference
          
          ## remove times wher there was none of that algae
          df.subset = subset(df.subset, !is.na(df.subset$mean))
          
          ggplot(df.subset, aes(x=month, y=as.factor(quadrat_height_m), size=mean))+
            geom_point()+
            facet_grid(.~year, scales="free", space="free")+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  strip.text.y = element_text(angle = 0))+
            ggtitle(paste0(df.subset$seaweed_id, " (", df.subset$phylum, ")"))+
            labs(x="Sampling Month", y="Chart datum quadrat height (m)", 
                 fill="Mean Percent Cover")+
            scale_color_manual(values=c(unique(df.subset$phylum)))
          } ## end of else
        
        
    } ,height = 700, width = 700 )
    
    ##### KELP REPRODUCTION - plot by user input #######
    output$reproPlot <- renderPlot({
        
        ## subset data from user input 
        repro.subset <- subset(reproduction, seaweed_id == input$seaweed_species)
        
        
        ##### SET UP DESCISION TO PLOT REPRODUCTION DATAT OR NOT #####
        ## count occurence of reproduction data
        descision = sum(as.numeric(repro.subset$repro_yn))
        
        ## were any seaweeds reproductive?
        if(descision<1){
            ## no observations
            ggplot(repro.subset, aes(x=input$seaweed_id, 
                                     y = repro_yn))+
                geom_point()+
                annotate("text", x = 0.1, y = 0.1, label = "No reproductive data available for this seaweed", size=10)+
                theme(axis.text = element_blank(), 
                      axis.title = element_blank(),
                      axis.ticks = element_blank())
            
        } ## end of if
        else {
            ## some observations (make plot)
            
            
            ##### MAKE PLOT FOR REPRODUCTION #####

            singleyears = ggplot(repro.subset, aes(x=month, y=as.factor(year), color=repro_yn))+
                geom_point(cex=5, pch=19)+
                scale_color_manual(values=c("grey95", "black"))+
                labs(y="Observation Year", x="Observation Month", color="Kelp Reproductive (1=yes, 0=no)")
            
            # Extract the legend. Returns a gtable
            singleyearsleg <- get_legend(singleyears)
            # Convert to a ggplot and print
            singleyearsleg=as_ggplot(singleyearsleg)
            # remove legend for real plot
            singleyears = singleyears + theme(legend.position = "none")
            
            ## arrange plots
            ggarrange(singleyears, singleyearsleg,
                      ncol=2, nrow=2, widths = c(0.75, 0.3), heights = c(0.1, 0.1))
        } ## end of else 
        
        ## end parentheses for  output$reproPlot <- renderPlot({
    },height = 500, width = 950
    )
    

    
    
}



##### APP #####
# Run the application 
shinyApp(ui = ui, server = server)
