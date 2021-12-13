#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#1. Load all required libraries----
library(shiny)
library(shinythemes) 
library(shinydashboard)
library(htmltools)
library(leaflet)
library(highcharter)
library(reactable)
library(dplyr)
library(tidyr)
library(plotly)

#2. Load all required dataset----
load("ETA_NonRushHr.RData")
load("ETA_RushHr.RData")
load("Station_ETA_Summary.RData")
load("Overall_Summary.RData")
#font styling
t <- list(
  family = "News Cycle",
  size = 11
  )
#margin styling
mrg <- list(l = 50, r = 50,
            b = 50, t = 50,
            pad = 20)



# Define UI----
ui <- fluidPage(
    theme = shinytheme("journal"),
    # Application title
    titlePanel("The KMB Bus ETA info keeps changing - Should we rely on ETA info?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel = (""),
      


        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Experiencing delay with KMB ETA?", 
                     shiny::fluidRow(
                       column(
                         width = 6,
                         htmlOutput("introduction")
                       ),
                       column(
                         width = 6,
                         htmlOutput("introVideo")
                       ))
                     ),
            tabPanel("How to investigate?",
                     shiny::fluidRow(
                       column(
                         width = 6,
                         htmlOutput("methodology")
                       ),
                       column(
                         width = 6,
                         htmlOutput("method_datasource")
                       ))
                     ),
            tabPanel("How often?",
                     shiny::fluidRow(
                      column(
                        width = 5,
                        htmlOutput("freq")
                      ),
                      column(
                        width = 7,
                        htmlOutput("freq_KPI", width = 7),
                        plotlyOutput("freq_hist", height = 350)
                      ))
                     ),
            tabPanel("How long?", 
                     shiny::fluidRow(
                       column(
                         width = 5,
                         htmlOutput("keyFindings")
                       ),
                       column(
                         width = 7,
                         htmlOutput("long_KPI", width = 7),
                         plotlyOutput("long_hist", height = 350)
                       ))
                     ),
            tabPanel("When & Where?",
                     shiny::fluidRow(
                       column(
                         width = 5,
                         htmlOutput("whenWhere_1"),
                         plotlyOutput("busStop_freq_NRH", height = 240),
                         htmlOutput("whenWhere_2"),
                         plotlyOutput("busStop_freq_RH", height = 240)
                       ),
                       column(
                         width = 7,
                         htmlOutput("emptySpace1"),
                         leafletOutput("mapNonRush"),
                         htmlOutput("emptySpace2"),
                         leafletOutput("mapRush")
                       ))
            ),
            tabPanel("Which Bus Stops?", 
                     htmlOutput("busStop"), 
                     reactableOutput("BusStation"))
          ),
          width=12
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
#Background Section----  
    output$introduction <- renderUI(
      HTML(
      '<H2>The KMB ETA Delay Issue</H2>
       <p>According to the "Public Bus Services Ordinance" Chapter 230 of the Laws of Hong Kong, franchised bus companies are required to provide transportation services in accordance with the "Service Details Table" of each route. The frequency of the time period, etc. </p>
       <p>However, the Transport Department has not properly monitored the frequency of franchised buses in the past. Even if the bus arrival time is delayed, and the delay is close to the interval between two buses, from the perspective of the Department, it is not regarded as out of service. In fact, the Transport Department’s definition of bus off-duty is "the actual number of trips operated daily is less than the number of trips specified in its service schedule", rather than the number of trips in different time periods.</p>
       <p>The inconvenience caused to the public by the bus “Lost trips”.  In addition to being late for work, the cumulative waiting passengers increases if a shift is lost. Some of the passengers need to wait a few more buses to get on.</p> 
       <p>As early as 2021, members of the Legislative Council asked questions about "Lost trips”.  Ms Eva Cheng, the then Secretary for Transportation and Housing, indicated the government would closely monitor the bus services. They would work with those bus service providers to take improvement measures, such as revising driving routes, to stabilize the service quality. </p> 
       <p>Nevertheless, the same issue was mentioned in 2014 and 2019 Legislative Council meetings. Has the situation improved? According to data from the Transportation and Housing Bureau, the rate of “Lost trips” increased last year.</p> 
       <p>This paper is to evaluate the inaccuracy situation of KMB by examining their real time arrival data (ETA) and to discuss the potential causes by further examining other data. KMB is one of the main bus service providers in Hong Kong.</p> 
      ')
    )

    
    output$introVideo <- renderUI(
      HTML('
      <br/><br/>
      <video controls width=100%>
      <source src="news_trailer.mp4" type="video/mp4">
      </video>
      <p style="padding-left:10px;">(Video of HK citizens keep checking ETA with their mobiles, waiting long for the bus.)</p>
      ')
    )
        
    
#Methodology Section----  
    output$methodology <- renderUI(
      HTML(
        '<H2>Methodology</H2>
       <p>Our team adopts a series of tools and methods to acquire real time data of different periods and conduct analysis.</p>
       <p>For data source collection, we used the API offered on the data.gov.hk(hk open data website) to collect the data. A simple random sampling technique conducted in R programme was adopted with the purpose of getting an overview of the bus delay situation and reducing time consuming for data selection. A total number of 120 bus stations was selected to represent the entire KMB bus situation. However, please note that not every bus station is planned with bus arrival during the window of study. Eventually we have captured ETA data from 105 bus stations during non-rush hour and 109 bus stations during rush hour.</p>
       <p>The API returns a snapshot of the real time ETA data of a particular bus stop at a specific moment. To capture the change of ETA, the API needs to be run repetitively, with a time gap of 60 seconds between each data extraction job.</p>
       <p>A comparison will be made between the ETA collected in different snapshots. If the ETA in a later snapshot is greater than its previous one, this will be marked as a delayed case. Below is an example of how the data will be transformed. e.g. Bus Route = E42, Bus Stop = Lantau Link Toll Plaza</p>
       <table>
       <tr>
       <th style="background-color: whitesmoke;padding: 5px;">Snapshot Time</th>
       <th style="background-color: whitesmoke;padding: 5px;">ETA</th>
       <th style="background-color: whitesmoke;padding: 5px;">Is Delay Indicator</th>
       </tr>
       <tr>
       <td style="padding:5px;">2021-10-30 09:00:00</td>
       <td style="padding:5px;">2021-10-30 09:05:00</td>
       <td style="padding:5px;">N</td>
       </tr>
       <tr>
       <td style="padding:5px;">2021-10-30 09:01:00</td>
       <td style="padding:5px;">2021-10-30 09:05:00</td>
       <td style="padding:5px;">N</td>
       </tr>
       <tr>
       <td style="padding:5px;border-bottom: 3px solid whitesmoke;">2021-10-30 09:02:00</td>
       <td style="padding:5px;border-bottom: 3px solid whitesmoke;">2021-10-30 09:07:00</td>
       <td style="padding:5px;border-bottom: 3px solid whitesmoke;">Y</td>
       </tr>

       </table>
       <p style="font-size:12px;font-style:italic;">(The above is a simplified illustration, to know the details, please visit our GitHub page.)</p>
       <p>The percentage of delayed ETA is therefore equals to the count of Y in the Is Delay Indicator column, divided by the total number of snapshots. Note that the percentage of delayed ETA is calculated for a specific route at a specific bus stop during a specific period. Eventually these would be aggregated at bus stop level for evaluating the potential causes.</p>
       <p>Since the accuracy of ETA would be prone to period, for example, for peak hours the bus may be more likely to delay due to traffic jams, and at non-peak hours the situation will reverse. We selected 2 different time periods to represent each. For non-peak hours, we have collected data from 4 Dec 2215-2245, a total number of 38,255 data has been collected. For peak hours, we collected data from 6 Dec 1800-1830, with 41445 rows of record.</p>
       <p>Other factors that may potentially affect ETA may include geographical data – latitude and longitude of the bus stop is collected.</p>
      ')
    )

    output$method_datasource <- renderUI(
      HTML(
        '<br/><br/>
        <img src="datasource_photo.png" class="responsive" style="width:100%;height:auto;">
        <p style="padding-left:10px;">(Data collected from <a href="https://data.gov.hk/en-data/dataset/hk-td-tis_21-etakmb" target="_blank">data.gov.hk</a>)</p>
        ')
    )
    

#Delay Frequency Section----    
    output$freq <- renderUI(
      HTML(
        "<H2>26% of the time, the KMB ETA delay</H2>
       <p>According to our statistics, most bus stops have records of delay. Compared to the time record one minute ago, we observe a 26 percentage of delaying overall. Nearly 20 bus stops exhibited 25% to 30% delaying phenomenon during peak hours, which is also the highest off-duty probability. </p> 
       <br/>
       <H4>No significant difference between rush and non-rush hour </H4>
       <p>Among these records, the delaying rate at non-rush hours is 25.7%, and 26.2% for rush hours, which is approximately 0.5% higher than that at off-peak hours. In terms of distribution of the delaying, few impacts by period on delay rate could be captured by us, since there are quite a bit of non-rush hours performs higher number of delaying.</p> 
       <br/>
       <H4>The extreme cases: delay over 60% of the time</H4>
       <p>In all records, there are 3 bus stops with an off-duty rate even exceeding 60%, namely <b>Lai Chi Kok station (62%)</b>, <b>Full Silver Garden (60%)</b> and <b>San Hui Market (60%)</b>.</p> 
      ")
    )
    
    output$freq_KPI <- renderUI(
      HTML(paste0('<table>
                  <tr>
                  <th><H3 style = "color:wheat;font-size: 100%;padding-right: 15px;text-align: left;">Percentage of delay (non rush hour)</H3></th>
                  <th><H3 style = "color:tomato;font-size: 100%;padding-right: 15px;text-align: left;">Percentage of delay (rush hour)</H3></th>
                  </tr>
                  <tr>
                  <td><H1 style = "color:wheat; font-size: 5vw;padding-right: 15px;text-align: left;">', Overall_Summary$NRH.percentage_of_delay, '%</H1></td>
                  <td><H1 style = "color:tomato; font-size: 6vw;padding-right: 15px;text-align: left;">', Overall_Summary$RH.percentage_of_delay, '%</H1></td>'
                  ))
    )
    
    output$freq_hist <- renderPlotly({
      NRH.density <- subset(Station_ETA_Summary,NRH.percentage_of_delay>=0)
      RH.density <- subset(Station_ETA_Summary,RH.percentage_of_delay>=0)
      freq_density_chart <- plot_ly(alpha = 0.6) %>%
        add_histogram(x = RH.density$RH.percentage_of_delay, opacity = 0.9, color = I("tomato"), name = 'rush hour') %>%
        add_histogram(x = NRH.density$NRH.percentage_of_delay, opacity = 0.9, color = I("wheat"), name = 'non rush hour') %>%
        layout(title = list(text = "Number of bus stop at different frequency of delay", 
                            xanchor = "left", x=0, y = 0.98),
               yaxis = list(title = 'Number of bus stop'),
               xaxis = list(title = '% of delay'),
               barmode = "overlay", 
               font = t,
               margin = mrg,
               legend = list(x=0.8, y=0.98))  %>% 
        config(displaylogo = FALSE,
                                  modeBarButtonsToRemove = list(
                                      'sendDataToCloud',
                                      'toImage',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'hoverCompareCartesian', 
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d', 
                                      'zoomOut2d'
                                ))
      
    })

    
    


#Delay Duration Section----
    output$keyFindings <- renderUI(
      HTML(
        "<H2>On average, delays 21.8 - 25.4 seconds</H2>
       <p>In addition to the frequency of delaying, our project also examined the data by the length of delaying recorded at various bus stops. </p>
       <p>From the chart, we can see that most buses arrived at the station within 100 seconds after being late, and within the 100 seconds, as the delaying time increases, the bus station’s record of late incidents shows a <b>downward</b> trend. On average, the KMB ETA delaying time is between 21.8 to 25.4 second. The overall level of delaying period during peak hours is <b>higher</b> than that during off-peak hours.</p> 
       <br/>
       <H4>The extrem cases - delay more than 5 minutes after 1 minute passed</H4>
       <p>However, in the one-hour record, we also observed some extreme lateness: the records of three bus stops showed that there were buses that were late for more than 5 minutes. These records are observed from <b>Kowloon Central Post Office (302 seconds)</b>, <b>North Point Road (346 seconds)</b> and <b>City One Station (300 seconds)</b>.</p>
      ")
    )
    
    output$long_KPI <- renderUI(
      HTML(paste0('<table>
                  <tr>
                  <th><H3 style = "color:wheat;font-size: 100%;padding-right: 15px;text-align: left;">Average delay duration (non rush hour)</H3></th>
                  <th><H3 style = "color:tomato;font-size: 100%;padding-right: 15px;text-align: left;">Average delay duration (rush hour)</H3></th>
                  </tr>
                  <tr>
                  <td><H1 style = "color:wheat; font-size: 5vw;padding-right: 15px;text-align: left;">', Overall_Summary$NRH.average_delay, 's</H1></td>
                  <td><H1 style = "color:tomato; font-size: 6vw;padding-right: 15px;text-align: left;">', Overall_Summary$RH.average_delay, 's</H1></td>'
      ))
    )
    
    output$long_hist <- renderPlotly({
      NRH.density <- subset(ETA_NonRushHr,delay_seconds>0) %>%
        arrange(desc(delay_seconds))
      RH.density <- subset(ETA_RushHr,delay_seconds>0) %>%
        arrange(desc(delay_seconds))
      
      
      NRH.label <- NRH.density[which.max(NRH.density$delay_seconds),]
      RH.label <- RH.density[which.max(RH.density$delay_seconds),]
      label <- rbind(NRH.label, RH.label)
      
      
      freq_density_chart <- plot_ly(alpha = 0.6) %>%
        add_histogram(x = RH.density$delay_seconds, opacity = 0.9, color = I("tomato"), name = 'rush hour') %>%
        add_histogram(x = NRH.density$delay_seconds, opacity = 0.9, color = I("wheat"), name = 'non rush hour') %>%
        layout(title = list(text = "Number of delay observation and corresponding delay duration", 
                            xanchor = "left", x=0, y = 0.98),
               yaxis = list(title = 'Number of delay observation'),
               xaxis = list(title = 'Delay duration (seconds)'),
               barmode = "overlay", 
               font = t,
               margin = mrg,
               legend = list(x=0.83, y=0.87))  %>%
        add_annotations(x = label$delay_seconds,
                        y = 1,
                        text = paste("!",label$delay_seconds),
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = 20,
                        ay = -40) %>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = list(
                 'sendDataToCloud',
                 'toImage',
                 'autoScale2d',
                 'resetScale2d',
                 'hoverClosestCartesian',
                 'hoverCompareCartesian', 
                 'zoom2d',
                 'pan2d',
                 'select2d',
                 'lasso2d',
                 'zoomIn2d', 
                 'zoomOut2d'
               ))
      
    })
    
    
        
#When & where section----    
    output$whenWhere_1 <- renderUI(
      HTML(
        "<H2>West vs East and Non-Rush Hour vs Rush Hour</H2>
       <p>We also predict that KMB bus ETA is affected differently by different routes, and we assume that this difference may increase the passenger load of different routes, and this condition may cause a regional differentiation among transportation services. In this section, we show the geographical distribution of late arrivals on the map.</p>
       <p>The result presents a large differentiation among different districts in Hong Kong, and here are some interesting observations by our team in the aspect of location differences:</p>
       <p><b>Western Kowloon and New Territories</b> are with more delay during the <b>non-rush hour</b>, what’s more, all the top 5 record in the percentage of delay are coming from western.</p> 
      ")
    )

    output$whenWhere_2 <- renderUI(
      HTML(
        "<p>However, for <b>rush hour</b>, there is a clue of delaying coming from the <b>eastern</b> part of Hong Kong, with all the top 5 record of percentage of delaying coming from this area.</p>
        ")
    )
    
        output$busStop_freq_NRH <- renderPlotly({
          delay_freq_top5_NRH <- Station_ETA_Summary %>%
            arrange(desc(NRH.percentage_of_delay)) %>%
            head(5)
          
          NRH_5_BusStop_freq <- plot_ly(
            y=delay_freq_top5_NRH$data.name_en, 
            x=delay_freq_top5_NRH$NRH.percentage_of_delay, 
            type = 'bar', 
            orientation = 'h',
            color = I("wheat"),
            opacity = 0.9) %>%
            layout(
              yaxis = list(categoryorder = "total ascending"),
              font = t,
              margin = mrg,
              yaxis = list(title = 'Bus stop'),
              xaxis = list(title = '% of delay'),
              title = list(text = "Top 5 bus stops with most frequent delay (non-rush hour)", 
                           xanchor = "left", x=0, y = 0.98)) %>%
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = list(
                     'sendDataToCloud',
                     'toImage',
                     'autoScale2d',
                     'resetScale2d',
                     'hoverClosestCartesian',
                     'hoverCompareCartesian', 
                     'zoom2d',
                     'pan2d',
                     'select2d',
                     'lasso2d',
                     'zoomIn2d', 
                     'zoomOut2d'
                   ))
        })
        
        output$emptySpace1 <- renderUI(
          HTML("<br/><br/>")
        )
        
        output$emptySpace2 <- renderUI(
          HTML("<br/>")
        )
        
        
        output$busStop_freq_RH <- renderPlotly({
          delay_freq_top5_RH <- Station_ETA_Summary %>%
            arrange(desc(RH.percentage_of_delay)) %>%
            head(5)
          
          RH_5_BusStop_freq <- plot_ly(
            y=delay_freq_top5_RH$data.name_en, 
            x=delay_freq_top5_RH$RH.percentage_of_delay, 
            type = 'bar', 
            orientation = 'h',
            color = I("tomato"),
            opacity = 0.9) %>%
            layout(
              yaxis = list(categoryorder = "total ascending"),
              font = t,
              margin = mrg,
              yaxis = list(title = 'Bus stop'),
              xaxis = list(title = '% of delay'),
              title = list(text = "Top 5 bus stops with most frequent delay (rush hour)", 
                           xanchor = "left", x=0, y = 0.98)) %>%
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = list(
                     'sendDataToCloud',
                     'toImage',
                     'autoScale2d',
                     'resetScale2d',
                     'hoverClosestCartesian',
                     'hoverCompareCartesian', 
                     'zoom2d',
                     'pan2d',
                     'select2d',
                     'lasso2d',
                     'zoomIn2d', 
                     'zoomOut2d'
                   ))
        })
        
    
    output$mapNonRush <- renderLeaflet({
      addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
        colorAdditions <- paste0(colors, "; width:", sizes, "px;border-radius: 50%; height:", sizes, "px")
        labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
        
        return(addLegend(map, "bottomright", colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = '% of Delay'))
      }
      
      tag.map.title <- tags$style(HTML("
      .leaflet-control.map-title { 
        text-align: left;
        padding-left: 10px; 
        padding-right: 10px;
        padding-top: 5px;
        padding-bottom: 5px;
        background: rgba(255,255,255,0.75);
        font-weight: bold;
        font-size: 18px;
        font-family: News Cycle;
        }
      "))
      
      
      title <- tags$div(
        tag.map.title, HTML("Delay frequency and duration for each bus stop - non-rush hour")
      )  

      NRH.map <- subset(Station_ETA_Summary,NRH.percentage_of_delay>=0)
      pal <- colorNumeric(palette = 'Reds', domain = c(0:55))
      leaflet(NRH.map) %>% 
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(lat = ~data.lat, 
                         lng = ~data.long, 
                         radius = ~NRH.percentage_of_delay*0.2,
                         color= ~pal(NRH.average_delay), 
                         label= lapply(paste0("Bus Station: ", NRH.map$data.name_en, "<br/>",
                                              "% of Delay: ", NRH.map$NRH.percentage_of_delay, "%<br/>",
                                              "Average Delay Duration:", NRH.map$NRH.average_delay),htmltools::HTML),
                         stroke = FALSE,
                         fillOpacity = 0.5) %>%
        addLegend("bottomright", pal = pal, values = c(0:55),
                  title = 'Delay duration',
                  labFormat = labelFormat(suffix = " seconds"),
                  opacity = 1
        )%>%
        addLegendCustom(colors = c("grey", "grey", "grey"), labels = c("20%", "40%", "60%"), sizes = c(8, 16, 24)) %>%
        addControl(title, position = "bottomleft", className="map-title")
    
    })
    
    output$mapRush <- renderLeaflet({
      addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
        colorAdditions <- paste0(colors, "; width:", sizes, "px;border-radius: 50%; height:", sizes, "px")
        labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
        
        return(addLegend(map, "bottomright", colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = '% of Delay'))
      }
      
      tag.map.title <- tags$style(HTML("
      .leaflet-control.map-title { 
        text-align: left;
        padding-left: 10px; 
        padding-right: 10px; 
        background: rgba(255,255,255,0.75);
        font-weight: bold;
        font-size: 18px;
        }
      "))
      
      
      title <- tags$div(
        tag.map.title, HTML("Delay frequency and duration for each bus stop - rush hour")
      )  
      
      RH.map <- subset(Station_ETA_Summary,RH.percentage_of_delay>=0)
      pal <- colorNumeric(palette = 'Reds', domain = c(0:55))
      leaflet(RH.map) %>% 
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(lat = ~data.lat, 
                         lng = ~data.long, 
                         radius = ~RH.percentage_of_delay*0.2,
                         color= ~pal(RH.average_delay), 
                         label= lapply(paste0("Bus Station: ", RH.map$data.name_en, "<br/>",
                                              "% of Delay: ", RH.map$RH.percentage_of_delay, "%<br/>",
                                              "Average Delay Duration:", RH.map$RH.average_delay),htmltools::HTML),
                         stroke = FALSE,
                         fillOpacity = 0.5) %>%
        addLegend("bottomright", pal = pal, values = c(0:55),
                  title = 'Delay duration',
                  labFormat = labelFormat(suffix = " seconds"),
                  opacity = 1
        )%>%
        addLegendCustom(colors = c("grey", "grey", "grey"), labels = c("20%", "40%", "60%"), sizes = c(8, 16, 24)) %>%
        addControl(title, position = "bottomleft", className="map-title")
      
    })
    
    


#Bus Station Section----
    output$busStop <- renderUI(
      HTML(
        '<H2>More about each bus stop</H2>
       <p>Here is the full list of our record to show you the delaying situation we captured during have an hour for both rush hour and non-rush hour of each individual bus station. </p>
       <p>By typing the bus station name in the search box, you will be present the detailed information for the corresponding bus station. Also, please make use of the sorting function to find out the rank of delay duration of the bus stops.</p>
       <p style="font-style: italic; font-size: 12px">***Note: Not all stations are with data for both rush hour/non-rush hour</p>
      ')
    )
    
  output$BusStation <- renderReactable({
    busStop_table <- Station_ETA_Summary %>%
      select(data.name_en, 
             NRH.percentage_of_delay,
             NRH.average_delay,
             NRH.maximum_delay,
             RH.percentage_of_delay,
             RH.average_delay,
             RH.maximum_delay) %>%
    reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    columns = list(
      data.name_en = colDef(name = "Bus Station"),
      NRH.percentage_of_delay = colDef(
        name = "Frequency of Delay (%)",
        na = "-", 
        format = colFormat(suffix = "%")),
      NRH.average_delay = colDef(
        name = "Average delay duration (seconds)",
        na = "-", 
        format = colFormat(suffix = "s")),
      NRH.maximum_delay = colDef(
        name = "Maximum delay duration (seconds)",
        na = "-", 
        format = colFormat(suffix = "s")),
      RH.percentage_of_delay = colDef(
        name = "Frequency of Delay (%)",
        na = "-", 
        format = colFormat(suffix = "%")),
      RH.average_delay = colDef(
        name = "Average delay duration (seconds)",
        na = "-", 
        format = colFormat(suffix = "s")),
      RH.maximum_delay = colDef(
        name = "Maximum delay duration (seconds)",
        na = "-", 
        format = colFormat(suffix = "s"))
    ),
    columnGroups = list(
      colGroup(name = "Non-Rush Hour", columns = c("NRH.percentage_of_delay", "NRH.average_delay", "NRH.maximum_delay")),
      colGroup(name = "Rush Hour", columns = c("RH.percentage_of_delay", "RH.average_delay", "RH.maximum_delay"))
    ),
    theme = reactableTheme(
      style = list(fontFamily = "News Cycle")
    ))
  })
  

  }


# Run the application---- 
shinyApp(ui = ui, server = server)
