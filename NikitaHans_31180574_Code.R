# Name: Nikita Hans
# Student id: 31180574
# Description: Data visualization project code

######################### Set your working directory here ####################################

setwd("C:/Users/Nikita Hans/Desktop/Monash Data Science/Semester_01/FIT5147 Data vis/Initial proposal/Bushfire Data")

##################### Reading data #####################################

boxPlot <- read.csv("BoxPlot_scan_data.csv")
barplot <- read.csv("Barplot_data.csv")
spread_animation_data <- read.csv("Spread_animation_data.csv")
lineplot_viirs <- read.csv("Lineplot_data_viirs.csv")
lineplot_modis <- read.csv("Lineplot_data_modis.csv")


################ List of libraries used #############################

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(shinythemes)
library(shiny)
library(plotly)
library(magrittr)
library(sqldf)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)
library(ggmap)
library(shinyalert)

######################### Data Preparation #############################

# VIIRS data

km.res <- kmeans(lineplot_viirs[0:2], 6, nstart = 25)

cluster_centroid<-data.frame(km.res$centers,c(1,2,3,4,5,6))
cluster_centroid<-cluster_centroid %>% rename(cluster = c.1..2..3..4..5..6.)

viirs_data<-cbind(lineplot_viirs[0:4],data.frame(km.res$cluster))
cluster_data_viirs<-viirs_data%>% rename(cluster = km.res.cluster)


#################### Define user for application ###############################

ui <- fluidPage(
  useShinyalert(),
  theme = shinytheme("united"),
  
######### Navigation page ##########
  
  navbarPage(
    "Australia Bushfire",
    
    ##### Tab panel 1 - About #####
    
    tabPanel(
      "About",
      style = "border-style: solid; border-color: black",
      
      h1("AUSTRALIA BUSHFIRE INSIGHTS", style = "color:black;text-align:center"),
      
      p(
        "Australian landscape have bushfires as its natural component. Bushfire is essential for plants to revive where plants have adjusted to the harsh conditions of the climate.
            With change in landscape from place to place in Australia, the bushfire risk and the timing of the bushfire seasons varies. The combination of weather and vegetation results
            in bushfire as it acts like a fuel for the fire. Impacts generated because of bushfire includes lightning, tornadoes and fire-storms which, in turn, can impact on fire behaviour.
            As Australia is a large country, factors of weather, vegetation and terrain vary widely across its area and distinct regional climates."
      ),
      p(
        "But 2019-20 bushfire season was apart
            from other yearly bushfires as normal bushfire begin in August. Today it is known to be black summer season because there was no normal start to it but an early one where thousands
            of fires burnt until summer was not over. 2019-20 bushfire was a devastating burn which was fuelled by record breaking temperature and terrible drought conditions. Bushfires can have
            a natural start or a deliberate start, it depends on many factors such as climate change, human activity, lightning strikes, humidity and wind speed. Since Australia is getting hotter
            over recent 10 years it leads to favourable conditions for drought to happen. Hot and dry climate is not a good sign as drier climates act as a fuel to fire."
      ),
      
      h2("About Project", style = "color:black;text-align:center"),
      
      p(
        "The main aim of the project is to show the insights and details of australia bushfire in the period November 2019 to January 2020. NASA sent two instruments MODIS and VIIRS installed
            in satellites to capture fire affected regions in Australia. This process will take place in three stages. Stage one shows the comparison between MODIS and VIIRS in order to choose the
            most reliable dataset to perform further analysis. Stage two shows the spread of fire in different regions and sites overall Australia. Stage three shows the density of fire for each
            day and where is that fire situated."
      ),
      
      p(
        "The project shows insights of Australia bushfire from the period of November 2019 to January 2020. It majorly answers three questions in stages where first stage is to compare
            instruments that captured affected regions. Second stage is to visualise and analyse the growth and expansion of fire over span of time using magnitudes and coordinates recorded
            by MODIS and VIIRS instruments. Stage three shows hotspot regions and time where fire was at peak and the pattern of fire during this time."
      ),
      
      p(
        "The main aim of this project is to understand and provide information to researchers and scientists about various patterns and behaviour of bushfire. The reason behind showing insights of this project
            is to avoid such massive bushfires and destructions in future and take necessary preventive measures in advance."
      ),
      
      h2("About Data", style = "color:black;text-align:center"),
      
      p(
        "The files are classified on the basis of satellites - MODIS (M6) and VIIRS (V1), further classified as Archive or NRT.
            MODIS (Moderate Resolution Imaging Spectroradiometer) represents each hotspot detection represents centre of 1km,
            meaning at least one fire is located in less than 1km region whereas VIIRS (Visible Infrared Imaging Radiometer Suite) has improved spatial resolution of 375m."
      ),
      
      p(
        "The Visible Infrared Imaging Radiometer Suite (VIIRS) is a sensor designed scanning radiometer that captures imagery and measurements of radiometric for land, atmosphere and ocean. It basically monitors reshaping of surface vegetation, land usage on a global scale."
      ),
      
      p(
        "The Moderate Resolution Imaging Spectrodiometer (MODIS) is a satellite based and earth viewing global imager that estimates various applications of earth such as land, atmosphere and ocean. Along Earth’s orbit, instrument uses a two-sided rotating scan mirror in order to gather continuous path of Earth surface."
      )
    ),
    
    
    ##### Tab panel 2 - Overview #####
    
    tabPanel("Overview",
             
             fluidPage(
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   wellPanel(
                     sliderInput(
                       "DatesMerge2",
                       "Dates:",
                       min = as.Date("01-11-2019", "%d-%m-%Y"),
                       max = as.Date("30-01-2020", "%d-%m-%Y"),
                       value = as.Date("01-11-2019", "%d-%m-%Y"),
                       animate = animationOptions(interval = 6000, loop = TRUE)
                     )
                   ),
                   wellPanel(
                     style = "background: white",
                     h5(
                       "Note: Plots may take 60-80 seconds to load due to large datasets"
                     )
                   )
                 ),
                 
                 mainPanel(
                   width = 8,
                   style = "border-style: solid; border-color: black",
                   fluidRow(column(
                     h3(p(
                       strong("Animation showing spread of bushfire"), style = "color:black;text-align:center"
                     )),
                     
                     br(),
                     
                     p(
                       "A map displays increase in fire in Australia as time escalates. The duration of spread shown is from November 2019 to January 2020. As dates elevate in the scroll bar, locations where bushfire happened are plotted.",
                       style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
                     ),
                     width = 12
                   )),
                   
                   withSpinner(plotOutput("animap", height = "600px"))
                 )
               )
             )),
    
    ##### Tab panel 3 - Instrument #####
    
    tabPanel(
      "Instrument",
      
      mainPanel(
        width = 8,
        style = "border-style: solid; border-color: black",
        fluidRow(column(
          h3(p(
            strong("MODIS instrument vs VIIRS instrument"), style = "color:black;text-align:center"
          )),
          
          br(),
          
          p(
            "There are two instruments MODIS and VIIRS with their own characteristics and technicalities. MODIS (Moderate Resolution Imaging Spectroradiometer) represents each hotspot detection around centre of 1km,
            meaning at least one fire is located in less than 1km region whereas VIIRS (Visible Infrared Imaging Radiometer Suite) has improved spatial resolution of 375m. Comparison between these instruments is done on the basis of spatial resolution.
            Spatial resolution is number of pixels utilized where a pixel represents an area on earth’s surface. There are two variables in the dataset scan and track which represents actual spatial resolution of a scanned pixel. Scan variable depicts
            spatial resolution in east-west direction whereas track variable depicts spatial resolution in north-south direction. In simple words, scan and track values show average value of ground covered.",
            style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
          ),
          width = 12
        )),
        
        withSpinner(plotlyOutput("boxplot",width = 1000)),
        fluidRow(column(
          h3(p(strong(
            "What are we seeing here?"
          ), style = "color:black;text-align:center")),
          
          br(),
          
          p(
            "MODIS instrument pixel values has a range between 1 and 4.80 and VIIRS instrument pixel values has a range between 0.32 and 0.8. Pixel value shows an average of ground area covered so
            for example let’s consider a value (say 2.3) indicates that 2.3 area of ground is covered and another value (say 0.6) indicates that 0.6 area of ground is covered. Clearly 0.6 shows that
            less ground area is covered which means that more detailed area is focused and captured. So lower the value of pixel, better is the reading of instrument. Hence VIIRS dataset is considered
            for further analysis and visualisations.",
            style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
          ),
          width = 12
        ))
      )
    ),
    
    ##### Tab panel 4 - Density of fire #####
    
    tabPanel("Density of fire",
             
             fluidPage(fluidRow(
               column = 4,
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   wellPanel(
                     sliderInput(
                       "DatesMerge",
                       "Dates:",
                       min = as.Date("01-11-2019", "%d-%m-%Y"),
                       max = as.Date("30-01-2020", "%d-%m-%Y"),
                       value = as.Date("01-12-2019"),
                       timeFormat = "%d-%m-%Y",
                       animate = animationOptions(interval = 1000, loop = TRUE)
                     )
                   ),
                   wellPanel(
                     style = "background: white",
                     h5(
                       "Note: Plots may take 60-80 seconds to load due to large datasets"
                     )
                   )
                 ),
                 
                 mainPanel(
                   width = 8,
                   style = "border-style: solid; border-color: black",
                   fluidRow(column(
                     h3(p(strong("Timeline Barplot"), style = "color:black;text-align:center")),
                     
                     br(),
                     
                     p(
                       "The timeline barplot shows FRP (Fire Radiant Power) for each day from November 2019 to January 2020. Fire Radiant Power is a technique that uses remotely sensed data to quantify burned biomass. It measures the radiant energy released by burning vegetation.
              It basically calculates the rate of radiant heat output from a fire. The bars in below barplot represents the FRP value for each date. These bars contain two colours red and yellow where red represents all the bars and yellow is the bar showing FRP value for
              the date selected from the scrollbar. Clearly, between January and December FRP was fluctuating and worst fire reaching its peak on January 8, 2020 with highest FRP value.",
                       style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
                     ),
                     width = 12
                   )),
                   
                   withSpinner(plotlyOutput("barplot",width = 1000)),
                   fluidRow(column(
                     h3(p(strong("Density Map"), style = "color:black;text-align:center")),
                     
                     br(),
                     
                     p(
                       "The density map displays locations with varying intensity of fire. Red markers shows the hotspot region, orange markers shows regions with moderate intesity and green markers show low intensity of fire. Intensity of fire is calculated by considering number of bushfires
              around that region. These are not static markers as they change their marking location for each day. Since day one intensity of fire may not match day twelve intensity of fire. So as you scroll along the dates in the scroll bar, marking locations will change for that particular date.
              Hovering over these markers will let you know the number of bushfires in that region.",
                       style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
                     ),
                     width = 12
                   )),
                   
                   withSpinner(leafletOutput("densityplot", height = "600px"))
                 )
               )
             ))),
  
    ##### Tab panel 5 - Spread of fire #####
    
    tabPanel("Spread of fire",
             
             fluidPage(
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   wellPanel(
                     style = "background: white",
                     selectInput(
                       "Instrument",
                       "Choose an instrument",
                       choices = c("MODIS", "VIIRS"),
                       selected = "VIIRS"
                     )
                   ),
                   wellPanel(
                     style = "background: white",
                     h5(
                       "Note: Processing time is high and plots may take 80-90 seconds to load due to large datasets"
                     )
                   )
                 ),
                 mainPanel(
                   width = 8,
                   style = "border-style: solid; border-color: black",
                   fluidRow(column(
                     h3(p(
                       strong("Variation in fire at different locations"), style = "color:black;text-align:center"
                     )),
                     
                     br(),
                     
                     p(
                       "The line chart shows variation in Fire Radiant Power value from November 2019 to January 2020. Fire Radiant Power is a technique that uses remotely sensed data to quantify burned biomass.
            It measures the radiant energy released by burning vegetation. It basically calculates the rate of radiant heat output from a fire. Each instrument captured bushfire data as per specified spatial resolution (seen in Instrument tab).
            Therefore calculation difference is present for FRP value for both instruments. In this line chart FRP value is shown for six locations to see the pattern of fire in different regions. Results of both the instruments is shown individually
            as you choose an instrument from the drop down tab.",
                       style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
                     ),
                     width = 12
                   )),
                   
                   withSpinner(plotlyOutput("linechart",width = 800)),
                   fluidRow(column(
                     h3(p(
                       strong("Spread of fire at different locations"), style = "color:black;text-align:center"
                     )),
                     
                     br(),
                     
                     p(
                       "The spread map shows location of six sites where pattern of FRP value differs. Each location shows their respective spread. In simple words, bigger the circle, more is the spread. In terms of FRP value it is not connected to the line chart as each
            circle is proportionate to number of locations. As regions go up in flames, ignition spreads and number of areas covered in fire increases with distance. Circle becomes larger to record fire covered areas showing increase in spread. You can view spread captured
            by both the instruments MODIS and VIIRS by choosing from the drop down tab.",
                       style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
                     ),
                     width = 12
                   )),
                   
                   withSpinner(leafletOutput("siteplot"))
                 )
               )
             ))
    

  )
)


############### Define server for application ######################

server <- function(input, output) {
  
  # Pop-up at the beginning of the application
  
  shinyalert(
    title = "Welcome to Australia bushfire application",
    text = "",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Let's Start!",
    confirmButtonCol = "papayawhip",
    timer = 0
  )
  
  # Bar plot
  
  output$barplot <- renderPlotly({
    ggplotly(
      ggplot(
        barplot %>% mutate(ToHighlight = ifelse(
          Date == format(input$DatesMerge, "%d-%m-%Y"),
          "User selected date",
          "Dates"
        )),
        aes(
          x = as.Date(barplot$Date, "%d-%m-%Y"),
          y = FRP,
          fill = ToHighlight,
          text = paste("Date: ", barplot$Date, "<br>FRP: ", FRP)
        )
      )
      + geom_bar(stat = "identity", width = .5)
      + xlab("Date") + ylab("Fire Radiant Power")
      + scale_fill_manual(
        values = c("User selected date" = "yellow", "Dates" = "red"),
        guide = FALSE
      ),
      tooltip = "text"
    )
  })
  
  
  # Animation spread map
  
  output$animap <- renderPlot({
    start <- as.Date("01-11-2019", format = "%d-%m-%Y")
    end   <- as.Date(as.character(input$DatesMerge2, "%d-%m-%Y"), format = "%d-%m-%Y")
    theDate <- start
    
    plot.new()
    
    plot(
      x = 1,
      type = "n",
      ylim = c(-9,-50),
      xlim = c(100, 160),
      pch = 16,
      ylab = "Latitude",
      xlab = "Longitude",
      main = "Spread of bushfire in Australia"
    )
    
    points(
      spread_animation_data$longitude[spread_animation_data$acq_date == "01-11-2019"],
      spread_animation_data$latitude[spread_animation_data$acq_date == "01-11-2019"],
      pch = 8,
      col = "orange"
    )
    
    while (theDate <= end)
      
    {
      points(
        spread_animation_data$longitude[spread_animation_data$acq_date == as.character(theDate, "%d-%m-%Y")],
        spread_animation_data$latitude[spread_animation_data$acq_date == as.character(theDate, "%d-%m-%Y")],
        pch = 8,
        col = "orange"
      )
      theDate <- theDate + 1
    }
    
  })
  
  # Box plot
  
  output$boxplot <- renderPlotly({
    ggplotly(
      ggplot(
        sqldf(
          "select 'Scan Modis' as FLag,scan_modis from boxPlot union select 'Scan Viirs' as Flag,scan_viirs from boxPlot"
        ),
        aes(
          x = FLag,
          y = scan_modis,
          fill = "gold1",
          legend = FALSE
        )
      )
      + theme_bw() + geom_boxplot()
      + scale_x_discrete(labels = c("MODIS", "VIIRS"))
      + xlab("Date") + ylab("Fire Radiant Power")
      + theme(legend.position = "none")
    )
    
  })
  
  
  # Line plot
  
  output$linechart <- renderPlotly({
    


    # MODIS data
    # 
    km.res.m <- kmeans(lineplot_modis[0:2], 6, nstart = 25)

    cluster_centroid<-data.frame(km.res.m$centers,c(1,2,3,4,5,6))
    cluster_centroid<-cluster_centroid %>% rename(cluster = c.1..2..3..4..5..6.)

    modis_data<-cbind(lineplot_modis[0:4],data.frame(km.res.m$cluster))
    cluster_data_modis<-modis_data %>% rename(cluster = km.res.m.cluster)

    # Date format

    cluster_data_viirs$acq_date<-as.Date(cluster_data_viirs$acq_date,"%d-%m-%Y")
    cluster_data_modis$acq_date<-as.Date(cluster_data_modis$acq_date,"%d-%m-%Y")

    # Combining data

    instrument_data<- sqldf("select 'VIIRS' as Flag,acq_date, frp, cluster from cluster_data_viirs union all select 'MODIS' as Flag, acq_date,frp, cluster from cluster_data_modis")
    
    # Plot
    
    filtered_data <-
      instrument_data[instrument_data$Flag == input$Instrument, ]
    
    
    ggplotly(
      ggplot(filtered_data,
             aes(
               x = acq_date,
               y = frp,
               group = 6
             )) + geom_smooth(aes(colour = cluster), se = FALSE) + facet_grid(~ cluster) + labs(y = "Fire Radiant Power", x = "Date") + theme(legend.position = "none")
    )
    
  })
  
  # Density map
  
  output$densityplot <- renderLeaflet({
    
    viirs_data_filtered <-
      lineplot_viirs[lineplot_viirs$acq_date == format(input$DatesMerge, "%d-%m-%Y"), ]
    
    km.res <- kmeans(viirs_data_filtered[0:2], 6, nstart = 25)
    
    cluster_centroid <- data.frame(km.res$centers, c(1, 2, 3, 4, 5, 6))
    
    cluster_centroid <-
      cluster_centroid %>% rename(cluster = c.1..2..3..4..5..6.)
    
    viirs_data_filtered <-
      cbind(viirs_data_filtered[0:4], data.frame(km.res$cluster))
    
    viirs_data_filtered <-
      viirs_data_filtered %>% rename(cluster = km.res.cluster)
    
    viirs_data_filtered <-
      sqldf(
        "select a.*,b.latitude as centroid_lat,b.longitude as centroid_lon from viirs_data_filtered as a left join cluster_centroid as b on a.cluster=b.cluster"
      )
    
    viirs_data_filtered <-
      sqldf(
        "select count(*) as cnt,centroid_lat,centroid_lon from viirs_data_filtered group by centroid_lat,centroid_lon"
      )
    
    getColor <- function(viirs_data_filtered) {
      sapply(viirs_data_filtered$cnt, function(val) {
        if (val <= median(viirs_data_filtered$cnt)) {
          "green"
        } else if (val < max(viirs_data_filtered$cnt)) {
          "orange"
        } else {
          "red"
        }
      })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(viirs_data_filtered)
      
    )
    
    leaflet(viirs_data_filtered) %>%
      
      addTiles() %>%
      
      addAwesomeMarkers(
        ~ viirs_data_filtered$centroid_lon,
        ~ viirs_data_filtered$centroid_lat,
        icon = icons,
        label = ~ viirs_data_filtered$cnt,
        labelOptions = labelOptions(noHide = F)
      )
    
  })
  
  # Spread map
  
  output$siteplot <- renderLeaflet({

    viirs_data_filtered <-
      sqldf(
        "select a.*,b.latitude as centroid_lat,b.longitude as centroid_lon from cluster_data_viirs as a left join cluster_centroid as b on a.cluster=b.cluster"
      )

    viirs_data_filtered <-
      sqldf(
        "select avg(power((latitude-centroid_lat)*(latitude-centroid_lat) + (longitude-centroid_lon)*(longitude-centroid_lon),1)) as spread_calc,
                                centroid_lat,centroid_lon,cluster from viirs_data_filtered group by centroid_lat,centroid_lon,cluster "
      )

    # MODIS data

    km.res.m <- kmeans(lineplot_modis[0:2], 6, nstart = 25)

    cluster_centroid_m <- data.frame(km.res.m$centers, c(1, 2, 3, 4, 5, 6))
    cluster_centroid_m <-
      cluster_centroid_m %>% rename(cluster = c.1..2..3..4..5..6.)

    modis_data_filtered <-
      cbind(lineplot_modis[0:4], data.frame(km.res.m$cluster))
    modis_data_filtered <-
      modis_data_filtered %>% rename(cluster = km.res.m.cluster)
    modis_data_filtered <-
      sqldf(
        "select a.*,b.latitude as centroid_lat,b.longitude as centroid_lon from modis_data_filtered as a left join cluster_centroid_m as b on a.cluster=b.cluster"
      )

    modis_data_filtered <-
      sqldf(
        "select avg(power((latitude-centroid_lat)*(latitude-centroid_lat) + (longitude-centroid_lon)*(longitude-centroid_lon),1)) as spread_calc,
                                centroid_lat,centroid_lon,cluster from modis_data_filtered group by centroid_lat,centroid_lon,cluster "
      )
    
    
    instrument_data_map <-
      sqldf(
        "select 'VIIRS' as Flag,centroid_lon,centroid_lat,spread_calc, cluster from viirs_data_filtered union all select 'MODIS' as Flag,
                                 centroid_lon,centroid_lat,spread_calc, cluster from modis_data_filtered"
      )
    
    filtered_data_map <-
      instrument_data_map[instrument_data_map$Flag == input$Instrument, ]
    
    colours <-
      colorFactor(
        c("red", "green", "blue", "purple", "yellow", "pink"),
        domain = unique(viirs_data_filtered$cluster)
      )
    
    leaflet(filtered_data_map) %>%
      addTiles() %>%
      
      addCircleMarkers(
        color = ~ colours(filtered_data_map$cluster),
        stroke = FALSE,
        fillOpacity = 0.5,
        lng = ~ centroid_lon,
        lat = ~ centroid_lat,
        label = ~ as.character(filtered_data_map$cluster),
        radius = filtered_data_map$spread_calc * 3
        
      )
  })
  
  
}


######## Create Shiny App #############

shinyApp(ui = ui, server = server)

