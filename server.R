# server.R
library(shiny)
library(ggplot2)
library(ggmap)
library(dplyr)
library(leaflet)
library(tidyverse)


## -----------------------------------------------------------------------------
##   Data preparation
## -----------------------------------------------------------------------------

fileurl <- "data/airbnb.csv"
file.exists(fileurl)
airbnb <- read.csv(fileurl,  stringsAsFactors = FALSE)
airbnb$review_scores_rating[is.na(airbnb$review_scores_rating)]<-0
print(str(airbnb))


airbnb$color <- "red"


icons <- awesomeIcons(
  icon = 'home',
  iconColor = 'black',
  library = 'glyphicon',
  markerColor = airbnb$color
)


## -----------------------------------------------------------------------------
##   MODEL CALCULATIONS
## -----------------------------------------------------------------------------


library(xgboost)

#Load Model
price_model <- xgb.load("data/price_model")
occupancy_model <- xgb.load("data/occupancy_model")

pred_df <- read.csv("data/features_final.csv")
#occupancy rate from this is 0.032666
pred_df[] <- lapply(pred_df, as.numeric)
print(pred_df)

#Drop occupancy rate







## -----------------------------------------------------------------------------
##   Distance Calculations
## -----------------------------------------------------------------------------



library(tidyverse)
library(geosphere)

mel_stations <- read.csv('data/ptv_geocode.csv', fileEncoding = "latin1")

trams <- mel_stations %>%
  filter(stop_type == 'train')

trains <- mel_stations %>%
  filter(stop_type == 'tram')

trip_advisor <- read.csv('data/tripadvisor_geocode.csv',fileEncoding = "latin1")

cbd_coor <- data.frame('lat' = -37.8098, 'lon' = 144.9652)

calc_distance <- function(df1, df2) {
  df1 %>%
    apply(1, function(x){distm(c(as.numeric(x['lon']),as.numeric(x['lat'])), 
                               df2[,c('lon','lat')], 
                               fun = distVincentyEllipsoid)
    })
}


generate_distance_features <- function(df) {
  # takes a dataframe with lat and lon
  df %>%
    generate_tripadvisor_features %>%
    generate_cbd_features %>%
    generate_public_transport_features 
}

generate_cbd_features <- function(df) {
  df %>%
    mutate(dist_cbd = calc_distance(cbd_coor,df)
    )
} 

generate_public_transport_features <- function(df) {
  df %>%
    mutate(dist_train = 
             (trains %>%
                calc_distance(df) %>% 
                min())
    ) %>%
    mutate(dist_tram = 
             (trams %>%
                calc_distance(df) %>% 
                min()))
}

generate_tripadvisor_features <- function(df) {
  distance_to_spots = calc_distance(trip_advisor, df)
  df %>%
    mutate(
      dist_200 = sum(distance_to_spots < 200),
      dist_500 = sum(distance_to_spots < 500),
      dist_1000 = sum(distance_to_spots < 1000),
      closest_attr_distance = min(distance_to_spots),
      median_attr_distance = median(distance_to_spots),
      mean_attr_distance = mean(distance_to_spots)
    )
}










## -----------------------------------------------------------------------------
##   Ed's stuff
## -----------------------------------------------------------------------------
loc_ed <- c("data/prop_rent_buy.csv")
df_ed <- read.csv(file=loc_ed, header=TRUE)
colnames( df_ed ) <- c('x', 'suburb',
                       '1 bed unit median', '2 bed unit median', '3 bed unit median',
                       '2 bed house median', '3 bed house median', '4 bed house median',
                       'house median price', 'unit median price')

propertyPriceInfo <- function(property_info, rent_or_buy){
  # Define format variables
  # Set working directory and dataframe

  
  # Name convention formatting
  property_format <- list('style', 'bedrooms','bathrooms','street','suburb')
 
  
  # Assign column calls for unit and house variables
  unit = c(colnames( df_ed )[3],colnames( df_ed )[4],colnames( df_ed )[5])
  house = c(colnames( df_ed )[6],colnames( df_ed )[7],colnames( df_ed )[8])
  unit
  house
  
  # Generate bed, suburb, and property style (house or unit) from property_info argument
  bed <- as.numeric(property_info[[match(c('bedrooms'),property_format)]])
  suburb <- property_info[[match(c('suburb'),property_format)]]
  style <- property_info[[match(c('style'),property_format)]]
  flag <- c(rent_or_buy)
  
  # pull pricing data based on property type and bedroom count if rental price is sought
  if (style == 'house'){
    buy_col = 'house median price'
    room = min(4,as.integer(bed))
    room = max(2,as.integer(bed))
    room = house[[room-1]]
  } else if (style == 'unit'){
    buy_col = 'unit median price'
    room = min(4,as.integer(bed))
    room = max(2,as.integer(bed))
    room = unit[[room]]
  }
  print(room)
  # If/else for 'rent' or 'buy' as option;
  #       if rent: value equals the value for room type for a given suburb, so long as the suburb is represented in the dataframe
  #       if buy: value equals the value for property type for a given suburb, so long as the suburb is represented in the dataframe
  if (flag == 'rent'){
    if (suburb %in% df_ed$suburb){
      value = df_ed[[room]][df_ed$suburb == suburb]
      print_value = as.numeric(as.character(value))
      # Final print out - rental income for property
      print(c("The median monthly rental price for this type of property is $",
              format(print_value*4, format="d", big.mark=",")),quote=FALSE)
    } else
      print("We don't have enough data for that suburb, sorry!")
  } else if (flag == 'buy'){
    if (suburb %in% df_ed$suburb){
      value = df_ed[[buy_col]][df_ed$suburb == suburb]
      print_value = as.numeric(as.character(value))*4
      # Final print out - purchasing a property
      print(c("The median purchase price for the last quarter for this class of property is $",
              format(print_value,format="d", big.mark=",")), quote=FALSE)
    } else
      print("We don't have enough data for that suburb, sorry!")
  }
}


# Example properties
prop_1 <- c('house', 3, 4, '22 Mary Street', 'armadale')
prop_2 <- c('unit', 3, 4, '22 Bob Street', 'elwood')
propertyPriceInfo(prop_1, 'rent')
propertyPriceInfo(prop_1, 'buy')
propertyPriceInfo(prop_2, 'rent')
propertyPriceInfo(prop_2, 'buy')




## -----------------------------------------------------------------------------
##    Variables and Data for Plotting - colour palette, labels, lats/longs
## -----------------------------------------------------------------------------

# Create colour palette to be used by both ggplot and leaflet
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colour_palette <- gg_color_hue(10)

#Get the distinct locations for each site
locations <-  airbnb # potentially filter by state
#%>%




## -----------------------------------------------------------------------------
##          SHINY SERVER 
## -----------------------------------------------------------------------------


# Define server logic required to plot coral data

shinyServer(function(input, output) {
  
  # ------ SELECTORS:  ---
  

  thisLocation <- data.frame('lat' = -37.81, 'lon' = 144.98)
  thisCity <- "Melbourne"
  
  
  
  
  
  
  
  
  # ------ LEAFLET MAP  -------------------------
  
  # Define popup style and text to be used for leaflet map
  #locations$popup <- paste("<span  class='text-info', style='text-align:center;
  #                          width: 140px;
  #                          display: inline-block;
  #                          font-size:18px;
  #                          color: #ffffff;
  #                          >",
  #                          locations$city,"</span>
  #                          <p class='text-info'>Latitude = ",locations$latitude, "
  #                          <br>Longitude = ", locations$longitude,"<br>")
  # 
  # locations$popup 
  # 
  
  
  get_latlong <- function(address) {
    geocode(address,output = 'more')
  }
  
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  

    
  
  
  property_lat_lon <- eventReactive(input$go, {
    locn <- get_latlong(input$address)
    thisLocation$lat <<- locn$lat
    thisLocation$lon <<- locn$lon
    thisCity <<- as.character(locn$locality)
    update_prediction_df(thisLocation, thisCity)
    print(thisCity)

    
    
    
    
    locn

  })
  

  
  
  
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$property_lat_lon <- renderText({
    paste("lon:",property_lat_lon()$lon, "lat:",property_lat_lon()$lat, sep = ' ')
  })
  
  
  locations$label = paste(locations$city,'<p>',format(locations$price))
  
  
  # # Define label options to be used
  label_opt <- labelOptions(noHide = F,
                            direction = 'top',
                            textOnly = T,
                            offset=c(-1,-32),
                            style = list('color' = 'dark blue','font-family' = 'sans-serif',
                                         'font-size' = '14px'))

  


  


  # Initialise --> Create leaflet map tiles + circle markers + labels 
  map <- leaflet() %>%
    addTiles() %>%
    addAwesomeMarkers(lat = locations$latitude, 
                     lng = locations$longitude, 
                     layerId = locations$site,
                     label = lapply(locations$label,HTML),
                     labelOptions = label_opt,
                     icon = icons,
                    clusterOptions = markerClusterOptions(maxClusterRadius=30)
    )
  
  # Prepare the leaflet map for the UI
  output$mapPlot <- renderLeaflet(map)
  
  

  
  
  
 
  
  # POP-UPS ... showing/hiding contolled by the value in the input$site box
  # either selected manually, or as a result of clicking marker on map
  
  # Add observation for the site input box
  # # Remove any previous popups, then add the popup corresponding to the selected site
  #  observeEvent(input$site, {
  #    leafletProxy("mapPlot") %>% removePopup(layer="popups")%>%
  #       addPopups(show_locations$longitude,
  #                 show_locations$latitude,
  #                 popup = show_locations$popup,
  #               layer = "popups")
  # })
  # 
  # Add observations for marker_click event --> set the input$site value, 
  ## which in turn adds a pop-up to the click
  observe({
    click <-input$mapPlot_marker_click
    if(is.null(click)) { return()}
    output$selectorTwo <-renderUI({
      selectInput("site", "Select Site: (or click on map)", selector_Two, selected=2)
      #print(click$latitude)
    })
  })
  atts = ""
  observe({
      atts <- input$attributes
      checksum <- paste0(input$property_type ,input$room_type, input$month_discount,
              input$guests_included,input$extra_people, input$bedrooms,input$beds ,
        input$bathrooms, input$accommodates ,input$week_discount)
      print(thisLocation)
      print(thisCity)
      update_prediction_df(thisLocation, thisCity)
      
   
  })
  
  
  # Add observations for map_click event
  # If the map is clicked reset the site choice to "All Sites"
  observe({
    mapclick <-input$mapPlot_click
    
    if(is.null(mapclick)) {return()} # handle startup
    print("mapPlot_click changed sitelist")
    #output$selectorTwo <-renderUI({
    #  selectInput("site", "Select Site: (or click on map)", selector_Two)
    #})
  })
  
  
  checks <- c("shampoo","self_checkin","hair_dryer","iron","hangers","lockbox",
              "hour24_check","doorman_entry","wheelchair_access","laptop_friendly",
              "familykid_friendly", "buzzer","essentials","breakfast",
              "pool","internet","free_parking","elev_in_build")
  
  check_labels <- c("Shampoo","Self Check-In","Hair Dryer","Iron","Clothes Hangers","Lockbox",
                    "24 hour Check-In","Doorman Entry","Wheelchair Accessible","Laptop friendly workspace",
                    "Family/kid Friendly", "Buzzer / Wireless Intercom","Essentials","Breakfast",
                    "Pool","Internet","Free Parking on Premises", "Elevator")
  
  make_attributes <- function(features){
    for (c in check_labels){
      ##
    }
  
  }
  
  
  # Add observation for the reset button, resets all choices and redraws plots
  observeEvent(input$estimate,{
    # output$mapPlot <- renderLeaflet(map)
    update_prediction_df(thisLocation, thisCity)
    #print(input$attributes)
    #make_attributes(input$attributes)
    

    
  })
  
  update_prediction_df <- function(theLocation, theCity){
    print(pred_df)
    print(input$property_type)
    print(input$room_type)
    pred_df[grep("property",names(pred_df))] <- 0
    pred_df[,as.numeric(input$property_type)] <- 1
    pred_df[grep("room_type",names(pred_df))] <- 0
    pred_df[,as.numeric(input$room_type)] <- 1
    pred_df['accommodates'] <- input$accommodates
    pred_df['bathrooms'] <- input$bathrooms
    pred_df['bedrooms'] <- input$bedrooms
    pred_df['beds'] <- input$beds
    pred_df['guests_included'] <- input$guests_included
    pred_df['extra_people'] <- input$extra_people
    pred_df[19:36] <- 0
    for (att in input$attributes){
      print(att)
      pred_df[checks[which(check_labels==att)]]<- 1
    }
    print(theLocation)
     distances <- generate_distance_features(theLocation)
    
    pred_df['attr_less_200'] <- distances$dist_200
    pred_df['attr_less_500'] <- distances$dist_500
    pred_df['attr_less_1000'] <- distances$dist_1000
    pred_df['nearest_train_dist'] <- distances$dist_train
    pred_df['nearest_tram_dist'] <- distances$dist_tram
    pred_df['closest_attr_dist'] <-  distances$closest_attr_dist  
    pred_df['median_attr_dist'] <-  distances$median_attr_dist
    pred_df['mean_attr_dist'] <- distances$mean_attr_dist
    pred_df_two <- cbind(pred_df)
    pred_df_two$price <- predict(price_model, as.matrix(pred_df_two))
    
    pred_df_two$price_pp <- pred_df_two$price / pred_df_two$guests_included
    pred_df_two$occupancy_rate <- predict(occupancy_model, as.matrix(pred_df_two))
    pred_df_two$rev_per_month <- pred_df_two$occupancy_rate * pred_df_two$price * 30
    
    
    price <- round(pred_df_two$price[1],2)
    occ <-  round(pred_df_two$occupancy_rate[1]*100)
    price_m <- format(round(pred_df_two$rev_per_month[1],2),format="d", big.mark=",")
    if(input$property_type==3) {h_type <- "house"} else {h_type <-"unit"}
    prop_v <- c(h_type, (input$bathrooms), (input$bedrooms), '22 Bob Street', tolower(theCity))
    print(prop_v)
    #propertyPriceInfo(prop_1, 'rent')
    #propertyPriceInfo(prop_1, 'buy')
    
    
    
    output$results_detail0 <- renderText({paste0("AirBnB:  ", theCity)})
    output$results_detail1 <- renderText({paste0("Price per Night:  $", price)})
    output$results_detail2 <- renderText({paste0("Expected Occupancy: ", occ, "%")})
    output$results_detail3 <- renderText({paste0("Estimated Revenue per Month: $", price_m)})
    output$results_detail4 <- renderText({propertyPriceInfo(prop_v, 'rent')})
    output$results_detail5 <- renderText({propertyPriceInfo(prop_v, 'buy')})
    #utput$results_detail <- renderText({HTML(paste(output_results))})
    
    print("UPDATE")
    print(pred_df_two)
  }
  
  
  
  
  
  
  
  # ------ GGPLOT - line chart section  ----------
  
  #Caption over line chart showing current selections
  output$caption <- renderText({"Model Outputs"})
  output$results <- renderText({"Summary of Values"})
 
  output$holder <- renderText({"Some text goes here"})

  
  output$hist <-  renderPlot({
   
    # Plot the selected data
    g<- ggplot(locations )+ geom_histogram(aes(price), color = "blue", alpha = 0.8, bins =100) 
   
  
    
    # Labels and Titles
    g <- g + labs(title = "Histogram of Price")
    
    # Scales ... ensure smoothers still show to full extent 
    # by using the coord_cartesion function (zooms plot)
   
     g <- g + theme(axis.text.x=element_text(angle=90),
                    axis.text=element_text(size=12),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=14, face="bold"),
                    strip.text = element_text(size = 12, face="bold", colour = "white"))
    

    # Suppress the warning (in console only!) produced for polynomial fits 
    # where there are just three data points.
    suppressWarnings(print(g))
  })  
  
  
  
  
  
  
})
