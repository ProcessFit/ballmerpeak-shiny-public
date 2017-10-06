library(shiny)
library(leaflet)


# ---------- Define Attribute Fields -----------------------------------------------
# tweaks, a list object to set up multicols for checkboxGroupInput
# https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny



checks <- c("shampoo","self_checkin","hair_dryer","iron","hangers","lockbox",
            "24_hour_checkin","doorman_entry","wheelchair_accessible","laptop_friendly_workspace",
            "family_kid_friendly", "buzzerwireless_intercom","essentials","breakfast",
            "pool","internet","free_parking_on_premises")


check_labels <- c("Shampoo","Self Check-In","Hair Dryer","Iron","Clothes Hangers","Lockbox",
            "24 hour Check-In","Doorman Entry","Wheelchair Accessible","Laptop friendly workspace",
            "Family/kid Friendly", "Buzzer / Wireless Intercom","Essentials","Breakfast",
            "Pool","Internet","Free Parking on Premises")

controls <-
  list(h4("Select property features:"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'attributes', 
                                   label    = "Check all that apply:", 
                                   choices  = check_labels,
                                   selected = checks,
                                   inline   = FALSE))) 




## -----------------------------------------------------------------------------
##  Define UI for Coral Data Display
## -----------------------------------------------------------------------------

shinyUI(fluidPage(theme = "bootstrap.css",includeCSS("css/style.css"),
                  
                  # Application title
                 
                  fluidRow(column(12,tags$hr())),
                  
                  # Define layout:  three columns (rather than the sidebar + main)
                  fluidRow(
               
                    # Empty  column - used as a spacer
                    
                    column(4,tags$div(class="panel panel-primary",
                                      # Column 1: 2 units wide. For selectors
                                      # Selector lists are populated from the data defined in the Server file.
                                      tags$div(class="panel-heading",
                                               tags$h3(class="panel-title","How will my House/Unit perform as an AirBnB?")),
                                      tags$div(class = "well", 
                                               
                                             
                                               
                      #--------------------Letian import
                     
                      fluidRow(column(6,selectInput("property_type", label = h4("Property Type"), 
                                           choices = list(
                                             "Apartment" = 1, 
                                             "House" = 3,
                                             "Townhouse" = 4,
                                             "Bed & Breakfast" =2
                                           ), selected = 1),
                             sliderInput('bedrooms', label = 'Bedrooms', min = 1, max = 5, value = 2, step = 1),
                             sliderInput('beds', label = 'Number of beds', min = 1, max = 12, value = 2, step = 1),
                             sliderInput('bathrooms', label = 'Bathrooms', min = 1, max = 5, value = 2, step = 0.5),
                             sliderInput('accommodates', label = 'Max. No. Guests', min = 1, max = 10, value = 2, step = 1)),
                      column(6,
                             selectInput("room_type", label = h4("Room Type"), 
                                         choices = list(
                                           "Private Room" = 6,
                                           "Entire home/apt" = 5,
                                           "Shared Room" = 7
                                         ), selected = 1),
                             sliderInput('week_discount', label = 'Weekly discount', 
                                           min = 0, max = 30, value = 0, step = 2.5, post = "%"),
                              sliderInput('month_discount', label = 'Monthly discount', 
                                         min = 0, max = 30, value = 0, step = 2.5, post = "%"),
                             sliderInput('guests_included', label = 'No. Guests Included', min = 1, max =10,  value = 0, step = 1),
                             sliderInput('extra_people', label = '$ per Extra Person', pre = "$", min = 1, max = 10, value = 2, step = 1))),
                        controls,
                        textOutput("property_lat_lon")
                                      )
                      
                      
                      #--------------------------------
  
                                               
                                      )), 
                    
                    column(4,tags$div(class="panel panel-primary",
                                      # Columns 2: 5 units wide. Displays map plot
                                      tags$div(class="panel-heading",
                                               tags$h3(class="panel-title","Location of AirBnB Sites")),
                                      leafletOutput("mapPlot", height = 600),
                                      absolutePanel(top=20, left=70,div(style = "display:inline-block",
                                                    textInput("address", "" , "Search for Address: ")),
                                      div(style = "display:inline-block",
                                                        
                                         actionButton("go", "GO")))
                    
                           
                           )
                           
                    ),
                    fluidRow(
                      column(4,
                             tags$div(class="panel panel-primary",
                                               # Column 3: 3 units wide. Displays x-y plot.
                                               tags$div(class="panel-heading",
                                                        tags$h3(class="panel-title",
                                                                textOutput("caption"))),
                                               plotOutput("hist", height = 300))
                                               )
                             
                             
                             ,
                      column(4,tags$div(class="panel panel-primary",height = 300,
                                        tags$div(class="panel-heading",
                                        tags$h3(class="panel-title",textOutput("results"))),
                                        tags$div(class = "well", 
                                        tags$h2(textOutput("results_detail0")),
                                        tags$h2(textOutput("results_detail1")),
                                        tags$h2(textOutput("results_detail2")),
                                        tags$h2(textOutput("results_detail3")),
                                        tags$h3(textOutput("results_detail4")),
                                        tags$h3(textOutput("results_detail5")))))
                   
                    
                   
                           
                    )
                  )
                  
))