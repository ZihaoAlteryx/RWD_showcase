# Packages used
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(lubridate)
library(tmap)
library(ggmap)
library(purrr)
library(highcharter)# ploting package
library(DT) # data table pakcage for dashboard
library(plotly)

# When testing locally, I set my local working directory
#setwd('C:/Users/Zihao/Google Drive/USC MSBA Course Materials/GSBA 542 - Communication for Management/GSBA542 Communication/Project 2/shiny/dso545_dashboard')
load('data.RData')

#  Find the lowest and highest dates to put into the date filters
calls$CREATEDDATE <- as.Date(as.character(calls[1:nrow(calls), 'CREATEDDATE']), format='%m/%d/%Y')
ld <- min(calls$CREATEDDATE) #lowest create date
hd <- max(calls$CREATEDDATE) #highest create date

# Define the user interface
ui <- shinyUI(dashboardPage(
  dashboardHeader(disable = TRUE), 
  dashboardSidebar(
    # Links that will show up on the sidebar
    sidebarMenu(
      menuItem("Executive Dashboard", tabName = 'executive', icon = icon('dashboard')),
      menuItem("Crimes", tabName = 'crimes', icon = icon('ambulance')),
      menuItem("Migrations and Trends", tabName = 'migration', icon = icon('car')),
      menuItem("Resources", tabName = 'resources', icon = icon('hospital-o')),
      menuItem("El Pueblo", tabName = 'elpueblo')
    ),
    # User inputs in the side bar
    dateRangeInput("date", label = "Date Range: ", start = ld, end = hd, format = 'mm-dd-yy'),
    column(1, align = 'right', actionButton("refresh", " Refresh Charts", icon = icon('refresh')))),
  # CSS to adjust apperance of site
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") # Custom CSS file in www\custom.css
    ),
    # In the executive tab, these are the widgets/charts that go inside
    tabItems(
      tabItem(tabName = "executive",
              fluidRow(
                valueBoxOutput('totalunsheltered_valuebox', width = 6),
                valueBoxOutput('mostvulnerable_valuebox', width = 6)
              ),
              fluidRow(
                box(status = 'primary', solidHeader = FALSE, width = 2, title = 'Homeless Weight', sliderInput('homeless_weight', 'Homeless Weight', 0, 100, 100, step = 1)),
                box(status = 'primary', solidHeader = FALSE, width = 2, title = 'Crime Weight', sliderInput('crime_weight', 'Crime Weight', 0, 100, 100, step = 1)),
                box(status = 'primary', solidHeader = FALSE, width = 2, title = 'Call Weight', sliderInput('call_weight', 'Call Weight', 0, 100, 100, step = 1)),
                box(status = 'primary', solidHeader = FALSE, width = 2, title = 'Shelter Weight', sliderInput('shelter_weight', 'Shelter Weight', 0, 100, 100, step = 1)),
                box(status = 'primary', solidHeader = FALSE, width = 2, title = 'Delta Weight', sliderInput('delta_weight', 'Delta Weight', 0, 100, 100, step = 1)),
                box(status = 'primary', solidHeader = FALSE, width = 2, title = 'Percent Weight', sliderInput('percent_weight', 'Percent Weight', 0, 100, 100, step = 1))
              ),
              fluidRow(
                box (status = 'primary', solidHeader = FALSE, width = 4, title = 'Vulnerability Plot', plotlyOutput('mostvulnerable_plt')), #output
                box (status = 'primary', solidHeader = FALSE, width = 8, title = 'Vulnerability Map', leafletOutput('vulnerability_map'))
              ),
              fluidRow(
                box(status = 'primary', solidHeader = FALSE, width = 12, title = 'Vulnerability Table', DT::dataTableOutput('vulnerability_table'))
              )
      ),
      tabItem(tabName = 'crimes',
              fluidRow(
                valueBoxOutput('totalcrime_valuebox', width = 3),
                valueBoxOutput('violentcrime_valuebox', width = 3),
                valueBoxOutput('theftcrime_valuebox', width = 3),
                valueBoxOutput('othercrime_valuebox', width = 3)
              ),
              fluidRow(
                box (status = 'primary', solidHeader = FALSE, width = 12, title = 'Crime Map',  leafletOutput('crime_map'), collapsible = TRUE)
              ),
              fluidRow(
                box (status = 'primary', solidHeader = FALSE, width = 12, title = 'Crime Time Drill Down', highchartOutput('crime_drill'), collapsible = TRUE)
              ),
              fluidRow(
                box (status = 'primary', solidHeader = FALSE, width = 12, title = 'Crime Table',  DT::dataTableOutput('crime_table'), collapsible = TRUE)
              )
      ),
      tabItem(tabName = 'migration',
              fluidRow(
                valueBoxOutput('biggest_increase_valuebox', width = 6),
                valueBoxOutput('biggest_discrepancy_valuebox', width = 6)
              ),
              fluidRow(
                box (status = 'primary', solidHeader = FALSE, width = 12, title = 'Migration Map', leafletOutput('migration_map'), collapsible = TRUE)
              ),
              fluidRow(
                box (status = 'primary', solidHeader = FALSE, width = 12, title = 'PIT and Calls Discrepancy Map',  leafletOutput('pit_difference_map'), collapsible = TRUE)
              )
      ),
      tabItem(tabName = 'resources',
              fluidRow(
                valueBoxOutput('shelters_valuebox', width = 6),
                valueBoxOutput('substanceabuse_valuebox', width = 6)
              ),
              fluidRow(
                box(status = 'primary', solidHeader = FALSE, width = 12, title = 'Resource Map', leafletOutput('resource_map'))
              )
      ),
      tabItem(tabName ='elpueblo')
    )
  )
)
)

# Define server logic
server <- function(input, output) {
   
   output$totalunsheltered_valuebox <- renderValueBox({
     total <- sum(CT$TOT_2017)
     valueBox(paste0(prettyNum(total, big.mark = ","), ' people'), 'Total Unsheltered', icon = icon('check'),  color = 'red')
   })
  
   output$mostvulnerable_valuebox <- renderValueBox({
     total <- vulnerability_df()[1, 'CENSUS_TRACT']
     valueBox(paste0(total, ' Census Tract'), 'Most Vulnerable', icon = icon('check'), color = 'orange')
   })
   
   output$shelters_valuebox <- renderValueBox({
     total <- nrow(shelters_df())
     valueBox(paste0(total, ' shelters'), 'Total Shelters', icon = icon('check'),  color = 'red')
   })
   
   output$substanceabuse_valuebox <- renderValueBox({
     total <- nrow(substance_abuse)
     valueBox(paste0(total, ' Substance Abuse Programs'), 'Substance Abuse', icon = icon('check'), color = 'orange')
   })
   
   
   output$biggest_increase_valuebox <- renderValueBox({
     full <- full_df() %>%
       arrange(-delta)
     total <- full[1, 'delta'] * 100
     valueBox(paste0(round(total, 2), '% Increase'), paste0('Census Tract: ', full[1, 'CENSUS_TRACT'], 
                                                            ' / 2017 Count: ', full[1, 'TOT_2017'],
                                                            ' / 2016 Count: ', full[1, 'TOT_2016']), icon = icon('check'),  color = 'red')
    })
   
   output$biggest_discrepancy_valuebox <- renderValueBox({
     full <- full_df() %>%
       arrange(-percent_difference)
     total <- full[1, 'percent_difference'] * 100
     valueBox(paste0(round(total, 2), '% Difference'), paste0('Census Tract: ', full[1, 'CENSUS_TRACT'], 
                                                              ' / Pit Count: ', full[1, 'TOT_2017'],
                                                              ' / Call Count: ', full[1, 'calls_count']), icon = icon('check'),  color = 'blue')
   })
   
   output$totalcrime_valuebox <- renderValueBox({
     total <- sum(full_df()$totalcrime_count)
     valueBox(paste0(prettyNum(total, big.mark = ",")), 'Total Crimes', icon = icon('check'),  color = 'red')
   })
   
   output$violentcrime_valuebox <- renderValueBox({
     total <- sum(full_df()$violentcrime_count)
     valueBox(paste0(prettyNum(total, big.mark = ",")), 'Violent Crimes', icon = icon('check'), color = 'orange')
   })
   
   output$theftcrime_valuebox <- renderValueBox({
     total <- sum(full_df()$theftcrime_count)
     valueBox(paste0(prettyNum(total, big.mark = ",")), 'Theft Crimes', icon = icon('check'), color = 'blue')
   })
   
   output$othercrime_valuebox <- renderValueBox({
     total <- sum(full_df()$othercrime_count)
     valueBox(paste0(prettyNum(total, big.mark = ",")), 'Other Crimes', icon = icon('check'), color = 'purple')
   })
  
   output$mostvulnerable_plt <- renderPlotly({
     df <- vulnerability_df()[1:15, ]
     x <- list(title = '', categoryorder ='array', categoryarray = rev(df$CENSUS_TRACT))
     plot_ly(df, x = ~percent_score, y = ~CENSUS_TRACT, orientation = 'h', type = 'bar',
             marker = list(color = '#191970	', hoverinfo = 'none'), name = 'Percent Score') %>%
       add_trace(x = ~homeless_score, name = 'Homeless Score', marker = list(color = '#0000CD')) %>%
       add_trace(x = ~crime_score, name = 'Crime Score', marker = list(color = '#4169E1	')) %>%
       add_trace(x = ~call_score, name = 'Call Score', marker = list(color = '#6A5ACD')) %>%
       add_trace(x = ~shelter_score, name = 'Shelter Score', marker = list(color = '	#4682B4')) %>%
       add_trace(x = ~delta_score, name = 'Delta Score', marker = list(color = '#1E90FF')) %>%
       layout(yaxis = x, xaxis = list(title = 'Vulnerability Score'), showlegend = FALSE, barmode = 'stack')
   })
   
   output$vulnerability_table <- renderDataTable({
     df <- vulnerability_df()
     colnames(df) <- c('Census Tract', 'Homeless Score', 'Crime Score', 'Call Score', 'Shelter Score', 'Delta Score', 'Percent Score', 'Vulnerability')
     DT::datatable(df, 
                   options = list(lengthMenu = c(10, 25, 50),
                                  pageLength = 10,
                                  orderClasses = TRUE,
                                  fillContainer = TRUE, 
                                  info = FALSE),
                   rownames = FALSE,
                   selection = 'single'
     ) %>%
       formatCurrency(c(2, 3, 4, 5, 6, 7, 8), '', digits = 3)
   })
   
   output$crime_table <- renderDataTable({
     df <- full_df() %>%
       mutate(TOT_2017 = round(TOT_2017, 0),
              crime_density = round(crime_density, 3)) %>%
       select(CENSUS_TRACT, TOT_2017, violentcrime_count, theftcrime_count, othercrime_count, totalcrime_count, population, crime_density) %>%
       arrange(-crime_density)
     colnames(df) <- c('Census Tract', 'PIT Count', 'Violent Crimes', 'Theft Crimes', 'Other Crimes', 'Total Crimes', 'Population', 'Crime Density')
     DT::datatable(df, 
                   options = list(lengthMenu = c(10, 25, 50),
                                  pageLength = 10,
                                  orderClasses = TRUE,
                                  fillContainer = TRUE, 
                                  info = FALSE),
                   rownames = FALSE,
                   selection = 'single'
     )
   })
   
   output$crime_drill <- renderHighchart({
     crimes_hour <- crimes_df() %>%
       group_by(HOUR) %>%
       summarise(y = n()) %>%
       mutate(name = as.numeric(HOUR)) %>%
       arrange(name) %>%
       mutate(drilldown = as.character(HOUR),
              name = as.character(name)) %>%
       select(name, y, drilldown)
     
     hc <- highchart() %>%
       hc_chart(type = "column") %>%
       hc_title(text = "") %>%
       hc_xAxis(type = "category") %>%
       hc_legend(enabled = FALSE) %>%
       hc_plotOptions(
         series = list(
           boderWidth = 0,
           dataLabels = list(enabled = FALSE)
         )
       ) %>%
       hc_add_series(
         name = "Hour",
         colorByPoint = TRUE,
         data = crimes_hour
       )
     
     for (i in 1:23) {
       tmp <- crimes_df() %>%
         mutate(hour = as.numeric(HOUR)) %>%
         filter(hour == i) %>%
         group_by(Type) %>%
         summarise(count = n()) %>%
         mutate(name = as.character(Type),
                value = count) %>%
         select(name, value)
       assign(paste0('crime_', i), tmp)
     }
     
     hc <- hc %>%
       hc_drilldown(
         allowPointDrilldown = TRUE,
         series = list(
           list(id = "1", data = list.parse2(crime_1)),
           list(id = "2", data = list.parse2(crime_2)),
           list(id = "3", data = list.parse2(crime_3)),
           list(id = "4", data = list.parse2(crime_4)),
           list(id = "5", data = list.parse2(crime_5)),
           list(id = "6", data = list.parse2(crime_6)),
           list(id = "7", data = list.parse2(crime_7)),
           list(id = "8", data = list.parse2(crime_8)),
           list(id = "9", data = list.parse2(crime_9)),
           list(id = "10", data = list.parse2(crime_10)),
           list(id = "11", data = list.parse2(crime_11)),
           list(id = "12", data = list.parse2(crime_12)),
           list(id = "13", data = list.parse2(crime_13)),
           list(id = "14", data = list.parse2(crime_14)),
           list(id = "15", data = list.parse2(crime_15)),
           list(id = "16", data = list.parse2(crime_16)),
           list(id = "17", data = list.parse2(crime_17)),
           list(id = "18", data = list.parse2(crime_18)),
           list(id = "19", data = list.parse2(crime_19)),
           list(id = "20", data = list.parse2(crime_20)),
           list(id = "21", data = list.parse2(crime_21)),
           list(id = "22", data = list.parse2(crime_22)),
           list(id = "23", data = list.parse2(crime_23))
         )
       )
     
     hc
   })
  
   output$resource_map <- renderLeaflet({
     full <- full_df()
     full <- full %>%
       arrange(-TOT_2017)
     shelters <- shelters_df()
     
     for (i in 1:nrow(full)) {
       if (i == 1) {
         full[i, 'rank'] <- 1
       } else {
         if (full[i, 'TOT_2017'] == full[i-1, 'TOT_2017']) {
           full[i, 'rank'] <- full[i-1, 'rank']
         } else {
           full[i, 'rank'] <- full[i-1, 'rank'] + 1
         }
       }
     }
     
     geo_map <- geo[geo@data$COUNTYFP == '037',]
     
     map <- append_data(geo_map, full, key.shp = 'TRACTCE', key.data = 'CENSUS_TRACT', ignore.na = TRUE)
     
     map <- map[which(!is.na(map$rank)), ]
     
     resource_cp <- colorNumeric(palette = c('#00008B', '#B0E0E6'), domain=map$rank)

     
     
     resource_popup <- paste0('<b>Total Unsheltered:</b> ', map$TOT_2017, '<br>',
                           '<b>Section ID:</b> ', map$TRACTCE)
     
     shelters_popup <- paste0("<b><a href='",shelters$URL, "'>", shelters$NAME, "</a></b>")
     
     substance_popup <- paste0("<b><a href='",substance_abuse$link, "'>", substance_abuse$Name, "</a></b>")
     
     shelterIcon <- makeIcon(
       iconUrl = "http://www.bethohr.com/hp_wordpress/wp-content/uploads/2016/04/sleeping-shelter-icon-1105173459.png",
       iconWidth = 20, iconHeight = 20,
       iconAnchorX = 0, iconAnchorY = 0
     )
     
     substanceIcon <- awesomeIcons(
       icon = 'fa-plus-square'
     )
     
     leaflet(map) %>%
       addProviderTiles("CartoDB.Positron") %>%
       addPolygons(stroke=FALSE, 
                   smoothFactor = 0.2, 
                   fillOpacity = .8,
                   color= ~resource_cp(map$rank),
                   popup = resource_popup) %>%
       addMarkers(data = shelters,
                  clusterOptions = markerClusterOptions(),
                  icon = shelterIcon,
                  popup = shelters_popup,
                  group = 'Shelters') %>%
       addMarkers(data = substance_abuse,
                  clusterOptions = markerClusterOptions(),
                  icon = substanceIcon,
                  popup = substance_popup,
                  group = 'Substance Abuse') %>%
       addLayersControl(
         overlayGroups = c('Shelters', 'Substance Abuse'),
         options = layersControlOptions(collapsed = FALSE)
       ) %>%
       setView(lng = -118.3571,
               lat = 34.09116,
               zoom = 10)

   })
   
   
   
   output$migration_map <- renderLeaflet({
     full_map_decrease <- full_map()[which(full_map()$delta < 0), ]
     full_map_increase <- full_map()[which(full_map()$delta > 0), ]
     decrease_cp <- colorNumeric(palette = c('#00008B','#F0F8FF'), domain=full_map()$delta_rank)
     increase_cp <- colorNumeric(palette = c('#FFA07A', '#DC143C'), domain=full_map()$delta_rank)
     
     increase_popup <- paste0('<b>Percent Change from 2016 to 2017: </b>', round(full_map_increase$delta * 100, 2), '%<br>',
                              '<b>2015 Count: </b>', full_map_increase$TOT_2015, '<br>',
                              '<b>2016 Count: </b>', full_map_increase$TOT_2016, '<br>',
                              '<b>2017 Count: </b>', full_map_increase$TOT_2017)
     decrease_popup <- paste0('<b>Percent Change from 2016 to 2017: </b>', round(full_map_decrease$delta * 100, 2), '%<br>',
                              '<b>2015 Count: </b>', full_map_decrease$TOT_2015, '<br>',
                              '<b>2016 Count: </b>', full_map_decrease$TOT_2016, '<br>',
                              '<b>2017 Count: </b>', full_map_decrease$TOT_2017) 
     
     leaflet(full_map) %>%
       addProviderTiles("CartoDB.Positron") %>%
       addPolygons(data = full_map_decrease,
                   stroke=FALSE, 
                   smoothFactor = 0.2, 
                   fillOpacity = .8,
                   color= ~decrease_cp(full_map_decrease$delta_rank),
                   popup = decrease_popup) %>%
       addPolygons(data = full_map_increase,
                   stroke=FALSE, 
                   smoothFactor = 0.2, 
                   fillOpacity = .8,
                   color= ~increase_cp(full_map_increase$delta_rank),
                   popup = increase_popup) %>%
       setView(lng = -118.3571,
               lat = 34.09116,
               zoom = 10)
       
   })
   
   output$pit_difference_map <- renderLeaflet({
     pit_difference_increase <- full_map()[which(full_map()$percent_difference > 0), ]
     pit_difference_cp <- colorNumeric(palette = c('#FA8072', '#FF0000'), domain=pit_difference_increase$percent_difference_rank)
     
     pit_difference_popup <- paste0('<b>Percent Difference: </b>', round(pit_difference_increase$percent_difference * 100, 2), '%<br>',
                              '<b>2017 Count: </b>', pit_difference_increase$TOT_2017, '<br>',
                              '<b>Call Count: </b>', pit_difference_increase$calls_count)
     leaflet(pit_difference_increase) %>%
       addProviderTiles("CartoDB.Positron") %>%
       addPolygons(stroke=FALSE, 
                   smoothFactor = 0.2, 
                   fillOpacity = .8,
                   color= ~pit_difference_cp(pit_difference_increase$percent_difference_rank),
                   popup = pit_difference_popup) %>%
       setView(lng = -118.3571,
               lat = 34.09116,
               zoom = 10)
   })
   
   output$crime_map <- renderLeaflet ({
     
     full_map <- full_map()
     crime_cp <- colorNumeric(palette = c('#00008B', '#B0E0E6'), domain=full_map$crime_rank)
     
     leaflet(full_map) %>%
       addProviderTiles("CartoDB.Positron") %>%
       addPolygons(stroke=FALSE, 
                   smoothFactor = 0.2, 
                   fillOpacity = .8,
                   color= ~crime_cp(full_map$crime_rank))  %>%
       addCircleMarkers(data = violent_df(),
                        radius = 7,
                        color = 'red',
                        fillOpacity = 0.5,
                        stroke = FALSE,
                        group = 'Violent Crimes') %>%
       addCircleMarkers(data = theft_df(),
                        radius = 7,
                        color = 'blue',
                        fillOpacity = 0.5,
                        stroke = FALSE,
                        group = 'Theft Crimes') %>%
       addCircleMarkers(data = other_df(),
                        radius = 7,
                        color = 'yellow',
                        fillOpacity = 0.5,
                        stroke = FALSE,
                        group = 'Other Crimes') %>%
       addLayersControl(
         overlayGroups = c('Violent Crimes', 'Theft Crimes', 'Other Crimes'),
         options = layersControlOptions(collapsed = FALSE)
       ) %>%
       setView(lng = -118.3571,
               lat = 34.09116,
               zoom = 10)
     
   })
   
   output$vulnerability_map <- renderLeaflet({
     
     geo_map <- geo[geo@data$COUNTYFP == '037',]
     vulnerability <- vulnerability_df()
     vulnerability <- vulnerability[which(!is.na(vulnerability$vulnerability)), ]
     for (i in 1:nrow(vulnerability)) {
       if (i == 1) {
         vulnerability[i, 'rank'] <- 1
       } else {
         if (vulnerability[i, 'vulnerability'] == vulnerability[i-1, 'vulnerability']) {
           vulnerability[i, 'rank'] <- vulnerability[i-1, 'rank']
         } else {
           vulnerability[i, 'rank'] <- vulnerability[i-1, 'rank'] + 1
         }
       }
     }
     
     vulnerability_map <- append_data(geo_map, vulnerability, key.shp = 'TRACTCE', key.data = 'CENSUS_TRACT', ignore.na = TRUE)
     
     vulnerability_map <- vulnerability_map[which(!is.na(vulnerability_map$rank)), ]
     
     vulnerability_cp <- colorNumeric(palette = c('#8B0000', '#FFA07A'), domain = vulnerability_map$rank)
     vulnerability_popup <- paste0('<b>', vulnerability_map$CENSUS_TRACT, '</b>',
                                   '<b>Vulnerability Score: </b>', round(as.numeric(vulnerability_map$vulnerability), 3), '<br>',
                                   '<b>Homeless Score: </b>', round(as.numeric(vulnerability_map$homeless_score), 3), '<br>',
                                   '<b>Call Score: </b>', round(as.numeric(vulnerability_map$call_score), 3), '<br>',
                                   '<b>Crime Score: </b>', round(as.numeric(vulnerability_map$crime_score), 3), '<br>',
                                   '<b>Shelter Score: </b>', round(as.numeric(vulnerability_map$shelter_score), 3), '<br>',
                                   '<b>Delta Score: </b>', round(as.numeric(vulnerability_map$delta_score), 3), '<br>',
                                   '<b>Percent Score: </b>', round(as.numeric(vulnerability_map$percent_score), 3))

     
     leaflet(vulnerability_map) %>%
       addProviderTiles("CartoDB.Positron") %>%
       addPolygons(stroke=FALSE, 
                   smoothFactor = 0.2, 
                   fillOpacity = .8,
                   color= ~vulnerability_cp(vulnerability_map$rank),
                   popup = vulnerability_popup) %>%
       setView(lng = -118.3571,
               lat = 34.09116,
               zoom = 10)
   })
   
   vulnerability_df <- reactive ({
     vulnerability <- full_df() %>%
       select(CENSUS_TRACT, homeless_density_normalized, crime_density_normalized, call_density_normalized, shelter_density_normalized,
              delta_normalized, percent_difference_normalized)
     
     homeless_weight <- as.numeric(input$homeless_weight)/100
     crime_weight <- as.numeric(input$crime_weight)/100
     call_weight <- as.numeric(input$call_weight)/100
     shelter_weight <- as.numeric(input$shelter_weight)/100
     delta_weight <- as.numeric(input$delta_weight)/100
     percent_weight <- as.numeric(input$percent_weight)/100

     vulnerability %>%
       mutate(homeless_score = homeless_weight * homeless_density_normalized, 
              crime_score = crime_weight * crime_density_normalized, 
              call_score = call_weight * call_density_normalized, 
              shelter_score = shelter_weight * shelter_density_normalized,
              delta_score = delta_weight *delta_normalized, 
              percent_score = percent_difference_normalized) %>%
       mutate(vulnerability = homeless_score + crime_score + call_score + shelter_score + delta_score + percent_score) %>%
       select(CENSUS_TRACT, homeless_score, crime_score, call_score, shelter_score, delta_score, percent_score, vulnerability) %>%
       arrange(-vulnerability)
   })
   
   crimes_df <- reactive({
     crimes
   })
   
   calls_df <- reactive({
     calls
   })
   
   shelters_df <- reactive({
     shelters
   })
   
   violent_df <- reactive({
     crimes_df() %>%
       filter(Type == 'Violent') %>%
       select('LATITUDE', 'LONGITUDE')
   })
   
   theft_df <- reactive({
     crimes_df() %>%
       filter(Type == 'Theft') %>%
       select('LATITUDE', 'LONGITUDE')
   })
   
   other_df <- reactive({
     crimes_df() %>%
       filter(Type == 'Other') %>%
       select('LATITUDE', 'LONGITUDE')
   })
   
   full_df <- reactive({
     geo_calls <- calls_df()
     geo_crimes <- crimes_df()
     geo_shelters <- shelters_df()

     geo_calls$CENSUS_TRACT <- substr(geo_calls$api_block_fips, 6, 11) 
     geo_crimes$CENSUS_TRACT <- substr(geo_crimes$api_block_fips, 6, 11) 
     geo_shelters$CENSUS_TRACT <- substr(geo_shelters$api_block_fips, 6, 11) 
     
     geo_calls <- geo_calls %>%
       group_by(CENSUS_TRACT) %>%
       summarise(calls_count = n())
     
     geo_violent <- geo_crimes %>%
       filter(Type == 'Violent') %>%
       group_by(CENSUS_TRACT) %>%
       summarise(violentcrime_count = n())
     
     geo_theft <- geo_crimes %>%
       filter(Type == 'Theft') %>%
       group_by(CENSUS_TRACT) %>%
       summarise(theftcrime_count = n())
     
     geo_other <- geo_crimes %>%
       filter(Type == 'Other') %>%
       group_by(CENSUS_TRACT) %>%
       summarise(othercrime_count = n())
     
     geo_total <- geo_crimes %>%
       group_by(CENSUS_TRACT) %>%
       summarise(totalcrime_count = n())
     
     geo_shelters <- geo_shelters %>%
       group_by(CENSUS_TRACT) %>%
       summarise(shelter_count = n())
     
     full <- merge(x = CT, y = geo_calls, by = "CENSUS_TRACT", all = TRUE)
     full <- merge(x = full, y = geo_violent, by = "CENSUS_TRACT", all = TRUE)
     full <- merge(x = full, y = geo_theft, by = "CENSUS_TRACT", all = TRUE)
     full <- merge(x = full, y = geo_other, by = "CENSUS_TRACT", all = TRUE)
     full <- merge(x = full, y = geo_total, by = "CENSUS_TRACT", all = TRUE)
     full <- merge(x = full, y = geo_shelters, by = "CENSUS_TRACT", all = TRUE)
     full <- merge(x = full, y = tot, by = "CENSUS_TRACT", all = TRUE)
     full <- full[!with(full,(TOT_2017 == 0 | is.na(TOT_2017)) & is.na(totalcrime_count) & is.na(calls_count) & is.na(shelter_count)),]
     
     
     full[c('TOT_2015', 'TOT_2016', 'TOT_2017', 'calls_count', 'violentcrime_count', 'theftcrime_count', 'othercrime_count', 'totalcrime_count', 'shelter_count')][is.na(full[c('TOT_2015', 'TOT_2016', 'TOT_2017', 'calls_count', 'violentcrime_count', 'theftcrime_count', 'othercrime_count', 'totalcrime_count', 'shelter_count')])] <- 0
     
     full <- full %>%
       mutate(population = as.numeric(as.character(population))) %>%  
       mutate(homelesscrime_density = ifelse(TOT_2017 == 0, 0, totalcrime_count/TOT_2017),
              crime_density = ifelse(population == 0 | is.na(population), totalcrime_count, totalcrime_count/population),
              homeless_density = ifelse(population == 0 | is.na(population), TOT_2017, as.numeric(TOT_2017)/population),
              shelter_density = ifelse(shelter_count == 0, 0, TOT_2017/shelter_count),
              call_per_person = ifelse(population == 0 | is.na(population), 0, calls_count/population),
              delta = ifelse(TOT_2016 == 0, TOT_2017, (TOT_2017 - TOT_2016)/TOT_2016),
              percent_difference = ifelse(TOT_2017 == 0, calls_count, (calls_count - TOT_2017)/TOT_2017))
     
     full <- full[which(!is.na(full$CENSUS_TRACT)), ]
     
     full <- full %>%
       mutate(crime_density_normalized = (crime_density - min(crime_density, na.rm = TRUE)) / (max(crime_density, na.rm = TRUE) - min(crime_density, na.rm = TRUE)),
              homeless_density_normalized = (homeless_density - min(homeless_density, na.rm = TRUE)) / (max(homeless_density, na.rm = TRUE) - min(homeless_density, na.rm = TRUE)),
              shelter_density_normalized = (shelter_density - min(shelter_density, na.rm = TRUE)) / (max(shelter_density, na.rm = TRUE) - min(shelter_density, na.rm = TRUE)),
              call_density_normalized = (call_per_person - min(call_per_person, na.rm = TRUE)) / (max(call_per_person, na.rm = TRUE) - min(call_per_person, na.rm = TRUE)),
              delta_normalized = (delta - min(delta, na.rm = TRUE)) / (max(delta, na.rm = TRUE) - min(delta, na.rm = TRUE)),
              percent_difference_normalized = (percent_difference - min(percent_difference, na.rm = TRUE)) / (max(percent_difference, na.rm = TRUE) - min(percent_difference, na.rm = TRUE))) %>%
       arrange(-crime_density)

     
     for (i in 1:nrow(full)) {
       if (i == 1) {
         full[i, 'crime_rank'] <- 1
       } else {
         if (full[i, 'crime_density'] == full[i-1, 'crime_density']) {
           full[i, 'crime_rank'] <- full[i-1, 'crime_rank']
         } else {
           full[i, 'crime_rank'] <- full[i-1, 'crime_rank'] + 1
         }
       }
     }
     full <- full %>%
       arrange(delta)
     
     for (i in 1:nrow(full)) {
       if (i == 1) {
         full[i, 'delta_rank'] <- 1
       } else {
         if (full[i, 'delta'] == full[i-1, 'delta']) {
           full[i, 'delta_rank'] <- full[i-1, 'delta_rank']
         } else {
           full[i, 'delta_rank'] <- full[i-1, 'delta_rank'] + 1
         }
       }
     }
     
     full <- full %>%
       arrange(percent_difference)
     
     for (i in 1:nrow(full)) {
       if (i == 1) {
         full[i, 'percent_difference_rank'] <- 1
       } else {
         if (full[i, 'percent_difference'] == full[i-1, 'percent_difference']) {
           full[i, 'percent_difference_rank'] <- full[i-1, 'percent_difference_rank']
         } else {
           full[i, 'percent_difference_rank'] <- full[i-1, 'percent_difference_rank'] + 1
         }
       }
     }
     
     full
   })
   
   
   full_map <- reactive({
     geo_map <- geo[geo@data$COUNTYFP == '037',]
     full_map <- append_data(geo_map, full_df(), key.shp = 'TRACTCE', key.data = 'CENSUS_TRACT', ignore.na = TRUE)
     full_map <- full_map[which(!is.na(full_map$calls_count)), ]
     full_map
   })
   
}
# Run the application 
shinyApp(ui = ui, server = server)
