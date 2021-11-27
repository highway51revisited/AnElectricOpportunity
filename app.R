## Electric Oppurtunity Web App

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(waffle)) install.packages("waffle", repos = "http://cran.us.r-project.org")


# set mapping colour for each data
chrg_col = "#cc4c02"
allVh_col = "#662506"
Chevy_col = "#045a8d"


# import data
cv_ev_agg=read.csv("input_data/map_data.csv")
cv_ev_agg <- within(cv_ev_agg , Chevrolet <- round(VehiclesChevrolet_pct*100))
cv_ev_agg <- within(cv_ev_agg , 'Non-Chevrolet' <- 100-Chevrolet)
cv_ev_prdct<-read.csv("input_data/prediction_data.csv")
cv_chrg_prdct<-read.csv("input_data/Charger_forecast.csv")
cv_ev_data_out <- cv_ev_agg %>% select(State, County, Year, Total_Chargers, Vehicles, VehiclesChevrolet, VehiclesChevrolet_pct)%>%rename(Total_Vehicles=Vehicles, Chevrolet_Vehicles=VehiclesChevrolet,Chevrolet_MarketShare=VehiclesChevrolet_pct)







# create plotting parameters for map
bins = c(0,10,50,100,500,1000,Inf)
cv_pal <- colorBin("Oranges", domain = cv_ev_agg$Total_Chargers, bins = bins)

mapStates = map("state", fill = TRUE, plot = FALSE)
# creat cv base map 
basemap = leaflet(cv_ev_agg) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Charging Stations", "All EV Vehicles", "Chevy EV Vehicles"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c( "All EV Vehicles", "Chevy EV Vehicles")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -98.35, lat = 39.5, zoom = 5)
  #addLegend("bottomright", pal = cv_pal, values = ~cv_ev_agg$Total_Chargers,
            #title = "<small>Deaths per million</small>") 









### SHINY UI ###
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#"><img src="icon_w.png" width="100" height="50">Electric Vehicle Dashboard</a>'), id="nav",
             windowTitle = "Electric Vehicle Dashboard",
             tabPanel("Electric Vehicle Mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        h3(textOutput("reactive_chrg_count"), align = "right"),
                                        h6("Total chargers", align = "right"),
                                        h4(textOutput("reactive_reg_count"), align = "right"),
                                        h6("All EV Vehicle Registration", align = "right"),
                                        h4(textOutput("reactive_chevy_count"), align = "right"),
                                        h6("Chevrolet EV Vehicle Registration", align = "right"),
                                        #h6(textOutput("clean_date_reactive"), align = "right"),
                                        h4(textOutput("reactive_state_count"), align = "right"),
                                        #plotOutput("epi_curve", height="130px", width="100%"),
                                        #plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select mapping year"),
                                                        choices=sort(unique(cv_ev_agg$Year)),
                                                        selected = 2020,
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE))
                                        
                                        
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://sps.northwestern.edu/masters/data-science/', tags$img(src='Northwestern_University_seal.png',height='40',width='40'))),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://twitter.com/falsafifarzad")))
                          
                          
                      )
             ),
             
             tabPanel("Chevrolet Market Share",
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h5("Select State, County, and Year to see Chevrolet's Market Share.")), style="color:#045a8d"),
                          pickerInput("state_select", "State:",   
                                      choices = unique(cv_ev_agg$State), 
                                      selected = "WA",
                                      multiple = FALSE),
                          
                          uiOutput("countySelection") ,
                          sliderTextInput("plot_date1",
                                          label = h5("Year"),
                                          choices=sort(unique(cv_ev_agg$Year)),
                                          selected = 2020,
                                          grid = FALSE,
                                          animate=animationOptions(interval = 3000, loop = FALSE))
                        ),
                        mainPanel(tags$style(type="text/css",
                                             ".shiny-output-error { visibility: hidden; }",
                                             ".shiny-output-error:before { visibility: hidden; }"
                        ),
                            plotOutput("waffle_chart"),
                            plotlyOutput("lineChart")
                        )
                      
             )),
             tabPanel("Time Series Analysis",
                      sidebarLayout(
                        sidebarPanel(
                      span(tags$i(h5("Select State and County to see the projected number of Chevrolet Vehicles and Charging stations for the next 4 years.")), style="color:#045a8d"),
                      span(tags$i(h6("The projected numbers are predicted using the Time Series Analysis")), style="color:#045a8d"),
                      
                          pickerInput("state_select_predict", "State:",   
                                      choices = unique(cv_ev_agg$State), 
                                      selected = "WA",
                                      multiple = FALSE),
                          
                          uiOutput("countySelectionPredict") 
                      
                      
                      
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Chevrolet Vehicle Count Prediction",plotlyOutput("predictLineChart")),
                 tabPanel("Charging Station Count Prediction", plotlyOutput("predictLineChartStation"))
                      ))
                  )
             ),
             tabPanel("Data",
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br()
                      ),
             tabPanel("About",
                      tags$div(
                        tags$h4("ABOUT"),
                        p("This Web Application was created by the Highway 51 Revisited team in 2021 as part of the Capstone Project for Northwestern University’s School of Professional Studies (SPS) Master of Science in Data Science (MSDS) program. For the purposes of this project, the Highway 51 Revisited team adopted the role of an independent data consulting firm contracted by Chevy Motors to analyze relationships between Electric Vehicle (EV) Charging Infrastructure and Chevy’s position in the EV marketplace."),
                        p("This Web Application uses Time Series Forecasting to project the number of registered Chevy EVs and EV charging stations on a county-by-county basis through 2025 for those US states sharing EV registration data on the following data source:"),
                        a(href="atlasevhub.com","atlasevhub.com"),
                        p("The Highway 51 Revisited team is Farzad Falsafi, Douglas Hickey, Andy Holst, Rahul Lingam, and Swapna Vaidya.")
                      ))

             
             
  )          
)





### SHINY SERVER ###

server = function(input, output, session) {

  output$countySelection = renderUI({
    pickerInput("select_county", "County:", 
                choices = as.character(cv_ev_agg[cv_ev_agg$State==input$state_select,"County"]%>%unique()), selected = "King" )
  })
  
  output$countySelectionPredict = renderUI({
    pickerInput("select_county_predict", "County:", 
                choices = as.character(cv_ev_agg[cv_ev_agg$State==input$state_select_predict,"County"]%>%unique()), selected = "King" )
  })
  
  output$yearSelection = renderUI({
    pickerInput("select_year", "Year:", 
                choices = as.character(cv_ev_agg[cv_ev_agg$State==input$state_select & cv_ev_agg$County==input$select_county,"Year"]%>%unique()))
  }) 
  
  selected_year=reactive({
    input$plot_date1
  })
  selected_state=reactive({
    input$state_select
  })
  
  selected_state_predict=reactive({
    input$state_select_predict
  })
  selected_county=reactive({
    input$select_county
  })
  
  selected_county_predict=reactive({
    input$select_county_predict
  })
  
  formatted_date = reactive({
    input$plot_date
  })
  
  reactive_df0=reactive({
    cv_ev_agg%>%filter(County==selected_county(),State==selected_state())
  })
  reactive_df1=reactive({
    cv_ev_agg%>%filter(Year==selected_year(),County==selected_county(),State==selected_state())
  })
  reactive_df2=reactive({
    reactive_df1()[c('Non-Chevrolet','Chevrolet')]
  })
  
  reactive_df3=reactive({
    cv_ev_agg%>%filter(County==selected_county_predict(),State==selected_state_predict())
  })
  
  reactive_df4=reactive({
    cv_ev_prdct%>%filter(County==selected_county_predict(),State==selected_state_predict(),Year>=2020)
  })
  
  reactive_df5=reactive({
    cv_chrg_prdct%>%filter(County==selected_county_predict(),State==selected_state_predict(),Year>=2020)
  })
  
  reactive_db = reactive({
    cv_ev_agg %>% filter(Year == formatted_date())
  })
  
  output$reactive_chrg_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$Total_Chargers), big.mark=",")," ")
  })
  
  output$reactive_reg_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$Vehicles), big.mark=","), " ")
  })
  
  output$reactive_chevy_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$VehiclesChevrolet), big.mark=","), " ")
  })
  
  output$reactive_state_count <- renderText({
    paste0(nrow(subset(reactive_db())), " counties affected")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      

      addCircleMarkers(data = reactive_db(), lat = ~ pclat10, lng = ~ pclon10, weight = 1, radius = ~(Total_Chargers)^(2/5.5), 
                       fillOpacity = 0.1, color = chrg_col, group = "Charging Stations",
                       label = sprintf("<strong>%s County</strong><br/>Total chargers: %g<br/>Total EV Vehicles: %d<br/>Chevy EV Vehicles: %g<br/>Chevy Market Share: %g", reactive_db()$County, reactive_db()$Total_Chargers, reactive_db()$Vehicles, reactive_db()$VehiclesChevrolet, reactive_db()$VehiclesChevrolet_pct) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = chrg_col),
                         textsize = "15px", direction = "auto"))%>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ pclat10, lng = ~ pclon10, weight = 1, radius = ~(Vehicles)^(1/4), 
                     fillOpacity = 0.1, color = allVh_col, group = "All EV Vehicles",
                     label = sprintf("<strong>%s County</strong><br/>Total chargers: %g<br/>Total EV Vehicles: %d<br/>Chevy EV Vehicles: %g<br/>Chevy Market Share: %g", reactive_db()$County, reactive_db()$Total_Chargers, reactive_db()$Vehicles, reactive_db()$VehiclesChevrolet, reactive_db()$VehiclesChevrolet_pct) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px", "color" = allVh_col),
                       textsize = "15px", direction = "auto"))%>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ pclat10, lng = ~ pclon10, weight = 1, radius = ~(VehiclesChevrolet)^(1/4), 
                       fillOpacity = 0.1, color = Chevy_col, group = "Chevy EV Vehicles",
                       label = sprintf("<strong>%s County</strong><br/>Total chargers: %g<br/>Chevrolet EV Vehicles: %d<br/>Chevy EV Vehicles: %g<br/>Chevy Market Share: %g", reactive_db()$County, reactive_db()$Total_Chargers, reactive_db()$Vehicles, reactive_db()$VehiclesChevrolet, reactive_db()$VehiclesChevrolet_pct) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = Chevy_col),
                         textsize = "15px", direction = "auto"))
  })
  
  output$rawtable <- renderPrint({
    cv_ev_sub = cv_ev_data_out
    orig <- options(width = 1000)
    print(tail(cv_ev_sub, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("EV_data", ".csv", sep="")
    },
    content = function(file) {
      cv_ev_sub = cv_ev_agg
      write.csv(cv_ev_sub, file)
    }
  )
  
  #output to waffle chart
  output$waffle_chart<-renderPlot({waffle(reactive_df2(), rows=5, size=0.8,
                                          colors=c("#44D2AC", "#E48B8B"),
                                          
                                          title="Chevrolet Market Share",
                                          xlab=paste("Chevrolet Vehicles ",reactive_df1()$VehiclesChevrolet,"\n All Vehicles",reactive_df1()$Vehicles, "\n Chevrolet Market Share:",round(reactive_df1()$VehiclesChevrolet_pct,2)))
  })
  output$lineChart<-renderPlotly({
    ggplotly(ggplot(data=reactive_df1(), aes(x = Year, y = VehiclesChevrolet_pct,color = "#045a8d")) +
                    geom_point(size = 3) +
                     geom_line(data=reactive_df0(), aes(x = Year, y = VehiclesChevrolet_pct,color = "#662506"))+
               theme(legend.position = "none") +
                labs(y="Chevrolet Market Share", x = "Year")+
               scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 2)))
  })
  
  output$predictLineChart<-renderPlotly({
    ggplotly(ggplot() +
               geom_line(data=reactive_df3(), aes(x = Year, y = VehiclesChevrolet,color = "Actual"))
               +geom_line(data=reactive_df4(), aes(x = Year, y =  fcst,color = "Forecasted"),linetype='dotted')+
               scale_color_manual(labels = c("Actual", "Forecasted"), values = c("blue", "red"))+
               labs(y="Chevrolet Vehicle Count", x = "Year", title = "Chevrolet Vehicle Count Prediction",color = "")
    )
  })
  
  output$predictLineChartStation<-renderPlotly({
    ggplotly(ggplot() +
               geom_line(data=reactive_df3(), aes(x = Year, y = Total_Chargers,color = "Actual"))+
               geom_line(data=reactive_df5(), aes(x = Year, y =  fcst,color = "Forecasted"),linetype='dotted')+
               scale_color_manual(labels = c("Actual", "Forecasted"), values = c("blue", "red"))+
               labs(y="Charging Station Count", x = "Year", title = "Charging Station Count Prediction",color = "")
    )
  })
}

shinyApp(ui, server)


