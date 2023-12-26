
library(shiny)
library(leaflet)
library(readxl)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DCS and Customers locations Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput('file1', 'Choose xlsx file with DCs data',
                    accept = c(".xlsx")),
          fileInput('file2', 'Choose xlsx file with Customer data',
                    accept = c(".xlsx")),
          fileInput('file3', 'Choose xlsx file with Costs statistics data',
                    accept = c(".xlsx")),
          fileInput('file4', 'Choose xlsx file with Service level data',
                    accept = c(".xlsx")),
          plotOutput("plot2"), 
          plotOutput("plot3"), 
          plotOutput("plot4")

        ),

        mainPanel(
          leafletOutput("mymap"),
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), tableOutput('file1'),tableOutput('file2'))
          )
          
        )
    )
)


server <- function(input, output, session) {
  output$file2 <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })
  output$file1 <- renderTable({
    inFile <- input$file2
    if(is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })
  output$file3 <- renderTable({
    inFile <- input$file3
    if(is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })
  output$file4 <- renderTable({
    inFile <- input$file4
    if(is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })
  
  m <- leaflet() %>%
    setView(lng = 60, lat = 55, zoom = 4) %>%
    addProviderTiles(providers$CartoDB.Positron)
  output$mymap <- renderLeaflet(m)
  observe({
    req(c(input$file1, input$file2))
    inFile <- input$file1
    inFile2 <- input$file2
    inFile3 <- input$file3
    inFile4 <- input$file4
    if(is.null(inFile))
      return(NULL)
    if(is.null(inFile2))
      return(NULL)
    if(is.null(inFile3))
      return(NULL)
    if(is.null(inFile4))
      return(NULL)
    
    exceldata <- read_excel(inFile$datapath)
    df = data.frame(exceldata)
    exceldata2 <- read_excel(inFile2$datapath)
    df2 = data.frame(exceldata2)
    exceldata3 <- read_excel(inFile3$datapath)
    df3 = data.frame(exceldata3)
    exceldata4 <- read_excel(inFile4$datapath)
    df4 = data.frame(exceldata4)
    
    proxy <- leafletProxy("mymap", data = df, session)
    proxy %>% addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo")
    proxy %>% addProviderTiles(providers$OpenTopoMap, group = "TopoMap")
    proxy %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
    proxy %>% addProviderTiles(providers$OpenRailwayMap, group = "Rail")
    proxy %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group = "Nasa")
    proxy %>% addCircles(df$long, df2$lat, radius = (df2$demand)*10, popup = df2$city, color = "green")
    
    proxy %>% addMarkers(df$long, df$lat, group= df$group, popup = df$ID)
    
    proxy %>% addLayersControl(baseGroups = c("Drawing", "Satellite", "NatGeo", "TopoMap", "NASA", "Rail" ),
                               overlayGroups = c("1", "2", "3"),
                               options = layersControlOptions(collapsed = FALSE))
    
    output$plot2 <- renderPlot(
      ggplot(df2) + 
      aes(x = reorder(ID,demand), y = demand) +
      geom_col(fill = "green") +
      labs (
        x = "ID",
        y = "Demand",
        title = "Demand level",
        subtitle = "15 customers",
        caption = "data from anyLogistix"
      ) +
      coord_flip() +
      theme_minimal()
    )
    
    output$plot3 <- renderPlot(
      ggplot(df3) + 
        aes(x = reorder(ID,costs), y = costs) +
        geom_col(fill = "yellow") +
        labs (
          x = "ID",
          y = "Costs",
          title = "Total cost",
          subtitle = "5 warehouses",
          caption = "data from anyLogistix"
        ) +
        coord_flip() +
        theme_minimal()
    )

    output$plot4 <- renderPlot(
      ggplot(df4) + 
        aes(x = reorder(ID,sl), y = sl) +
        geom_col(fill = "blue") +
        labs (
          x = "ID",
          y = "sl",
          title = "Service level",
          subtitle = "15 clients",
          caption = "data from anyLogistix"
        ) +
        coord_flip() +
        theme_minimal()
    )  
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
