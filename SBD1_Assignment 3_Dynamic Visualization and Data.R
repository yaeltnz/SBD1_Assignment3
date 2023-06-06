# Assignment 3: Dynamic Visualization and Data

library(shiny)
library(shinydashboard)
library(leaflet)
library(rvest)

punktDaten <- data.frame(
  Jahr = c(2002, 2002, 2002, 2005, 2008, 2010, 2012, 2015, 2016, 2016, 2016, 2017, 2017, 2017, 2017, 2018, 2018, 2018, 2019, 2019, 2020, 2021, 2021, 2021, 2021, 2022, 2022, 2023, 2023, 2023),
  Country = c("New Zealand", "Singapore", "Australia", "Denmark", "Spain (Fuertaventura)", "Sweden", "Thailand", "Hungary (Budapest)", "Egypt", "France (Cannes)", "United kingdom (Eastbourne)", "Spain (Lloret de Mar)", "Maldives", "Sri Lanka", "France (Paris)", "Croatia (Split)", "United Kingdom (London)", "Portgual (Lisbon)", "France (Nice)", "Germany (Berlin)", "Croatia", "Greece (Rhodos)", "Turkey (Alacati)", "North Macedonia (Struga)", "Kosovo", "Greece (Kreta)", "Spain (Barcelona)", "Spain (Madrid)", "United Kingdom (London)", "Albania"),
  Latitude = c(-42.73391667675982, 1.3647200308326572, -24.51506429871501, 55.501592780911274, 28.365808226047143, 64.41175539807388, 15.377849982423193, 47.49779881023668, 26.8465929381391, 43.55231489277195, 50.76900024787598, 41.70023930609191, -0.7668386067917626, 7.631709088433756, 48.85747020720058, 43.51535437135369, 51.50254161850318, 38.723511645703155, 43.70929395514073, 52.519758291685825, 45.157087705372604, 36.435196556249714, 38.28844822370213, 41.182322674572475, 42.80195466201508, 35.13338304089379, 41.38568408312388, 40.41588395958578, 51.50254161850318, 41.003813740094195),
  Longitude = c(172.50628618212704, 103.84096411217276, 135.412706719962, 9.828952420398041, -14.001385379995678, 16.343917070091596, 100.83094869239977, 19.07055184731033, 30.08697890355662, 7.018205201765706, 0.2908911059909559, 2.8394070043689976, 72.97109617329308, 80.70041555897346, 2.351838342490967, 16.44461942521653, -0.12911132320937835, -9.142354250740125, 7.262057963509342, 13.417819579481279, 14.711120676934673, 28.218183547693982, 26.37721244333827, 20.69060068178356, 20.719408527558556, 24.99894299684512, 2.159754861561111, -3.7090771415480805, -0.12911132320937835, 20.18739024346358)
)

url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
page <- read_html(url)

# Extracting the data
data <- page %>%
  html_nodes("table.wikitable") %>%
  .[[1]] %>%
  html_nodes("tbody tr") %>%
  html_nodes("td:nth-child(2), td:nth-child(3)") %>%
  html_text()

# Reformatting the data
data <- matrix(data, ncol = 2, byrow = TRUE)
data <- as.data.frame(data)
colnames(data) <- c("Country", "Population")

ui <- dashboardPage(
  dashboardHeader(title = "My Vacations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Timeline", tabName = "timeline", icon = icon("calendar")),
      menuItem("Population", tabName = "population", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "timeline",
              fluidRow(
                column(width = 6,
                       selectInput(inputId = "jahrSelect", label = "Year", choices = as.character(2000:2023), selected = "2002")
                ),
                column(width = 6,
                       leafletOutput(outputId = "kartenOutput", width = "100%", height = "600px")
                )
              )
      ),
      tabItem(tabName = "population",
              fluidRow(
                column(width = 12,
                       dataTableOutput(outputId = "populationTable")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$tabelleOutput <- renderDataTable({
    data
  })
  
  output$kartenOutput <- renderLeaflet({
    jahr <- as.numeric(input$jahrSelect)
    
    filterDat <- punktDaten[punktDaten$Jahr == jahr, ]
    
    karte <- leaflet(data = filterDat) %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~Country)
    
    karte
  })
  
  output$populationTable <- renderDataTable({
    data
  })
  
}

shinyApp(ui = ui, server = server)

