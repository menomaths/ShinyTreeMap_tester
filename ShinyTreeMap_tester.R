## tester to show time series of tree health on a map
## using dummy data of a 'Tree Health Metric' 
## and using R packages Shiny, Mapview among others
##
## 0) Preamble - install, load packages & sources and set working directory
## 1) Load and/or prepare data - currently uses dummy data around Wymondham
## 2) Map and Plot functions - outputs to be displayed
## 3) Shiny UI code
## 4) Shiny server code
##
## 0) Preamble ############################### ----

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(shiny)) install.packages("shiny")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(leaflet)) install.packages("leaflet")
#if (!require(mapview)) install.packages("mapview")

## 1) Load & Prepare Data ################## -----

# assume we have 100 trees with time series data for 365 days
# which have data of the form:
# ID(unique), longitude, latitude, tree health metric, dates
{
set.seed(1)
ID <- seq(1,100)
longitude <- runif(100,1.061661, 1.094212)
latitude <- runif(100,52.580011, 52.591466)

# dates sequence
dates <- seq(from = as.Date("2021-01-01"), to = as.Date("2021-12-31"), by = "1 days")

# treeHealth values will allow us to create a sequence of random values
# from one value to another value - could be improving/worsening tree health sequences
treeHealth <- rlnorm(length(ID)*length(dates), meanlog=0.25, sdlog=0.25)
# normalize and flip the treeHealth's value so that we have more
# healthy trees (=1) than ill or dead trees (=0)
treeHealth <- 1-treeHealth/max(treeHealth) 

# dataframe with (name) and location of each tree
tree.df <- data.frame(ID, longitude, latitude)

#initialise dataframe with (name, date) and health value columns
health.df <- data.frame(ID = integer(0), 
                        date = numeric(0), 
                        value = numeric(0))

# generate rest dummy tree health values for other 99 trees 
for (i in c(1:length(ID))){
  tmp <- data.frame(ID = ID[i], 
                    date = dates, 
                    value = seq(sample(treeHealth,1), sample(treeHealth,1), length.out=length(dates)))
  
  health.df <- rbind(health.df,tmp)
}
rm(tmp,i,ID,latitude,longitude,treeHealth)
}

## 2) Map & Plot Functions ##### ----

# Plotting parameters for map
pal_health <- colorBin(palette = "RdYlGn", domain = c(0,1), bins = seq(from = 0, to = 1, by = 0.1))

tree_health_map <-  function(plot_date){
  
  # plot_date <- as.Date("2021-01-01")
  
  # use currently selected date to obtain tree health values as at that date
  tmp.df <- tree.df
  date_vector <- which(health.df$date==plot_date)
  tmp.df$value <- round(health.df$value[date_vector],3)
  
  tree_map <- leaflet(data = tmp.df) %>%
    addTiles() %>% 
    addCircles(~longitude, 
               ~latitude,
               fillOpacity = 1,
               popup = ~as.character(paste("TREE ID: ",ID)),
               label = ~as.character(paste("HEALTH: ",value)),
               color = ~pal_health(value)) %>%
    addLegend("bottomright", pal = pal_health, values = ~value,
              title = "<small>Tree Health Metric</small>")
  return(tree_map)
  
}


## 3) SHINY UI ##### ----

UI <- bootstrapPage(
  #tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Tree Health Map</a>'),
             id="nav",
             windowTitle = "Tree Health Map",
             
             tabPanel("Tree Health Map 2021",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          # main output of map
                          leafletOutput("health_map",width="100%", height="100%"),
                          
                          absolutePanel(id="controls", class="panel panel-default",
                                        top=75, left = 55, width = 250, fixed = TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h5("tree health is an artificial metric based on numerous different measurements.")), 
                                             style = "color:#045a8d"),
                                        h4("100 trees in the study", align = "right"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select map date"),
                                                        choices = format(unique(dates), "%d %b %y"),
                                                        selected = format(min(unique(dates)), "%d %b %y"),
                                                        grid = FALSE,
                                                        animate = animationOptions(interval = 365, loop = FALSE)
                                        )
                          )
                      )
             )
  )
)

## 4) SHINY SERVER #### -----

SERVER <- function(input, output, session){
  
  # reactive values for Tree Health Map 2021 tab
  
  data <- reactive({
    
    input$plot_date
    
  })

  # generate a map plot using the reactive, currently selected date
  output$health_map <- renderLeaflet({
    
    plot_date <- format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
    
    tree_health_map(plot_date)

  })
}

# Run the application 
shinyApp(ui = UI, server = SERVER)