library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)


files <- lapply(list.files(pattern='*.csv'), read.csv)
names(files) <- list.files(pattern='*.csv')
battle <- files$battles.csv %>% 
  mutate(summer = as.factor(summer)) %>% 
  tidyr::pivot_longer(c("attacker_size","defender_size"), names_to = "battle_side", values_to = "size_count")

# options for selectInput
region_options <- unique(battle[["region"]])
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GoT"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        shiny::selectInput(inputId ="region", 
                           label = "Select the Region",
                           choices = region_options
        )
      ),
      
      mainPanel(
        plotOutput("battlePlot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(session,input, output) {
    data <- reactive({
      d <- battle %>% dplyr::filter(region == input$region)
      return(d)
    })
    output$battlePlot <- renderPlot({
      p <- ggplot(data = data(), aes(x=name)) +
        geom_bar(aes(y=size_count, fill=battle_side), stat="identity") +
        scale_fill_manual(values = c("#31a7cd","#ee8e4a"))
        theme_minimal()
      p
 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
