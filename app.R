library(tidyverse)
library(shiny)
library(bslib)

ui <- fluidPage(
  
  theme = bs_theme(preset = "flatly"),
  
  # Titel der App
  titlePanel("Simulation zum Zentralen Grenzwertsatz für Mittelwerte"),
  
  tabsetPanel(
    tabPanel(title = "App",
             value = TRUE,
      
    sidebarLayout(
    
    # Sidebar mit Inputs für Graphic
    
    sidebarPanel(
      
      selectInput(
        "distrib",
        label = "Populationsverteilung",
        choices = c("normal", "multi-modal", "rechtsschief"),
        selected = "multi-modal",
        width = "130px"
      ),
      
      numericInput(
        "pop_size",
        label = "Populationsgröße",
        value = 1000,
        min = 100,
        max = 5000,
        step = 10,
        width = "150px"
      ),
      
      checkboxInput(
        "show_sample",
        "Stichproben ziehen?",
        value = FALSE
      ),
      
      sliderInput(
        "n_stichproben",
        label = "Anzahl der gezogenen Stichproben",
        value = 1,
        min = 1,
        max = 100,
        step = 1
        ),
      
      sliderInput(
        "size_stichprobe",
        label = "Größe der einzelnen gezogenen Stichproben",
        value = 30,
        min = 1,
        max = 300,
        step = 1
      ),
      
      checkboxInput(
        "use_seed",
        label = "Seed verwenden?",
        value = FALSE
      ),
      
    ),
    
    mainPanel(
      plotOutput("plot",
                 height = "550px")
    )
    
  )
    )
  ,
  tabPanel(title = "Hinweise",
           value = FALSE,
  fluidRow(
    column(width = 10,
           offset = 1,
           htmlOutput("hinweise")
    )
  )
  )
  )
)

server <- function(input, output) {
  
  distribution <- reactive({
    if (input$use_seed) {set.seed(2345)}
    
    if (input$distrib == "normal") {
      rnorm(input$pop_size, mean = 100, sd = 10)
    } else if (input$distrib == "multi-modal") {
      c(
        rnorm(0.6*input$pop_size, mean = 10, sd = 8),
        rnorm(0.2*input$pop_size, mean = 100, sd = 5),
        rnorm(0.3*input$pop_size, mean = 150, sd = 5)
      )
    } else if (input$distrib == "rechtsschief") {
      rlnorm(input$pop_size, meanlog = 5, sdlog = 0.4)
    }
  })
  
  distribution_data <- reactive({
    tibble(x = distribution())
  })
  
  means_data <- reactive(
    {
      if (input$use_seed) {set.seed(2345)}
      
      if (input$show_sample) {
          means <- numeric(input$n_stichproben)
          
          for (i in seq_len(input$n_stichproben)) {
            means[i] <-
              mean(sample(
                distribution_data()$x,
                size = input$size_stichprobe,
                replace = TRUE
              ))
          }
          tibble(x = means)
        }
      }
  )
  
  output$plot <- renderPlot({
    gg <- ggplot() +
      geom_vline(data = distribution_data(),
                 mapping = aes(xintercept = mean(x),
                               color = "Populationsmittelwert"),
                 show.legend = TRUE) +
      geom_density(
        data = distribution_data(),
        mapping = aes(x = x,
                      fill = "Populationsverteilung"),
        alpha = 0.8
      ) +
      theme_bw()
    
    if (input$show_sample) {
      gg <- gg +
        geom_vline(data = means_data(),
                   mapping = aes(xintercept = mean(x),
                                 color = "Stichprobenmittelwert"),
                   show.legend = TRUE)
    
      if (input$n_stichproben > 1) {
        gg <- gg +
          geom_density(
            data = means_data(),
            mapping = aes(x = x,
                          fill = "Stichprobenverteilung"),
            alpha = 0.5
          )
      }
        gg <- gg +
          scale_color_manual(
            values = c("Populationsmittelwert" = "darkblue",
                       "Stichprobenmittelwert" = "darkgreen")
          ) +
          scale_fill_manual(
            values = c("Populationsverteilung" = "lightblue",
                       "Stichprobenverteilung" = "lightgreen")
          )
    
    } else {
      gg <- gg +
        scale_color_manual(
          values = c("Populationsmittelwert" = "darkblue")
        ) +
        scale_fill_manual(
          values = c("Populationsverteilung" = "lightblue")
        )
    }
    
    gg <- gg +
      labs(color = "",
           fill = "",
           x = "metrisches Merkmal",
           y = "Dichte") +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            text = element_text(size = 16), 
            axis.text = element_text(size = 15)) +
      guides(fill = guide_legend(order = 0),
             color = guide_legend(order = 1))
    
    gg
  })
  
  output$hinweise <- renderText({
    HTML(
      "<br>
      <b>Allgemeines:</b><br>
      Diese kleine App soll dazu dienen, den in der statistischen Inferenz fundamentalen 
      Zetralen Grenzwertsatz für Mittelwerte (ZGM) zu verdeutlichen. Der ZGM besagt, dass
      bei einer häufig wiederholten Stichprobenziehung aus einer Merkmalsverteilung einer 
      Population die Mittelwerte dieser Stichproben immer normalverteilt um den wahren 
      Populationsmittelwert streuen. Dabei ist es egal, welche Verteilung das Merkmal in der
      Population ursprünglich hat. Dieser Zusammenhang soll hier simuliert werden. <br>
      Die App bietet die Möglichkeit, unterschiedliche Populationsverteilung zu wählen und 
      anschließend die Anzahl der gezogenen Stichproben und die Stichprobengröße festzulegen.
      Nun wird die Verteilung der Stichprobenmittelwerte über der Populationsverteilung angezeigt.
      Es gibt die Möglichkeit, einen Seed für die Stichprobenziehung zu verwenden,
      damit die angezeigten Ergebnisse reproduzierbar sind. Diese Funktion eignet sich z.B.
      für die Anwendung in der Lehre. <br><br>
      <b>Kontakt:</b><br>
      Jonas Frost <br>
      GitHub: <a href='https://github.com/analyticsociologyrox'>analyticsociologyrox</a>
      
      "
    )
    })
}

shinyApp(ui, server)
