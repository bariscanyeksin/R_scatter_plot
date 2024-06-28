library(shiny)
library(ggplot2)
library(readr)
library(plotly)
library(showtext)
library(dplyr)

font_add_google("Poppins")
showtext_auto()
Sys.setlocale(category = "LC_ALL", locale = "Portuguese_Portugal.1252")

ui <- fluidPage(
  tags$head(
    tags$script('
      $(document).ready(function() {
        $(".total_per90").click(function() {
          $(".total_per90").css("background-color", "");
          $(".total_per90").css("color", "#000");
          $(this).css("background-color", "#529bc4");
          $(this).css("color", "white");
        });
        $("#totalButton").css("background-color", "#529bc4");
        $("#totalButton").css("color", "white");
      });
    ')
  ),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #272727;",
      actionButton("totalButton", "Toplam", class = "total_per90"),
      actionButton("per90Button", "90 Dakika Başına", class = "total_per90"),
      selectizeInput("position", 
                     HTML("<br><span style='color:white;'>Pozisyon</span>"),
                     choices = c("DF", "MF", "FW"), selected = c("DF", "MF", "FW"), multiple = TRUE),
      selectizeInput("league", 
                     HTML("<span style='color:white;'>Lig</span>"),
                     choices = NULL, multiple = TRUE, selected = NULL),
      selectInput("country", 
                  HTML("<span style='color:white;'>Uyruk</span>"),
                  choices = NULL, selected = "ALL"),
      sliderInput("age", 
                  HTML("<span style='color:white;'>Yaş</span>"),
                  min = 16, max = 41, value = c(16, 41)),
      sliderInput("minute", 
                  HTML("<span style='color:white;'>Dakika</span>"),
                  min = 15*90, max = 46*90, value = c(15*90, 46*90)),
      style = "border: none;",
      uiOutput("xColumnUI"),
      uiOutput("yColumnUI"),
      actionButton("update", "Güncelle")
    ),
    mainPanel(
      plotlyOutput("scatterPlot", height = "600px", width = "1200px"),
    )
  ),
  tags$head(
    tags$title("FBref-Grafik"),
    tags$link(rel = "shortcut icon", href = "https://cdn.ssref.net/req/202405231/favicons/fb/favicon.ico"),
    tags$style(HTML("
      body, .content-wrapper, .main-panel, .wrapper {
        background-color: #272727 !important;
        color: white;
        font-family: 'Poppins', sans-serif;
      }
      .js-irs-0 .irs-single, 
      .js-irs-0 .irs-bar-edge, 
      .js-irs-0 .irs-bar,
      .js-irs-0 .irs-from,
      .js-irs-0 .irs-to,
      .js-irs-0 .irs-handle
      {
        background: #529bc4;
      }
      .js-irs-0 .irs-max,
      .js-irs-0 .irs-min{
        color: #333;
        background: #e6e6e6;
      }
      .js-irs-0 .irs-grid-pol{
        background: white;
      }
      .js-irs-1 .irs-single, 
      .js-irs-1 .irs-bar-edge, 
      .js-irs-1 .irs-bar,
      .js-irs-1 .irs-from,
      .js-irs-1 .irs-to,
      .js-irs-1 .irs-handle
      {
        background: #529bc4;
      }
      .js-irs-1 .irs-max,
      .js-irs-1 .irs-min{
        color: #333;
        background: #e6e6e6;
      }
      .js-irs-1 .irs-grid-pol{
        background: white;
      }
      .selectize-control.multi .selectize-input > div.active{
        background: #529bc4;
      }
    "))
  ),
  style = "background-color: #272727;"
)

server <- function(input, output, session) {
  
  url_total <- "csv_source"
  url_per90 <- "csv_source"
  
  data <- reactiveVal(read_csv(url_total))
  
  updateData <- function(url) {
    new_data <- read_csv(url)
    new_data <- new_data %>% arrange(Country)
    data(new_data)
    updateSelectizeInput(session, "league", choices = unique(new_data$Comp), selected = unique(new_data$Comp))
    updateSelectInput(session, "xColumn", choices = c(names(new_data)[which(names(new_data) == "Goals"):length(names(new_data))]))
    updateSelectInput(session, "yColumn", choices = c(names(new_data)[which(names(new_data) == "Goals"):length(names(new_data))]))
    updateSelectInput(session, "country", choices = c("ALL", unique(new_data$Country)), selected = "ALL")
  }
  
  observeEvent(input$totalButton, {
    updateData(url_total)
  })
  
  observeEvent(input$per90Button, {
    updateData(url_per90)
  })
  
  updateData(url_total)
  
  output$xColumnUI <- renderUI({
    selectInput("xColumn", HTML("<span style='color:white;'>İstatistik 1</span>"),
                choices = c(names(data())[which(names(data()) == "Goals"):length(names(data()))]))
  })
  
  output$yColumnUI <- renderUI({
    selectInput("yColumn", HTML("<span style='color:white;'>İstatistik 2</span>"), 
                choices = c(names(data())[which(names(data()) == "Goals"):length(names(data()))]))
  })
  
  output$scatterPlot <- renderPlotly({
    req(input$update)
    req(input$xColumn, input$yColumn)
    
    df <- data() %>% filter(`90s` > 15)
    
    df <- df[df$Pos %in% input$position, ]
    
    if (!is.null(input$league)) {
      df <- df[df$Comp %in% input$league, ]
    }
    
    if (input$country != "ALL") {
      df <- df[df$Country %in% input$country, ]
    }
    
    df <- df[df$Age >= input$age[1] & df$Age <= input$age[2], ]
    
    df <- df %>% filter(`90s`*90 >= input$minute[1] & `90s`*90 <= input$minute[2])
    
    x_median <- median(df[[input$xColumn]], na.rm = TRUE)
    y_median <- median(df[[input$yColumn]], na.rm = TRUE)
    
    df_filtered <- df %>% 
      filter(df[[input$xColumn]] > x_median & df[[input$yColumn]] > y_median)
    
    x_mean <- mean(df_filtered[[input$xColumn]], na.rm = TRUE)
    y_mean <- mean(df_filtered[[input$yColumn]], na.rm = TRUE)
    
    labeled_players <- df %>%
      filter(df[[input$xColumn]] > x_median)
    
    top_players <- df_filtered %>%
      filter(df_filtered[[input$xColumn]] >= quantile(df_filtered[[input$xColumn]], 0.93, na.rm = TRUE) | 
               df_filtered[[input$yColumn]] >= quantile(df_filtered[[input$yColumn]], 0.93, na.rm = TRUE))
    
    p <- ggplot(df_filtered, aes_string(x = input$xColumn, y = input$yColumn, label = "Player")) +
      geom_point(aes(text = paste0(
        "<b>Player:</b> ", Player, "<br><b>Team:</b> ", Squad, "<br><b>Age:</b> ", Age, "<br>",
        "<b>", input$xColumn, ":</b> ", !!as.name(input$xColumn), "<br>",
        "<b>", input$yColumn, ":</b> ", !!as.name(input$yColumn)
      )), 
      colour = ifelse(df_filtered[[input$xColumn]] > x_mean | df_filtered[[input$yColumn]] > y_mean, 
                      "#529bc4", '#4e5254'), shape = 16, size = 2) +
      geom_text(data = top_players, aes(label = Player), color = "white", size = 3, nudge_x = diff(range(df_filtered[[input$xColumn]])) * 0.025, nudge_y = diff(range(df_filtered[[input$yColumn]])) * 0.02) +
      labs(x = input$xColumn,
           y = input$yColumn) +
      scale_x_continuous(n.breaks = 10) +
      scale_y_continuous(n.breaks = 10) +
      theme(plot.background = element_rect(fill = "#272727", colour = "#272727"),
            panel.background = element_rect(fill = "#272727", colour = "#272727"),
            title = element_text(family="Poppins", color = 'white'),
            axis.title.x = element_text(family="Poppins", color = 'white'),
            axis.title.y = element_text(family="Poppins", color = 'white'),
            axis.text.x = element_text(family="Poppins", color = 'white'),
            axis.text.y = element_text(family="Poppins", color = 'white'),
            panel.grid.major = element_line(color = "#4e5254", linetype = "dashed"),
            panel.grid.minor = element_line(color = "white", linetype="dashed"),
            plot.margin = unit(c(1, 1, 2, 1), "cm")
      )
    
    ggplotly(p, tooltip = c("text")) %>% layout(
      annotations = list(
        x = 1,
        y = 0,
        xref = "paper",
        yref = "paper",
        text = "Data: FBref",
        showarrow = FALSE,
        font = list(color = "#4e5254")
      )
    ) %>% layout(font = list(family = "Poppins"),
                 hoverlabel = list(
                   bgcolor = "#529bc4",
                   font = list(family = "Poppins", color = "white")
                 )
    )
  })
}

shinyApp(ui = ui, server = server)