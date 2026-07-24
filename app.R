
Sys.setlocale("LC_TIME", "English")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(stringr)


ps_clean <- readRDS("ps_clean.rds")
ps_clean <- ps_clean %>% filter(!is.na(Category), Category != "")


format_num <- function(x) {
  ifelse(is.na(x), "N/A", format(round(x), big.mark = ",", scientific = FALSE))
}


vivid_base <- c(
  "#E6194B", "#3CB44B", "#4363D8", "#F58231", "#911EB4",
  "#F032E6", "#BCF60C", "#FABEBE", "#008080", "#9A6324",
  "#800000", "#808000", "#FF4500", "#2E8B57", "#DA70D6",
  "#1E90FF", "#FFD700", "#8B008B", "#00CED1", "#FF69B4",
  "#7FFF00", "#DC143C", "#00FA9A", "#FF6347", "#4682B4",
  "#D2691E", "#20B2AA", "#9932CC", "#FF8C00", "#6495ED",
  "#B22222", "#228B22", "#DB7093"
)

all_categories <- sort(unique(ps_clean$Category))

category_palette <- setNames(
  colorRampPalette(vivid_base)(length(all_categories)),
  all_categories
)

type_palette <- c("Free" = "#2ECC71", "Paid" = "#3498DB")


install_buckets <- c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000,
                      100000, 500000, 1000000, 5000000, 10000000,
                      50000000, 100000000, 500000000, 1000000000)

install_bucket_labels <- sapply(install_buckets, format_num)

update_year_min <- min(ps_clean$Update_Year, na.rm = TRUE)
update_year_max <- max(ps_clean$Update_Year, na.rm = TRUE)

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Play Store Analytics", titleWidth = 280),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      br(),
      pickerInput(
        inputId  = "category",
        label    = "Category:",
        choices  = all_categories,
        selected = all_categories,
        multiple = TRUE,
        options  = list(`actions-box` = TRUE, `live-search` = TRUE,
                         `selected-text-format` = "count > 3")
      ),
      radioGroupButtons(
        inputId  = "type",
        label    = "Type:",
        choices  = c("All", "Free", "Paid"),
        selected = "All",
        status   = "primary",
        justified = TRUE
      ),
      pickerInput(
        inputId  = "content_rating",
        label    = "Content Rating:",
        choices  = levels(ps_clean$Content.Rating),
        selected = levels(ps_clean$Content.Rating),
        multiple = TRUE,
        options  = list(`actions-box` = TRUE)
      ),
      sliderInput(
        inputId = "rating_range",
        label   = "Rating range:",
        min = 0, max = 5, value = c(0, 5), step = 0.1
      ),
      sliderTextInput(
        inputId  = "installs_range",
        label    = "Installs range:",
        choices  = install_bucket_labels,
        selected = range(install_bucket_labels),
        grid     = FALSE
      ),
      sliderInput(
        inputId = "update_year_range",
        label   = "Last Updated - Year:",
        min = update_year_min, max = update_year_max,
        value = c(update_year_min, update_year_max),
        step = 1, sep = ""
      ),
      br()
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-top: 3px solid #3c8dbc; }
    "))),

    fluidRow(
      valueBoxOutput("box_apps",     width = 3),
      valueBoxOutput("box_rating",   width = 3),
      valueBoxOutput("box_installs", width = 3),
      valueBoxOutput("box_reviews",  width = 3)
    ),

    fluidRow(
      box(title = "Total Installs by Category", width = 6, status = "primary",
          solidHeader = TRUE, plotlyOutput("plot_installs", height = 380)),
      box(title = "Free vs Paid", width = 6, status = "primary",
          solidHeader = TRUE, plotlyOutput("plot_freepaid", height = 380))
    ),

    fluidRow(
      box(title = "Rating Distribution", width = 6, status = "primary",
          solidHeader = TRUE, plotlyOutput("plot_rating_dist", height = 380)),
      box(title = "Average Rating by Category", width = 6, status = "primary",
          solidHeader = TRUE, plotlyOutput("plot_avg_rating", height = 380))
    ),

    fluidRow(
      box(title = "Apps Updated Over Time", width = 12, status = "primary",
          solidHeader = TRUE, plotlyOutput("plot_timeline", height = 350))
    ),

    fluidRow(
      box(title = "Rating vs Reviews (bubble size = Installs)", width = 8,
          status = "primary", solidHeader = TRUE,
          plotlyOutput("plot_scatter", height = 420)),
      box(title = "Content Rating Breakdown", width = 4, status = "primary",
          solidHeader = TRUE, plotlyOutput("plot_content_rating", height = 420))
    ),

    fluidRow(
      box(title = "Top 10 Most Installed Apps", width = 12, status = "primary",
          solidHeader = TRUE,
          plotlyOutput("plot_top10", height = 420),
          tags$p(style = "color:#7f8c8d; font-size:12px; margin-top:8px;",
                 "Note: Google Play reports installs as broad ranges (e.g. 500,000,000+), so several top apps can share the same value. Ranked by Installs, then by Reviews as a tiebreaker.")
      )
    )
  )
)


server <- function(input, output, session) {

  filtered <- reactive({
    installs_lo <- as.numeric(gsub(",", "", input$installs_range[1]))
    installs_hi <- as.numeric(gsub(",", "", input$installs_range[2]))

    df <- ps_clean %>%
      filter(Category %in% input$category) %>%
      filter(Content.Rating %in% input$content_rating) %>%
      filter(is.na(Rating) | (Rating >= input$rating_range[1] & Rating <= input$rating_range[2])) %>%
      filter(is.na(Installs_num) | (Installs_num >= installs_lo & Installs_num <= installs_hi)) %>%
      filter(is.na(Update_Year) | (Update_Year >= input$update_year_range[1] & Update_Year <= input$update_year_range[2]))

    if (input$type != "All") {
      df <- df %>% filter(Type == input$type)
    }

    df
  })

  output$box_apps <- renderValueBox({
    valueBox(format_num(nrow(filtered())), "Total Apps",
             icon = icon("mobile-alt"), color = "green")
  })

  output$box_rating <- renderValueBox({
    avg_r <- mean(filtered()$Rating, na.rm = TRUE)
    valueBox(ifelse(is.nan(avg_r), "N/A", round(avg_r, 2)), "Average Rating",
             icon = icon("star"), color = "yellow")
  })

  output$box_installs <- renderValueBox({
    valueBox(format_num(sum(filtered()$Installs_num, na.rm = TRUE)), "Total Installs",
             icon = icon("download"), color = "blue")
  })

  output$box_reviews <- renderValueBox({
    valueBox(format_num(sum(filtered()$Reviews, na.rm = TRUE)), "Total Reviews",
             icon = icon("comments"), color = "purple")
  })

  output$plot_installs <- renderPlotly({
    d <- filtered() %>%
      group_by(Category) %>%
      summarise(Total_Installs = sum(Installs_num, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Installs))

    plot_ly(
      d, x = ~Total_Installs, y = ~reorder(Category, Total_Installs),
      type = "bar", orientation = "h",
      marker = list(color = category_palette[d$Category]),
      hovertemplate = "%{y}: %{x:,.0f}<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Total Installs"), yaxis = list(title = ""))
  })

  output$plot_freepaid <- renderPlotly({
    d <- filtered() %>% count(Type)
    plot_ly(
      d, labels = ~Type, values = ~n, type = "pie", hole = 0.55,
      marker = list(colors = type_palette[as.character(d$Type)]),
      textinfo = "label+percent"
    )
  })

  output$plot_rating_dist <- renderPlotly({
    plot_ly(
      filtered(), x = ~Rating, type = "histogram",
      marker = list(color = "#F58231", line = list(color = "white", width = 0.5))
    ) %>%
      layout(xaxis = list(title = "Rating"), yaxis = list(title = "Count"))
  })

  output$plot_avg_rating <- renderPlotly({
    d <- filtered() %>%
      group_by(Category) %>%
      summarise(Avg_Rating = mean(Rating, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.nan(Avg_Rating)) %>%
      arrange(desc(Avg_Rating))

    plot_ly(
      d, x = ~Avg_Rating, y = ~reorder(Category, Avg_Rating),
      type = "bar", orientation = "h",
      marker = list(color = category_palette[d$Category]),
      hovertemplate = "%{y}: %{x:.2f}<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Average Rating", range = c(0, 5)), yaxis = list(title = ""))
  })

  output$plot_timeline <- renderPlotly({
    d <- filtered() %>%
      filter(!is.na(Last.Updated)) %>%
      mutate(YearMonth = format(Last.Updated, "%Y-%m")) %>%
      count(YearMonth) %>%
      arrange(YearMonth)

    plot_ly(
      d, x = ~YearMonth, y = ~n, type = "scatter", mode = "lines+markers",
      line = list(color = "#4363D8", width = 3),
      marker = list(color = "#E6194B", size = 6),
      hovertemplate = "%{x}<br>Updates: %{y}<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Year-Month", tickangle = -45),
             yaxis = list(title = "Number of Apps Updated"))
  })

  output$plot_scatter <- renderPlotly({
    d <- filtered() %>% filter(!is.na(Rating), !is.na(Reviews), Reviews > 0)
    req(nrow(d) > 0)

    max_sqrt <- max(sqrt(d$Installs_num + 1), na.rm = TRUE)

    plot_ly(
      d, x = ~Reviews, y = ~Rating, type = "scatter", mode = "markers",
      text = ~App,
      hovertemplate = "%{text}<br>Reviews: %{x:,.0f}<br>Rating: %{y}<extra></extra>",
      marker = list(
        size = ~sqrt(Installs_num + 1),
        sizemode = "area",
        sizeref = 2 * max_sqrt / (40^2),
        color = category_palette[d$Category],
        opacity = 0.55,
        line = list(width = 0.3, color = "white")
      )
    ) %>%
      layout(xaxis = list(title = "Reviews (log scale)", type = "log"),
             yaxis = list(title = "Rating", range = c(0, 5)))
  })

  output$plot_content_rating <- renderPlotly({
    d <- filtered() %>% count(Content.Rating) %>% filter(n > 0)
    plot_ly(
      d, labels = ~Content.Rating, values = ~n, type = "pie", hole = 0.5,
      marker = list(colors = vivid_base[seq_len(nrow(d))]),
      textinfo = "label+percent"
    )
  })

    output$plot_top10 <- renderPlotly({
    d <- filtered() %>%
      arrange(desc(Installs_num), desc(Reviews)) %>%
      distinct(App, .keep_all = TRUE) %>%
      head(10) %>%
      mutate(
        App = factor(App, levels = rev(App)),
        Label = paste0(format_num(Installs_num), "+  |  ", format_num(Reviews), " reviews")
      )

    plot_ly(
      d, x = ~Installs_num, y = ~App,
      type = "bar", orientation = "h",
      marker = list(color = category_palette[d$Category]),
      text = ~Label,
      textposition = "outside",
      hovertemplate = "%{y}<br>Installs: %{x:,.0f}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Installs", range = c(0, max(d$Installs_num) * 1.35)),
        yaxis = list(title = ""),
        margin = list(r = 120)
      )
  })
}


shinyApp(ui, server)
