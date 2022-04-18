source("helpers.R")

# Define UI for application that draws a histogram
ui <- dashboardPage (
  dashboardHeader(title = "Video Games Panel"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Sales", tabName = "sales", icon = icon("chart-bar")),
      menuItem("Games Infos", tabName = "infos", icon = icon("chart-area"))
    ),
    tags$hr(),
    textOutput("min_max"),
    tags$hr(),
    htmlOutput("dev_info")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .shiny-text-output {
          margin: 1%;
          text-align: center;
      }'))),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("nr_games"),
                valueBoxOutput("nr_genre"),
                valueBoxOutput("nr_platforms")
              ),
              h2("Dataset Overview"),
              tags$hr(),
              DT::dataTableOutput("mytable")
      ),
      tabItem(tabName = "sales",
              selectInput("genre_filter", 
                          label = "Genres",
                          choices = genres,
                          selected = genres[1]),
              fluidRow(
                box(plotOutput("global_sales", width = "700"),
                    title = "Global Sales by Genre", status = "primary", solidHeader = TRUE,
                ),
                box(plotOutput("release_count", width = "700"),
                    title = "Release Games Count by Genre", status = "primary", solidHeader = TRUE,
                )
              ),
              fluidRow(
                box(plotOutput("sales_top_games", width = "750"),
                    title = "Global Sales Scores of Top Games", status = "primary", solidHeader = TRUE
                ),
                box(sliderInput("bins",
                                "Number of games in the Top",
                                min = 3,
                                max = 40,
                                value = 10),
                    title = "Top N Games", solidHeader = TRUE
                ),
                box(title = "Total Sales per Region in Millions", solidHeader = TRUE,
                    htmlOutput("total_sales")
                )
              )
      ),
      tabItem(tabName = "infos",
              sliderInput("slider_year",
                          "Year Range:",
                          min = 1985,
                          max = 2016,
                          value = c(2000, 2016)),
              fluidRow(
                box(plotOutput("platform_freq", width = "700"),
                    title = "Platform Count by Year Range", status = "primary", solidHeader = TRUE,
                ),
                box(plotOutput("genre_top", width = "700"),
                    title = "Top Genres by Year Range", status = "primary", solidHeader = TRUE,
                )
              )
      )
    ),
  )
)


server <- function(input, output) {
  output$min_max <- renderText({ 
    paste("This data set simply extends the number of variables with another web scrape from Metacritic.
          It contains a list of video games with sales in millions of unit until 2016.")
  })
  
  output$dev_info <- renderText({ 
    paste("Developed by Francielle Vasconcellos Pereira. <br/>Case - Data Analyst<br/>
          <a href='https://github.com/franciellevp/game-sales' target='_blank'>Code on Github</a>")
  })
  
  output$total_sales <- renderText({ 
    paste("<b>North America Sales: </b>", sum(games$NA_Sales),
          "<br/><b>European Union Sales: </b>", sum(games$EU_Sales),
          "<br/><b>Japan Sales: </b>", sum(games$JP_Sales),
          "<br/><b>Others (Africa, Australia and the rest of Asia and Europe): </b>", sum(games$Other_Sales)
    )
  })
  
  #########################
  # Global Sales by Genre #
  #########################
  output$global_sales <- renderPlot({
    data <- subset(games, Genre == input$genre_filter, select = c("Year_Release", "Global_Sales"))
    data <- aggregate(data$Global_Sales, by = list(Year_Release = data$Year_Release), FUN=sum)
    ggplot(data, aes(x = Year_Release, y = x)) +
      geom_line(color="#3c8dbc") +
      geom_point(color="steelblue", fill="steelblue", size=2) +
      labs(x = "Year Release", y = "Total of Global Sales") +
      theme(panel.background = element_rect(fill="snow"),
            panel.grid.minor = element_blank())
  })
  
  ################################
  # Release Games Count by Genre #
  ################################
  output$release_count <- renderPlot({
    data <- subset(games, Genre == input$genre_filter)
    data %>%
      select(Year_Release, NA_Sales.Log, EU_Sales.Log, JP_Sales.Log, 
             Other_Sales.Log, Global_Sales.Log) %>%
      melt(id.vars = "Year_Release") %>%
      group_by(Year_Release, variable) %>% 
      summarise(total.sales = sum(value)) %>%
      ggplot(aes(x = Year_Release, y = total.sales, color = variable, group = variable)) +
      geom_point() + 
      geom_line() + 
      labs(x = "Year Release", y = "Total Sales Log Value", color = "Region") +
      theme(panel.background = element_rect(fill="snow"),
            panel.grid.minor = element_blank())
  })
  
  ################################
  # Platform Count by Year Range #
  ################################
  output$platform_freq <- renderPlot({
    data <- subset(games, Year_Release >= input$slider_year[1] & Year_Release <= input$slider_year[2], select = c("Platform_Category"))
    ggplot(data, aes(x=Platform_Category)) + 
      geom_bar(aes(Platform_Category, fill=..count..)) + 
      geom_text(stat='count', aes(label=..count..), vjust=-1, color = "steelblue")  +
      theme(panel.background = element_rect(fill="snow"),
            panel.grid.minor = element_blank())
  })
  
  ############################
  # Top Genres by Year Range #
  ############################
  output$genre_top <- renderPlot({
    data <- subset(games, Year_Release >= input$slider_year[1] & Year_Release <= input$slider_year[2], select = c("Genre"))
    dat <- data.frame(table(data))
    dat$fraction <- dat$Freq / sum(dat$Freq)
    dat <- dat[order(dat$fraction), ]
    dat$ymax <- cumsum(dat$fraction)
    dat$ymin <- c(0, head(dat$ymax, n = -1))
    names(dat)[1] <- "Genre"
    library(ggplot2)
    ggplot(dat, aes(fill = Genre, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
      geom_rect(colour = "grey30") +
      coord_polar(theta = "y") +
      xlim(c(0, 4)) +  
      labs(fill = "Genre") +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill="snow"),
            panel.grid.minor = element_blank())
  })
  
  ####################################
  # Global Sales Scores of Top Games #
  ####################################
  output$sales_top_games <- renderPlot({
    data <- games %>% 
      select(Name, User_Score, Critic_Score, Global_Sales, Rating) %>%
      group_by(Name) %>%
      summarise(Total_Sales = sum(Global_Sales * 1000000 + 1), Avg_User_Score = mean(User_Score), 
                Avg_Critic_Score = mean(Critic_Score)) %>%
      arrange(desc(Total_Sales)) %>%
      head(input$bins)
    ggplot(data, aes(x = factor(Name, levels = Name))) +
      geom_bar(aes(y = Total_Sales/10000000), stat = "identity", fill = "steelblue") +
      geom_line(aes(y = Avg_User_Score, group = 1, colour = "Mean User")) +
      geom_point( aes(y = Avg_User_Score), shape = 21, fill = "#1c1c1c" ) +
      geom_line(aes(y = Avg_Critic_Score,  group = 1, colour = "Mean Critic")) +
      scale_color_manual(name = "Scores", values = c("Mean User" = "#1c1c1c", "Mean Critic" = "red")) +
      geom_point(aes(y = Avg_Critic_Score), shape = 21, fill = "red") + 
      labs(title = "Top Global Sales Game with Scores", x = "Games", y = "Total Sales") +  
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90),
            axis.text = element_text(size = 12),
            panel.background = element_rect(fill="snow"),
            panel.grid.minor = element_blank()) 
  })
  
  #################
  # Box Dashboard #
  #################
  output$nr_games <- renderValueBox({
    valueBox(
      paste0(nrow(games)), "Number of Games", icon = icon("calculator"),
      color = "blue"
    )
  })
  
  output$nr_genre <- renderValueBox({
    valueBox(
      paste0(length(genres)), "Number of Genres", icon = icon("calculator"),
      color = "blue"
    )
  })
  
  output$nr_platforms <- renderValueBox({
    valueBox(
      paste0(length(unique(games$Platform))), "Number of Platforms", icon = icon("calculator"),
      color = "blue"
    )
  })
  
  ###################
  # Games DataTable #
  ###################
  dt_resume <- subset(games, select = c("Name", "Platform", "Year_Release", "Genre", "Publisher", "Global_Sales",
                                        "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Critic_Score", "Critic_Count",
                                        "User_Score", "User_Count", "Developer", "Rating"))
  output$mytable = DT::renderDataTable({
    dt_resume
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
