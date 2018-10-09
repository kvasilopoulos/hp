library(shiny)
# library(semantic.dashboard)
library(shinydashboard)
library(DT)


# Load everything ---------------------------------------------------------

store <- c((items <- c("price", "income", "rent")),
           c("countries_accepted", "cv", "cv_seq"),
           paste0("summary_", items),
           paste0("datestamp_", items),
           paste0("estimation_", items),
           paste0("plot_", items),
           paste0("autoplot_", items))

path_store <- paste0("data/", store, ".rds")

for (i in seq_along(store)) assign(store[i], readRDS(file = path_store[i]))

# Sidebar -----------------------------------------------------------------



sidebar <- dashboardSidebar(
  sidebarMenu(
    hr(),
    sidebarMenu(id = "tabs",
                selectInput("country", "Country:", countries_accepted),
                menuItem("Analysis", tabName = "analysis", icon = icon("table")),
                menuItem("Plot", tabName = "plot", 
                         icon = icon("line-chart"), selected = TRUE),
                menuItem("Data",  icon = icon("file-text-o"),
                         menuItem("Raw", tabName = "pkmodel", 
                                  icon = icon("angle-right"),
                                  menuSubItem("Price", tabName = "price"),
                                  menuSubItem("Price over Income", tabName = "income"),
                                  menuSubItem("Price over Rent", tabName = "rent")
                                  ),
                         menuItem("Estimation", tabName = "estimation",
                                  icon = icon("angle-right"),
                                  menuSubItem("Price", tabName = "est_price"),
                                  menuSubItem("Price over Income", tabName = "est_income"),
                                  menuSubItem("Price over Rent", tabName = "est_rent")
                                  ),
                         menuItem("Critical Values", tabName = "crit_values",
                                  icon = icon("angle-right"),
                                  menuSubItem("Stat", tabName = "stat"),
                                  menuSubItem("Sequence", tabName = "sequence")
                         )
                ),
                menuItem("About", tabName = "about", icon = icon("question"))
                # selectInput("estimation", "Estimation", c("ARMA", "GARCH"))
                
                # tableOutput("data")
    )
  )
)



# Body --------------------------------------------------------------------


body <- dashboardBody(
  tabItems(#selected = 1,
    tabItem(tabName = "analysis",
            fluidRow(
              box(
                title = "Real House Price",
                tableOutput("table11"),
                width = 4
              ),
              box(
                title = "House Price to Disposable Income", 
                tableOutput("table12"),
                width = 4
              ),
              box(
                title = "House Price to Rent", 
                tableOutput("table13"),
                width = 4
              )
            ),
            fluidRow(
              box(
                title = "Datestamp House Price",
                dataTableOutput("table21"),
                width = 4
              ),
              box(
                title = "Datestamp House Price to Income", 
                dataTableOutput("table22"),
                width = 4
              ),
              box(
                title = "Datestamp House Price to Rent", 
                dataTableOutput("table23"),
                width = 4
              )
            )
    ),
    tabItem(tabName = "plot",
            fluidRow(
              box(
                title = "Real House Price Index",
                plotOutput("plot1"),
                width = 4
              ),
              box(
                title = "House Price to Disposable Income", 
                plotOutput("plot2"),
                width = 4
              ),
              box(
                title = "House Price to Rent", 
                plotOutput("plot3"),
                width = 4
              )
            ),
            fluidRow(
              box(
                title = "Bubble for House Price",
                plotOutput("autoplot1"),
                width = 4
              ),
              box(
                title = "Bubble for House Price to  Income", 
                plotOutput("autoplot2"),
                width = 4
              ),
              box(
                title = "Bubble for House Price to Rent", 
                plotOutput("autoplot3"),
                width = 4
              )
            )
    ), 
    
    
    # Data --------------------------------------------------------------------
    
    ### Raw
    
    tabItem(tabName = "price",
            dataTableOutput("data_price")
    ),
    tabItem(tabName = "income",
            dataTableOutput("data_income")
    ),
    tabItem(tabName = "rent",
            dataTableOutput("data_rent")
    ),
    
    ### Estimation
    
    tabItem(tabName = "est_price",
            dataTableOutput("estimation_price")
    ),
    tabItem(tabName = "est_income",
            dataTableOutput("estimation_income")
    ),
    tabItem(tabName = "est_rent",
            dataTableOutput("estimation_rent")
    ),
    
    ### Critical Values
    
    tabItem(tabName = "stat",
            dataTableOutput("cv")
    ),
    tabItem(tabName = "sequence",
            dataTableOutput("cv_seq")
    ),
    
    
    # About ------------------------------------------------------------------
    
    
    tabItem(tabName = "about",
            includeMarkdown("./about/about.Rmd")
    )
  )
)

# ui ----------------------------------------------------------------------



ui <- dashboardPage(#skin = "blue",
  dashboardHeader(title = "Exuberance Analysis"),
  sidebar, 
  body)


# server ------------------------------------------------------------------



server <- function(input, output, session) {
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", tabName = "dashboard", icon = icon("calendar"))
    )
  })
  
  ### Tables - analysis section
  
  output$table11 <- renderTable({summary_price[[input$country]][[1]]},
                                striped = TRUE, bordered = TRUE,  
                                width = '100%', rownames = TRUE,
                                align = 'ccccc')
  output$table12 <- renderTable({summary_income[[input$country]][[1]]},
                                striped = TRUE, bordered = TRUE,  
                                width = '100%', rownames = TRUE,
                                align = 'ccccc')
  output$table13 <- renderTable({summary_rent[[input$country]][[1]]},
                                striped = TRUE, bordered = TRUE,  
                                width = '100%', rownames = TRUE,
                                align = 'ccccc')
  
  output$table21 <- renderDataTable({datestamp_price[[input$country]][[1]]},
                                    options = list(searching = FALSE,
                                                   # bPaginate=FALSE,
                                                   ordering = FALSE,
                                                   dom = "t"))
  output$table22 <- renderDataTable({datestamp_income[[input$country]][[1]]},
                                    options = list(searching = FALSE,
                                                   # bPaginate=FALSE,
                                                   ordering = FALSE,
                                                   dom = "t"))
  output$table23 <- renderDataTable({datestamp_income[[input$country]][[1]]},
                                    options = list(searching = FALSE,
                                                   # bPaginate=FALSE,
                                                   ordering = FALSE,
                                                   dom = "t"))
  
  
  ### Plots - plot section
  
  output$plot1 <- renderPlot({plot_price[[input$country]]})
  
  output$plot2 <- renderPlot({plot_income[[input$country]]})
  
  output$plot3 <- renderPlot({plot_rent[[input$country]]})
  
  # autoplot of exuber
  
  output$autoplot1 <- renderPlot({autoplot_price[[input$country]]})
  
  output$autoplot2 <- renderPlot({autoplot_income[[input$country]]})
  
  output$autoplot3 <- renderPlot({autoplot_rent[[input$country]]})
  
  
  fun_DT <- function(x, cap, str_name) {
    datatable(x,
              caption = cap,
              extensions = c('Buttons'),
              options = list(dom = 'Bfrtip',
                             # autoWidth = TRUE,
                             scrollX = TRUE,
                             fixedColumns = TRUE,
                             # scroller = TRUE,
                             pageLength = nrow(x),
                             searching = FALSE,
                             paging = FALSE,
                             buttons = 
                               list(list(
                                 extend = 'colvis', 
                                 columns = c(1:NCOL(x))),
                                 list(
                                   extend = "collection", 
                                   buttons = list(list(extend = 'csv',
                                                       filename = str_name),
                                                  list(extend = 'excel',
                                                       filename = str_name)),
                                   text = "Download"
                                 ))
              ) 
              )
    
  }
  
  
  ### Data section
  
  # Raw
  
  output$data_price <- DT::renderDataTable({
    fun_DT(price, 'Real House Price Index for all OECD countries', "rhp") %>% 
      formatRound(2:NCOL(price), 3) 
    })
  
  
  output$data_income <- renderDataTable({
    fun_DT(income, 'House Price to Disposable Income for all OECD countries', "hpi_rpi") %>% 
      formatRound(2:NCOL(income), 3) 
    })
  
  output$data_rent <- renderDataTable({
    fun_DT(rent, 'House Price to Rent Income for all OECD countries', "hpi_ydh") %>% 
      formatRound(2:NCOL(income), 3) 
  })
  
  ### Estimation
  
  output$estimation_price <- renderDataTable({
    fun_DT(estimation_price, 'BSADF sequence for House Price', "bsadf_rhp") %>% 
      formatRound(2:NCOL(price), 3) 
  })
  
  output$estimation_income <- renderDataTable({
    fun_DT(estimation_income, 'BSADF sequence for House Price to Rent', "bsadf_rhp") %>% 
      formatRound(2:NCOL(price), 3) 
    })
  
  output$estimation_rent <- renderDataTable({
    fun_DT(estimation_rent, 'BSADF sequence for House Price to Income', "bsadf_rhp") %>% 
      formatRound(2:NCOL(price), 3) 
  })
  
  # Critical Values
  
  output$cv <- renderDataTable({
    fun_DT(cv, 'Critical values', "cv") %>% 
      formatRound(1:NCOL(cv), 3) 
  })
  
  output$cv_seq <- renderDataTable({
    fun_DT(cv_seq, 'Critical values sequence', "cv_seq") %>% 
      formatRound(1:NCOL(cv_seq), 3) 
    })
  
}


# Launch ------------------------------------------------------------------

shinyApp(ui, server)

