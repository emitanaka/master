library(shiny)
library(shinydashboard)
source("questions.R")

# Define UI ---
ui <- dashboardPage(
  dashboardHeader(
    title = "reading plots",
    tags$li(class = "dropdown", 
            actionLink("btn_export", 
                       span(icon("Save"), "Submit Survey"))),
    tags$li(class = "dropdown",
            a(href = "https://github.com/kaiwenjanet/master", target = "_blank", 
              span(icon("github"), "More Info")))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About you", tabName = "About_you", icon = icon("dashbord")),
      menuItem("Questions", tabName = "Questions", icon = icon("th")),
      menuItem("Thank you", tabName = "Thank_you", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItem(
      tabName = "About_you",
      box(
        title = "Demographics",
        questions$demographics,
        div(uiOutput("ui_d_save")),
        width = 12,
        status = "info",
        solidHeader = T,
        collapsible = T
      )
    ),
    
    tabItem(tabName = "Questions",
            includeScript("www/img_size.js"),
            includeCSS("www/taipan.css"),
            column(8,
                   box(
                     title = textOutput("out_img_info"),
                     div(class = "taipan_image_div",
                         imageOutput("out_image",
                                     inline = T)),
                     width = 12,
                     status = "primary",
                     collapsible = F
                   )),
            
            column(4, 
                   box(
                     title = "Questions",
                     questions$scene,
                     div(
                       uiOutput("ui_btn_next")
                     ),
                     width = 12,
                     status = "info",
                     solidHeader = T,
                     collapsible = T
                   ))),
    
    tabItem(tabName = "Thank_you",
            box(
              title = "Thank you",
              div(uiOutput("validation")),
              width = 12,
              status = "info",
              solidHeader = T,
              collapsible = T
            ))
  )
)

# Define server logic ---
server <- function(input, output){
  
  # load survey image
  image.list <- list.files(paste0("www/images"), full.names = T)
  image.list <- sample(image.list, length(image.list))
  
  v <- reactiveValues(
    imageNum = 1,
    responses = list(),
    start.time = Sys.time(),
    finish.time = Sys.time(),
    plot.order = 1,
    submitted = F
  )
  
  current_img <- reactive({
    image.list[v$imageNum]
  })
  
  output$out_image <- renderImage({
    list(src = current_img(), id = "taipan_current_img")
  }, deleteFile = F)
 
}

# Run app ---
shinyApp(ui = ui, server = server)
