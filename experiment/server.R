library(shiny)
library(shinydashboard)
library(googlesheets4)
library(purrr)
questions <- readRDS("data/questions.Rds")

first <- sample(1:nrow(t$sketch),1)
second <- sample(1:nrow(t$sketch[-1,-first]),1)
third <- sample(1:nrow(t$sketch[-1,-first][-1, -second]),1)
if (t$sketch[-1,-first][1, second] 
    != t$sketch[1,first]) t$sketch[-1,-first][1, second]
if (!(t$sketch[-1,-first][-1, -second][1,] 
      %in% c(t$sketch[1,first], t$sketch[-1,-first][1, second])
) == rep(TRUE, nrow(t$sketch[-1,-first][-1, -second])))
{t$sketch[-1,-first][-1, -second][1, third]}

t$sketch[-1,-first][-1, -second][-1, -third]

book <- as_tibble(t$book)
book[book$row == 4 & book$variates == t$sketch[-1,-first][-1, -second][-1, -third],]$plots
book[book$row == 3 & book$variates == t$sketch[-1,-first][-1, -second][1, third],]$plots
book[book$row == 2 & book$variates == t$sketch[-1,-first][1, second],]$plots
book[book$row == 1 & book$variates == t$sketch[1,first],]$plots


getInputID <- function(input){
  if(!inherits(input, "shiny.tag")){
    return()
  }
  c(
    if(!is.null(input$attribs$id)){list(list(id=input$attribs$id, type = input$name))}else{NULL},
    do.call("c", map(input$children, getInputID))
  )
}

# Define server logic ---
shinyServer(
  function(input, output){
    
    # check connection to google sheet
    
    sheet <- tryCatch({
      gs4_auth("data/authentication.rds")
      sheet <- gs4_get("1JcmIB5dOi7qkArfGawTkOIVFyKMOU-xxHZRuV_7Kzlw")
    }, error = function(e){
      message("Access has not been granted, please try again in 5 minutes.")
      return(NULL)
    })
    
    # unique identifier function
    create_unique_id <- function(char_len = 7){
      set.seed(Sys.time())
      
      pool <- c(letters, LETTERS, 0:9)
      
      this_res <- paste0(sample(pool, char_len, replace = T), collapse = "")
      this_res
    }
    
    identifier <- create_unique_id()
    
    # load survey image
    image.list <- list.files(paste0("www/images/autism/Version", 1:4, sep = ""), full.names = T)
    #image.list <- list.files(paste0("www/images"), full.names = T)
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
  
    output$out_img <- renderImage({
      list(src = current_img(), id = "taipan_current_img")
    }, deleteFile = F)
  
  }
)



# Run app ---
