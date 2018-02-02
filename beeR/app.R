# load libraries
library(shiny)


# which fields are mandatory
fieldsMandatory = 
  c("userName",
    "beerName",
    "scoreHoppiness",
    "scoreBody",
    "scoreBalance",
    "scoreComplexity",
    "scoreCrispiness",
    "scoreHipsterness"
  )

# collate all field names to be exported
fieldsAll = 
  c("userName",
    "beerName",
    "scoreHoppiness",
    "scoreBody",
    "scoreBalance",
    "scoreComplexity",
    "scoreCrispiness",
    "scoreHipsterness",
    "userComment"
    )

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

# info for sharing this app on facebook/twitter
share <- list(
  title = "Mimicking a Google Form with a Shiny app",
  url = "http://daattali.com/shiny/mimic-google-form/",
  image = "http://daattali.com/shiny/img/mimic.png",
  description = "Learn how to create a Shiny app that allows users to submit responses to a form. Submissions get stored permanently and can be loaded back into the app.",
  twitter_user = "daattali"
)

  
# start shiny app
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(
  
  # UI
  # ~~~~~~~~
  ui = fluidPage(
    
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    
    # title
    titlePanel("The Beer RateR"),
    
    div(id = "header",
        h4("This app is powered by ",
           a(href = "https://shiny.rstudio.com",
             "Shiny")
        ),
        strong( 
          span("Created by "),
          a("Jan Knappe", href = "http://www.janknappe.com"),
          span("inspired by a blog post by"),
          a("Dean Attali", href = "http://deanattali.com"),
          HTML("&bull;"),
          span("Code on "),
          a("GitHub", href = "https://github.com/jknappe/shiny-server/")
          )
    ),
    
    DT::dataTableOutput("responsesTable"),
    
    fluidRow(
      column(6,
             div(
               id = "form",
               
               textInput("userName", "Your Name", ""),
               selectInput("beerName", "Which beer are you rating?",
                           c("", "Harper's: Golden Crown Ale", "O'Shea's: Spiced Winter Ale", "O'Hara's: Notorius IPA",
                             "St. Mel's: Autumn IPA", "Station Works: Foxes Rock Pale Ale", "Guinness Open: Gate Irish Wheat")
               ),
               sliderInput(inputId = "scoreHoppiness", 
                           label = "Hoppiness", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "scoreBody", 
                           label = "Body", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "scoreBalance", 
                           label = "Balance", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "scoreComplexity", 
                           label = "Complexity", 
                           min = 0,  max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "scoreCrispiness", 
                           label = "Crispiness", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "scoreHipsterness", 
                           label = "Overall Hipsterness", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               textInput("userComment", "General Comment"),
               actionButton("submit", "Submit Scoring", class = "btn-primary"),
               
               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               )
             ),
             
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response")
               )
             )
      )
    )
  ),
  
  # SERVER
  # ~~~~~~~~
  server = function(input, output, session) {
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })    
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    # display data table
    output$responsesTable <- DT::renderDataTable(
      loadData(),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    ) 
    
    
    
       
  }
)
