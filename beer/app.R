# load libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(googlesheets)

options(shiny.sanitize.errors = FALSE)


# create list of available categories
categoryList = c("Hoppiness", "Body", "Balance", "Complexity", "Crispiness", "Hipsterness")

# create list of available categories
fieldsMandatory = c("Name", "Beer", categoryList)

# collate all field names to be exported
fieldsAll = c(fieldsMandatory, "Comment")

# create list of available beers
typeList = c("Ale", "IPA", "Lager/Pilsner", "Stout/Porter", "Wheat", "Malt", "Other")

# create list of available categories
categoryList = c("Hoppiness", "Body", "Balance", "Complexity", "Crispiness", "Hipsterness")


# # save the results to a file
# saveData = function(data) {
#   fileName = file.path("responses", sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data)))
#   write.csv(x = data, file = fileName, row.names = FALSE)
# }
# 
# # load all responses into a data.frame
# loadData = function() {
#   data = 
#     list.files(file.path("responses"), full.names = TRUE) %>%
#     lapply(function(x) read_csv(x, col_names = TRUE, col_types = cols(.default = col_guess())) ) %>%
#     bind_rows(.) 
#   data
# }

# GOOGLESHEETS

table = "beer"

saveData = function(data) {
  # Grab the Google Sheet
  sheet = gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData = function() {
  # Grab the Google Sheet
  sheet = gs_title(table)
  # Read the data
  gs_read_csv(sheet)
}


# start shiny app
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(
  
  # UI
  # ~~~~~~~~
  ui = fluidPage(
    
    shinyjs::useShinyjs(),
    
    # title
    titlePanel("The Beer RateR"),
    
    
    div(id = "header",
        strong( 
          span("Code on "),
          a("GitHub", href = "https://github.com/jknappe/shiny-server/")
        )
    ),
    
    # fluid row, columns should add to 12
    fluidRow(
      
      # first column
      column(4,
             div(
               id = "form",
               
               h4("Submit a new rating:"),
               # Input fields
               textInput("Name", "Your Name", ""),
               textInput("Beer", "Which beer are you rating?", ""),
               selectInput("Type", "What type of beer is this?",
                           c("", typeList)),
               sliderInput(inputId = "Hoppiness", label = "Hoppiness", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "Body", label = "Body", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "Balance", label = "Balance", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "Complexity", label = "Complexity", 
                           min = 0,  max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "Crispiness", label = "Crispiness", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               sliderInput(inputId = "Hipsterness", label = "Overall Hipsterness", 
                           min = 0, max = 5, value = 0, step = 0.5, ticks = FALSE),
               textInput("Comment", "General Comment"),
               actionButton("submit", "Submit Scoring", class = "btn-primary"),
               
               shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                               div(id = "error", div(br(), tags$b("Error: "), span(id = "error_msg"))))
             ),
             
             shinyjs::hidden(div(id = "thankyou_msg", h3("Thanks, your response was submitted successfully!"),
                                 actionLink("submit_another", "Submit another response")))
      ),
      
      # second column
      column(8,
             h4("Browse the results:"),
             tabsetPanel(
               tabPanel("by type",
                        selectInput(inputId = "selectType", label = "Select type", choices = typeList), 
                        plotOutput("byType")
               ), 
               tabPanel("by category",
                        selectInput(inputId = "selectCategory", label = "Select category", choices = categoryList),  
                        plotOutput("byCategory")), 
               tabPanel("Comments", tableOutput("comments"))
             )
      )
      
      
      
    ) # close fluidRow
  ), # close fluidPage
  
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
    formData = reactive({
      data = t(c(input$Name, input$Beer, input$Type, input$Hoppiness, input$Body, input$Balance, input$Complexity, input$Crispiness, input$Hipsterness, input$Comment))
      # data = sapply(fieldsAll, function(x) input[[x]])
      # data = c(data)
      # data = t(data)
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
    
    # PLOTS ----
    #~~~~~~~~~~~
    
    # .summary ----
    
    
    # .output$byBeer ----
    # ~~~~~~~~
     output$byBeer =  renderPlot( {
        ggplot(as_tibble(mutate(loadData(), Beer = factor(Beer))), aes(x = Beer, y = Hoppiness)) +
          geom_bar(stat = "identity")
         # summaryData %>%
         #   tidyr::gather(.,
         #        key = "Category",
         #        value = "Score",
         #        Hoppiness, Body, Balance, Complexity, Crispiness, Hipsterness
         # ) %>%
         # dplyr::filter(.,
         #        Beer %in% input$selectBeer
         #        ) %>%
         # ggplot(., aes(x = Category, y = Score)) +
         #   geom_boxplot() +
         #   ylim(0, 5) +
         #   theme_minimal() +
         #   ggtitle("Average scores for ", input$selectBeer)
        } 
       )
    # ~~~~~~~~
    
    
    
    # # .output$byCategory ----
    # # ~~~~~~~~
    # output$byCategory =
    #   renderPlot({
    #     
    #     summaryData %>%
    #     select(.,
    #            Beer,
    #            input$selectCategory
    #            ) %>%
    #     group_by(.,
    #              Beer
    #              ) %>%
    #     summary(.,
    #             Average = mean(input$selectCategory)
    #             ) %>%
    #     ggplot(., aes(x = input$selectCategory, y = Score)) +
    #       geom_bar(stat = "identity") +
    #       ylim(0, 5) +
    #       theme_minimal() +
    #       ggtitle("Beers by ", input$selectCategory)
    #     
    #   })
    # ~~~~~~~~
    
  }
)
