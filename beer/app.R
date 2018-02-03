# load libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(magrittr)


# create list of available categories
categoryList = c("Hoppiness", "Body", "Balance", "Complexity", "Crispiness", "Hipsterness")

# create list of available categories
fieldsMandatory = c("Name", "Beer", categoryList)

# collate all field names to be exported
fieldsAll = c(fieldsMandatory, "Comment")

# create list of available beers
beerList = c("Harper's: Golden Crown Ale", "O'Shea's: Spiced Winter Ale", "O'Hara's: Notorius IPA",
             "St. Mel's: Autumn IPA", "Station Works: Foxes Rock Pale Ale", "Guinness Open: Gate Irish Wheat")

# create list of available categories
categoryList = c("Hoppiness", "Body", "Balance", "Complexity", "Crispiness", "Hipsterness")


# save the results to a file
saveData = function(data) {
  fileName = sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  write.csv(x = data, file = file.path("responses", fileName), row.names = FALSE)
}

# load all responses into a data.frame
loadData = function() {
  data = 
    list.files(file.path("responses"), full.names = TRUE) %>%
    lapply(function(x) read_csv(x, col_names = TRUE, col_types = cols(.default = col_guess())) ) %>%
    bind_rows(.) 
  data
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
               selectInput("Beer", "Which beer are you rating?",
                           c("", beerList)),
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
               tabPanel("by beer",
                        selectInput(inputId = "selectBeer", label = "Select beer", choices = beerList), 
                        plotOutput("byBeer")
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
      data = sapply(fieldsAll, function(x) input[[x]])
      data = c(data, timestamp = as.integer(Sys.time()))
      data = t(data)
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
     output$byBeer =
      renderPlot({
        
        df <- data.frame(dose=c("D0.5", "D1", "D2"),
                         len=c(4.2, 10, 29.5))
        
        ggplot(data=df, aes(x=dose, y=len, group=1)) +
          geom_line()+
          geom_point()
        
    #     summaryData =
    #       loadData() %>%
    #       mutate(.,
    #              Beer = factor(Beer),
    #              Hoppiness = as.numeric(Hoppiness),
    #              Body = as.numeric(Body),
    #              Balance = as.numeric(Balance),
    #              Complexity = as.numeric(Complexity),
    #              Crispiness = as.numeric(Crispiness),
    #              Hipsterness = as.numeric(Hipsterness),
    #              Comment = as.character(Comment)
    #       )  %>%
    #       as_tibble()
    #     summaryData %>%
    #       tidyr::gather(.,
    #            key = "Category",
    #            value = "Score",
    #            Hoppiness, Body, Balance, Complexity, Crispiness, Hipsterness
    #     ) %>%
    #     dplyr::filter(.,
    #            Beer %in% input$selectBeer
    #            ) %>%
    #     ggplot(., aes(x = Category, y = Score)) +
    #       geom_boxplot() +
    #       ylim(0, 5) +
    #       theme_minimal() +
    #       ggtitle("Average scores for ", input$selectBeer)
       })
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
