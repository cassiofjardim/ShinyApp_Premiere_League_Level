
metrics_vector <- metrics %>% unlist(use.names = F)

main_df <- main_league_df %>% select(Squad, id, all_of(metrics_vector))

module_top_right_UI <- function(id) {
  ns <- NS(id)
  tagList(div(
    class = 'top_right',
    
    h5("Others Stats"),
    
    
    div(
      class = 'panels_controls',
      pickerInput(
        inputId = ns("categories"),
        width = '200px',
        label = "Metrics Categories",
        choices = tabpanels_top_module
      ),
      
      checkboxGroupButtons(
        inputId = ns("metrics"),
        label = "Choices (Max. 4 Choices):",
        width = '100%',
        choices = metrics$standard,
        selected = "",
        status = "success"
      )
    ),
    uiOutput(ns('gpt')),
    uiOutput(ns('tabset_panel'))
  ))
}

module_top_right_Server <- function(id, app_control_input) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 observeEvent(input$categories,
                              {
                                if (input$categories == 'Standard League - Stats') {
                                  updateCheckboxGroupButtons(
                                    inputId = 'metrics',
                                    choices = metrics$standard,
                                    selected = metrics$standard[1],
                                    status = "success"
                                  )
                                } else{
                                  if (input$categories == 'Passing League - Stats') {
                                    updateCheckboxGroupButtons(
                                      inputId = 'metrics',
                                      choices = metrics$passing,
                                      selected = metrics$passing[1],
                                      status = "success"
                                    )
                                  } else{
                                    if (input$categories == 'Possession League - Stats') {
                                      updateCheckboxGroupButtons(
                                        inputId = 'metrics',
                                        choices = metrics$possession,
                                        selected = metrics$possession[1],
                                        status = "success"
                                      )
                                    } else{
                                      updateCheckboxGroupButtons(
                                        inputId = 'metrics',
                                        choices = metrics$misc,
                                        selected = metrics$misc[1],
                                        status = "success"
                                      )
                                    }
                                  }
                                  
                                }
                                
                              })
                 
                 output$tabset_panel <- renderUI({
                   mainPanel(class  = 'chart',
                             style  ='width:100%;',
                             
                             do.call(tabsetPanel,
                                     c(if (length(input$metrics) == 0) {
                                       list()
                                       
                                     } else{
                                       input$metrics %>%
                                         map( ~ tabPanel(
                                           class = 'tabpanel_choices',
                                           title = paste0(.x),
                                           div(
                                             scatter_chart_function(
                                               df = main_df,
                                               metric = .x,
                                               x_axis_title = .x,
                                               main_color = team_color_champion
                                             ) %>%
                                               hc_size(height = 400)
                                           )
                                           
                                         ))
                                     },
                                     id = ns("paineis"))))
                 }) %>%
                   bindCache(input$metrics, cache = "app")
                 
                 
                 metrics_react <- reactive({
                   input$paineis
                 })
                 #
                 output$gpt <- renderUI({
                   div(
                     class  = 'comment_div',
                     tags$img(
                       src = 'img/chatgpt-icon.png',
                       width = '48px',
                       height = '48px'
                     ),
                     
                     h2(input$paineis, ':',
                        list_db_definition[input$paineis],
                        
                        style = 'margin-left:.5em;'),
                     
                     div(p(class = 'slide-out', list_db_concept[input$paineis]))
                     
                   )})
                 
               })
}
