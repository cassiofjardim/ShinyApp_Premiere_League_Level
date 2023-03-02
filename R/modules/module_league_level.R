
data_csv_file <- read.csv(file = 'www/data/main_league_df.csv')

#*************************************************
#**************** FIRST ROW INFO **************
#*************************************************
metrics <- c('MP','Pts', 'W', 'L', 'Age','Gls', 'xG', 'xGA', 'Poss', 'Ast', 'xA',
             'KP','Cmp_percent_Total','Cmp_percent_Short', 'Cmp_percent_Medium',
             'Cmp_percent_Long', 'PPA',   'Top.Team.Scorer')

definition <- c('Games','Pts', 'Win', 'Lost', 'Average Age','Goals', 'Exp. Goals(xG)', 'Exp. Goals Against(xGA)',
                'Ball Poss.', 'Assists', 'Exp. Assist.(xA)', 'Key Passings','Completed Passes (%)',
             'Short Passings (%)', 'Medium Passings (%)', 'Long Passings (%)', 'Passings - Pen. Area','Top Team Scorer')

#*************************************************
#**************** Tab Panel Metrics **************
#*************************************************

variables_vec <- c('xG_std','xGA_std', 'xGD.90_std', 'xG_Home_std', 'xG_Away_std',  
                   'xG_Per_Minutes_std', 'Won_percent_Aerial_Duels')

vector_titles <- c('xG - Standardized', 'xGA - Standardized', 'xGD.90 - Standardized', 'xG_Home - Standardized', 
                   'xG_Away - Standardized', 'xG Per. Minutes - Standardized', 'Aerials Duels Won (%)')

csvDownloadButton <-
  function(id,
           filename = "data.csv",
           label = "Download as CSV") {
    tags$button(
      style = ' 
    background: #C8102E; color: whitesmoke; font-weight: 500;
    padding: 0.5em; border-radius: 5px; border: none;',
      tagList(icon("download"), label),
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
    )
  }

module_league_UI <- function(id) {
  ns <- NS(id)
  tagList(div(
    class = 'league_level',
    
    # fileInput(
    #   ns('datafile'),
    #   'Choose CSV file',
    #   accept = c('csv', 'comma-separated-values', '.csv')
    # ),
    
    div(
      class = 'first_infos',
      
      uiOutput(outputId = ns('excel'))
    ),
    
    hr(class = 'horizontal_line'),
    
    div(
      class = 'chart1_table',
      div(
        style = 'display:flex; justify-content: space-between;',
        h6(
          'All Season Historical: Goals and Goals Against',
          tags$img(
            src = 'img/icons/goal.png',
            width = '14px',
            height = '14px'
          )
        ),
        h6(
          'All Season Historical: Standardized Goals (sd)',
          tags$img(
            src = 'img/icons/goal.png',
            width = '14px',
            height = '14px'
          )
        )
      ),
      
      div(class = 'left_chart',
          
          highchartOutput(ns('chart_1')),
          highchartOutput(ns('chart_n'))),
      
      h6(
        'TOP-10 Teams: 2020 SEASON Ranking',
        tags$img(
          src = 'img/ranking.png',
          width = '14px',
          height = '14px'
        )
      ),
      
      div(class = 'right_chart',
          
          div(
            class = 'comments',
            tags$ul(
              tags$li(
                tags$span('About Data:', style = 'font-weight:700'),
                'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.'
              ))
          ))
    ),
    
    div(
      class = 'tabpanels_row',
      
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      div(
        class = 'comments',
        
        h5(
          'GOALS, EXPECTED GOALS (xG), EXPECTED GOALS AGAINST (xGA), Aerials Duels Won(%)',
          tags$span('- vs. Statisticals Moments')
        ),
        h6('Variables are standardized')
      ),
      
    do.call(tabsetPanel,
      1:length(variables_vec) %>% map( ~
      tabPanel(
        title = tags$span(
        class = 'subtitle_main',
          tags$img(
          class = 'svg_icon',
          src = 'img/icons/goal.png',
          width = '14px',
          height = '14px'
          ),
          h6(variables_vec[.x],class = 'percent')
        ),
        
          highchartOutput(ns(paste0('chart_', .x+1)))
    
    ))),
      
      div(
        class = 'footer',
        tags$ul(
          tags$li(
            tags$span('About Data:', style = 'font-weight:700'),
            'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.'
          )
        )
      )
    ),
    
    div(
      class = 'metrics_table',
      
      h5(
        'Others Metrics',
        tags$span('- vs. Statisticals Moments')
      ),
      h6('Variables are standardized')
    ),
    
    div(class = 'others_metrics_table',
      reactableOutput(ns('table_1')),
      csvDownloadButton(ns("table_1"),
                        filename = "league_table.csv")
    )
    
  ))
}

module_league_Server <- function(id, app_control_input) {
  moduleServer(id,
               function(input,
                        output,
                        session) {
                 
#****************************************************************************
                 dataframe<-reactive({
                   if (is.null(input$datafile))
                     return(NULL)                
                   data<-read.csv(input$datafile$datapath)
                   
                   data
                 })
#****************************************************************************
                 
                 output$excel <- renderUI({
                   if(is.null(data_csv_file)){
                     
                   }else{
                     
                   tagList(
                     h1(
                       'Champion -',
                       style = 'font-size: 5em;',
                       tags$span(
                         paste(data_csv_file %>% filter(Rk == 1) %>% select(Squad) %>% pull),
                         style = glue::glue(
                           "color: {data_csv_file %>% filter(Rk == 1) %>% select(Cores) %>% pull};
                           font-weight: 900;"
                         )
                       ),
                       
                       tags$img(
                         src = paste0(
                           'img/2020/',
                           paste0(data_csv_file %>% filter(Rk == 1) %>% select(Squad) %>% pull),
                           '.png'
                         ),
                         width = '148px',
                         height = '148px'
                       )
                     ),
                     
                     div(
                       
                       class = 'champion_stats',
                       1:(length(metrics)-1) %>% map(~
                       h3(data_csv_file %>% filter(Rk == 1) %>% select(metrics[.x]) %>% pull ,
                          tags$br(),
                          tags$span(definition[.x])
                       )),
                       
                      h3(data_csv_file %>% filter(Rk == 1) %>% select(metrics[18]) %>% pull ,
                         class = 'top_scorer',
                          tags$img(src ='img/salah.jpeg',
                                   width = '88px', height = '88px', 
                                   style  ='float: right;border-radius:50%;margin: 0 auto;'),
                          tags$br(),
                          tags$span(definition[18])
                       )),
                     tags$ul(
                       tags$li(
                         tags$span('About Data:', style = 'font-weight:700'),
                         'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.'
                       )
                     )
                   )
                   
                   
                   }
                 })
#***********************************************************************************
                 output$chart_1 <- renderHighchart({
                   if(is.null(data_csv_file)){
                     
                   }else{
                   
                     stacked_bar_chart_function(df = data_csv_file,
                                                x_axis = 'Squad',
                                                y_axis = 'GF',y2_axis = 'GA')
                   }
                 })
#***********************************************************************************
                 output$chart_n <- renderHighchart({
                   
                   if(is.null(data_csv_file)){
                     
                   }else{
                   
                     scatter_chart_function(df = data_csv_file,
                                            metric = 'Gls_std',
                                            x_axis_title = 'Gls standard Dev.')
                     
                   }
                 })
#***********************************************************************************
                 output$table_1 <- renderReactable({
                   
                   if(is.null(data_csv_file)){
                     
                   }else{
                     table_style(dataframe = data_csv_file %>%
                                   select(Squad:Attendance)%>%
                                   arrange(Rk),width = 'auto')
                   }
                 })
#***********************************************************************************
                 output_chart_function <- function(x,vars,titles){
                   output[[paste0('chart_',x)]] <- renderHighchart({
                     
                     if(is.null(data_csv_file)){
                       
                     }else{
                     
                       scatter_chart_function(df = data_csv_file,
                                              metric = vars,
                                              x_axis_title = titles)
                         
                     }
                   })
                 }

                  map2(2:(length(variables_vec)+1),variables_vec,
                       ~ output_chart_function(.x,
                                               vars = .y,
                                               titles = str_replace(vector_titles[.x-1],'_',' ')))
                 
               })
}