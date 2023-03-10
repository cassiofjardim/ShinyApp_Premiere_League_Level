module_bottom_right_UI <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "grid-area: bottom_right_two",
      class = 'bottom_right_two',
      h5("Goals and Assists. - Standards Statistics",
         tags$img(src = 'img/icons/table.png', width = '14px', height = '14px',
                  style = 'border-raius:50%;')),

      div(

        class = 'shoots',
        tabsetPanel(
          tabPanel(
            title = tags$span(class = 'subtitle_main',
                              tags$img(src = 'img/icons/goal.png',
                                       width = '14px', height = '14px'),
                              h6('Goals and Assists.', class = 'percent',
                                 style = 'display: inline;
                              font-weight: 700;font-size: .75em;')),

            p(tags$span("Goals", style = 'color: #C8102E;font-weight:700;'),
               "and",
               tags$span("Assists.", style = 'color: #484848;font-weight:700;'),
               tags$img(src = 'img/exclamation.png', width = '18px', height = '18px')),

            highchartOutput(ns('chart_1'))
          ),

          tabPanel(
            id = 'text_pie',

            title = tags$span(class = 'subtitle_main',
                              tags$img(src = 'img/icons/goal.png',
                                       width = '14px', height = '14px'),
                              h6('GOALS + Ast.', class = 'percent',
                                 style = 'display: inline;
                              font-weight: 700;font-size: .75em;')),

            htmlOutput(ns('pie_text')),

            highchartOutput(ns('chart_2'))
          ),

          tabPanel(
            title = tags$span(class = 'subtitle_main',
                              tags$img(src = 'img/icons/goal.png',
                                       width = '14px', height = '14px'),
                              h6('Completed Passes', class = 'percent',
                                 style = 'display: inline;
                              font-weight: 700;font-size: .75em;')),

            h5("ATTEMPTED PASSES: Short - Medium - Long Passes ",
               style = 'padding: 1em;',
               tags$img(src = 'img/exclamation.png', width = '18px', height = '18px')),

            radioGroupButtons(
              inputId = ns("passes"),
              label = "Completed Passes",
              size = 'xs',
              choices = c("Short - Passes","Medium - Passes", "Long - Passes"),
              status = "primary",
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"),
                no = icon("xmark",
                          lib = "glyphicon")
              )
            ),

            highchartOutput(ns('chart_3'))
          ),

          tabPanel(
            title = tags$span(class = 'subtitle_main',
                              tags$img(src = 'img/icons/table.png',
                                       width = '14px', height = '14px'),
                              h6('Other Stats - Table', class = 'percent',
                                 style = 'display: inline;
                              font-weight: 700;font-size: .75em;')),

            div(class = 'main_table',
              style = 'display: flex; justify-content: center; margin: 1em auto;',
            reactableOutput(ns('table_1')))
          )),

            div(
              class = 'footer',
              p(tags$span('About Data:', style = 'font-weight:700'),
              'In publishing and graphic design, Lorem ipsum is a placeholder text
              commonly used to demonstrate the visual form of a document or a typeface
              without relying on meaningful content. Lorem ipsum may be used as a
              placeholder before final copy is available.')
            )
        )))
}

module_bottom_right_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$chart_1 <- renderHighchart({

        main_league_df %>%

          bar_chart_function(x_axis = 'Squad',
                             y_axis = 'Gls',
                             y2_axis = 'Ast',
                             # height = 250,
                             width = NULL,
                             main_color = team_color_champion)

      })
      #
      output$pie_text <- renderUI({

        div(
          class = 'text_pie',
          style = 'margin: 1em;',
          p(tags$span('About Data:', style = 'font-weight:700'),'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.')
        )

      })


      output$chart_2 <- renderHighchart({

        main_league_df %>%
          pie_chart_function(x_axis = 'Squad',
                             y_axis = 'Gls',
                             title_slice = 'Goals + Assist.',
                             slices_colors_list = colors %>% as.list())

      })

      observeEvent(input$passes,
                   {
                     if(input$passes == 'Short - Passes'){

                       output$chart_3 <- renderHighchart({

                        main_league_df %>%
                           bar_chart(x_axis = 'Squad',
                                     y_axis = 'Cmp_Short',
                                     main_color = team_color_champion,
                                     input = paste0(input$passes))

                       })%>% bindCache(input$passes)

                     }else{
                       if(input$passes == 'Medium - Passes'){

                         output$chart_3 <- renderHighchart({

                           main_league_df %>%
                             bar_chart(x_axis = 'Squad',
                                       y_axis = 'Cmp_Medium',
                                       main_color = team_color_champion,
                                       input = paste0(input$passes))

                         })%>% bindCache(input$passes)

                       }else{

                         output$chart_3 <- renderHighchart({

                           main_league_df %>%
                             bar_chart(x_axis = 'Squad',
                                       y_axis = 'Cmp_Long',
                                       main_color = team_color_champion,
                                       input = paste0(input$passes))

                         })%>% bindCache(input$passes)

                       }

                     }
                   })
      #
      output$table_1 <- renderReactable({


        table_style(main_league_df %>%
                    select(all_of(table_vector)),
                    width = '650', height = 'auto')

      })
    }


  )
}
