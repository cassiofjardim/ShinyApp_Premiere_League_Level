source('R/metrics_to_choose.R')



csvDownloadButton <-
  function(id,
           filename = "data.csv",
           label = "Download as CSV") {
    tags$button(
      style = '
    background: #C8102E; color: whitesmoke; font-weight: 500;
    margin-top: 0.75em; border-radius: 5px; border: none;',
      tagList(icon("download"), label),
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
    )
  }



module_league_UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
    class = 'league_level',
    div(

        h1('Champion -',
           tags$span(paste(champion_team),
                     style = glue::glue("color: {main_league_df %>% filter(Rk == 1) %>% select(Cores) %>% pull};font-weight: 900;")),
           tags$img(src = paste0('img/2020/',paste0(champion_team),'.png'),
                    width = '88px', height = '88px', style  ='float: right;')),

        div(class = 'champion_stats',
            1:(length(metrics_introduction_top) - 1) %>%
              map(~
                    h3(
                      main_league_df %>% filter(Rk == 1) %>% select(metrics_introduction_top[.x]) %>% pull ,
                      tags$br(),
                      tags$span(definition[.x])
                    )),

            h4(player_top_scorer,
               tags$img(src ='img/salah.jpeg',
                        width = '88px', height = '88px',
                        style  ='float: right;border-radius:50%;margin: 0 auto;'),
               tags$br(),
               tags$span('Top Team Scorer')
            ),
            p("About Data: In publishing and graphic design, Lorem ipsum is a
              placeholder text commonly used to demonstrate the visual form of
              a document or a typeface without relying on meaningful content.
              Lorem ipsum may be used as a placeholder before final copy is
              available."))),

    hr(class = 'horizontal_line'),

    div(
      class = 'chart_table',

      div(class = 'left_chart',style = 'flex: 1;',
          h6(
            glue::glue('All Seasons Historical: Goals and Goals Against'),
            tags$img(
              src = 'img/icons/goal.png',
              width = '14px',
              height = '14px'
            )
          ),
          highchartOutput(ns('chart_1'), width = 'auto', height = 250) ),


      div(class = 'right_chart',style = 'flex: 1;',
          h6(

            'TOP-10 Teams: 2020 SEASON Ranking',
            tags$img(
              src = 'img/ranking.png',
              width = '14px',
              height = '14px'
            )
          ),

          reactableOutput(ns('table_1'), width = 'auto', height = 250),
          csvDownloadButton(ns("table_1"),
                            filename = "league_table.csv")

          )),

    div(class = 'tabpanels_row',
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        div(
          class = 'comments',

          h5('GOALS, EXPECTED GOALS (xG) and EXPECTED GOALS AGAINST (xGA)',
          tags$span('- vs. Statisticals Moments')),
          h6('Variables are standardized')),
            do.call(tabsetPanel,
                    1:(length(tabpanel_championship_metrics)-1) %>% map(
                      ~
                        tabPanel(
                          title = tags$span(
                            class = 'subtitle_main',
                            tags$img(
                              class = 'svg_icon',
                              src = 'img/icons/goal.png',
                              width = '14px',
                              height = '14px'
                            ),
                            h6(tabpanel_championship_metrics[.x], class = 'percent')
                          ),

                          highchartOutput(ns(paste0('chart_', .x +
                                                      1)))))),

      div(
        class = 'footer',
        p(tags$span('About Data:', style = 'font-weight:700'),'In publishing and graphic design, Lorem ipsum is a placeholder text
          commonly used to demonstrate the visual form of a document or a typeface
          without relying on meaningful content. Lorem ipsum may be used as a
          placeholder before final copy is available.')
      )
    )

  )
  )
}

module_league_Server <- function(id, app_control_input) {
  moduleServer(id,
               function(input,
                        output,
                        session) {
#***********************************************************************************
                 output$chart_1 <- renderHighchart({
                   stacked_bar_chart_function(df = main_league_df,
                                              x_axis = squad_metric,
                                              y_axis = 'GF',
                                              y2_axis = 'GA',
                                              main_color = team_color_champion
                                              )
                 })
#***********************************************************************************
                 output$table_1 <- renderReactable({
                    table_style(dataframe = table_displayed,
                                sizing_default = 5,
                                page_size_opt_vec = 5)

                 })
# #***********************************************************************************
                 output_chart_function <- function(x,vars,titles){
                   output[[paste0('chart_',x)]] <- renderHighchart({

                     scatter_chart_function(df = main_league_df,
                                            metric = vars,
                                            x_axis_title = titles,
                                            main_color = team_color_champion)

                   })
                 }
                  map2(2:(length(tabpanel_championship_metrics)+1),tabpanel_championship_metrics,
                       ~ output_chart_function(.x,
                                               vars = .y,
                                               titles = str_replace(tabpanel_championship_titles[.x-1],'_',' ')))

               })
}
