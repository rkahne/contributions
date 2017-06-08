library(anytime)
library(lubridate)
library(stringr)
library(scales)
library(treemap)
library(forcats)
library(shinydashboard)
library(shiny)
library(markdown)
library(tidyverse)

contributions <- read_csv('contribution_data_for_app.csv')

get_candidates <- function(year, office){
  (contributions %>% filter(election_year == year, `Office Sought` == office))$full_name %>% unique() %>% sort()
}

get_offices <- function(year){
  (contributions %>% filter(election_year == year))$`Office Sought` %>% unique()
}

get_frame <- function(candidate, year, type, in_kind){
  if(in_kind == F) kind <- c('MONETARY')
  else kind <- c('MONETARY', 'IN KIND')
  contributions %>% 
    mutate(primary_transfer = map_lgl(Name, function(i) str_detect(i, 'PRIMARY'))) %>%
    filter(full_name == candidate, 
           election_year == year, 
           election_type %in% type, 
           `Form of Transaction` %in% kind, primary_transfer == F)
}

ui <- dashboardPage(
  dashboardHeader(title = 'Contributions Dashboards'),
  dashboardSidebar(
    selectInput('plot_year', 'Year:', c(2014,2015,2016), selected = 2016),
    checkboxInput('in_kind', 'Include In-Kind Contributions?', value = T),
    uiOutput('office'),
    uiOutput('candidates'),
    checkboxGroupInput('election_type', 'Include These Types of Elections:', 
                       list('General', 'Primary', 'Special'), selected = c('General', 'Primary', 'Special'))
  ),
  dashboardBody(
    fluidRow(
      box(uiOutput('candidate_name'),
          uiOutput('party'),
          uiOutput('office_location'),
          uiOutput('raised'), width = 12, align = 'center')
    ),
    fluidRow(
      box(title = 'Average Contributions', status = 'primary', solidHeader = T,
          dataTableOutput('mm_table'),
          dataTableOutput('num_contrib'),
          includeMarkdown('violin.md')),
      box(title = 'Violin Plot of Contributions', status = 'primary', solidHeader = T,
          plotOutput('violin'))
    ),
    fluidRow(
      box(title = 'Treemap of Contributors', status = 'primary', solidHeader = T,
          plotOutput('tree')),
      box(title = 'Table of Contributors', status = 'primary', solidHeader = T,
          dataTableOutput('c_table'))
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    fluidRow(
      box(includeMarkdown('explain.md'), width = 12)
    )
  )
)

server <- function(input, output){
  df <- reactive({get_frame(input$candidate, input$plot_year, input$election_type, input$in_kind)})
  output$raised <- renderUI({
    total_raised <- df()$`Transaction Amount` %>% 
      sum() %>% 
      dollar_format()()
    h3(paste0('Total Raised: ', total_raised), align = 'center')
  })
  output$candidate_name <- renderUI({ input$candidate %>% h1(align = 'center') })
  output$party <- renderUI({ h2(df()$`Party Affiliation`[1]) })
  output$office_location <- renderUI({ 
    h2(paste0(df()$`Office Sought`[1],' - ',
              df()$`Location of Office`[1]),
       align = 'center') 
  })
  output$office <- renderUI({
    office <- get_offices(input$plot_year)
    selectInput('office', 'Choose Office Sought', office, selected = office[1])
  })
  output$candidates <- renderUI({
    candidates <- get_candidates(input$plot_year, input$office)
    selectInput('candidate', 'Choose Candidate', candidates, selected = candidates[1])
  })
  output$mm_table <- renderDataTable({
    bind_cols(
      df() %>% 
        summarize(`Mean Transaction Amount` = dollar_format()(mean(`Transaction Amount`))),
      df() %>% 
        summarize(`Median Transaction Amount` = dollar_format()(median(`Transaction Amount`)))
    )
  }, options = list(dom = 't', searching = F, columnDefs = list(list(width = '200px', targets = "_all"))
))
  output$num_contrib <- renderDataTable({
    bind_cols(
      df() %>% 
        summarize(`Number of Transactions` = n()),
      df() %>% 
        group_by(Name, `Form of Transaction`) %>%
        summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
        ungroup() %>% 
        summarize(`Unique Donors` = n())
    )
  }, options = list(dom = 't', searching = F, columnDefs = list(list(width = '200px', targets = "_all"))))
  output$violin <- renderPlot({
    df() %>% 
      group_by(Name, `Organization Name`) %>%
      summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
      arrange(`Total Given` %>% desc()) %>% 
      ggplot(aes(x= 1, y =`Total Given`, fill = 'A')) + 
      geom_violin(show.legend = F) +
      geom_boxplot(aes(fill = 'B', alpha = 0.6), width = 0.1, show.legend = F) +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_manual(values = c('dodgerblue', 'grey')) +
      labs(x = '') +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      coord_flip()
  })
  output$tree <- renderPlot({
    if(df() %>% 
       group_by(Name, `Form of Transaction`) %>%
       summarize(`Total Given` = sum(`Transaction Amount`)) %>%
       nrow() > 1000) NA
    else df() %>%
      group_by(Name, `Organization Name`, `Form of Transaction`) %>%
      summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
      filter(`Total Given` >= 0) %>% 
      treemap(index = 'Name', vSize = 'Total Given')
  })
  output$c_table <- renderDataTable({
    df() %>%
      group_by(Name, `Form of Transaction`) %>%
      summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
      arrange(desc(`Total Given`)) %>% 
      mutate(`Total Given` = dollar_format()(`Total Given`))
  }, options = list(dom = 'tp'))
}

shinyApp(ui,server)
