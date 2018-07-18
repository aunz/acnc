# set working dir to this script
setwd(getSrcDirectory(function () {}))

# dependencies
library(data.table)
library(ggplot2)
library(plotly)
library(shiny)

# data
dt1 = fread('./data/data.csv')

ui = fluidPage(
  # h4('Title'),
  div(
    style = 'margin: 2rem',
    fluidRow(
      column(1,
        checkboxGroupInput('gender', span('Gender'), # D3
          choices = list(
            Male = 1,
            Female = 2,
            Other = 3
          )
          # , selected = c(1, 2)
        ),
        checkboxGroupInput('age', span('Age group'), # D1
          choices = list(
            '18 ~ 24' = 2,
            '25 ~ 34' = 3,
            '35 ~ 44' = 4,
            '45 ~ 54' = 5,
            '55 ~ 64' = 6,
            '65 ~ 74' = 7,
            '75 ~ 84' = 8,
            '85+' = 9
          )
          # , selected = 2:9
        ),
        checkboxGroupInput('country', span('Birth country'), # D7
          choices = list(
            'Australia' = 1,
            'United Kingdom' = 2,
            'New Zealand' = 3,
            'China' = 4,
            'India' = 5,
            'Vietnam' = 6,
            'Italy' = 7,
            'Philippines' = 8,
            'South Africa' = 9,
            'Malaysia' = 10,
            'Germany' = 11,
            'Other' = 97,
            'Prefer not to answer' = 99 # extra option not present in the report PDF
          )
        )
      ),
      column(2,
        checkboxGroupInput('family', span('Family situation'), # D5
          choices = list(
            'Married or in de facto relationship with dependent children' = 1,
            'Married or in de facto relationship, with NO dependent children' = 2,
            'Single with dependent children' = 3,
            'Single with NO dependent children' = 4
          )
          # , selected = 1:4
        ),
        checkboxGroupInput('work', span('Work status'), # D6
          choices = list(
            'Full time' = 1,
            'Part time' = 2,
            'Home duties' = 3,
            'Student' = 4,
            'Retired' = 5,
            'Unemployed' = 6,
            'Other' = 97 # extra option not present in the report PDF
          )
          # , selected = c(1:6, 97)
        ),
        checkboxGroupInput('income', span('Income'), # D7
          choices = list(
            'Less than $19,999' = 1,
            '$20,000 ~ $39,999' = 2,
            '$40,000 ~ $59,999' = 3,
            '$60,000 ~ $79,999' = 4,
            '$80,000 ~ $99,999' = 5,
            '$100,000 ~ $119,999' = 6,
            '$120,000 ~ $139,999' = 7,
            '$140,000 or over' = 8,
            'Prefer not to answer' = 99 # extra option not present in the report PDF
          )
          # , selected = c(1:8, 99)
        )
      ),
      column(9,
        mainPanel(
          width = '100%',
          plotlyOutput(outputId = 'plot', height = '560px')
        ),
        h3('Insight'),
        HTML('
          <li>The overall trust towards Australian charities is the <b>highest</b> compared to all other entities.</li>
          <li>People in the age group of 65+ are <b>more</b> likely to trust charities compared to those less than 65.</li>
          <li>The unemployed, people with low income or did not provide income and single parents with dependent children were <b>less</b> likely to trust charities. They are possibly the groups that need charitible assistance the most.</li>
        '),
        h4('Source: https://github.com/aunz/acnc')
      )
    )
  )
)

server = function (input, output) {
  dt1.m = melt(
    dt1,
    id.vars = c('D1', 'D3', 'D4_Overall', 'D4_State', 'D4_Location', 'D5', 'D6', 'D7', 'D8'),
    measure.vars = paste0('Q7_', 1:12)
  )
  
  levels(dt1.m$variable) = c(
    'Charities',
    'Doctors',
    'Police',
    'Religious\norganisations',
    'News\nmedia',
    'High\nCourt',
    'Reserve\nBank',
    'Local\ncouncil',
    'State\nParliament',
    'Federal\nParliament',
    'The ABC',
    'Australian\nTaxation\nOffice'
  )
  
  tmp0 = dt1.m[, .(m = mean(value), s = sd(value), .N), variable]

  dataInput = reactive({
    tmp = dt1.m

    v = input$gender; if (length(v) > 0) tmp = tmp[D3 %in% v]
    v = input$age; if (length(v) > 0) tmp = tmp[D1 %in% v]
    v = input$family; if (length(v) > 0) tmp = tmp[D5 %in% v]
    v = input$work; if (length(v) > 0) tmp = tmp[D6 %in% v]
    v = input$income; if (length(v) > 0) tmp = tmp[D7 %in% v]
    v = input$country; if (length(v) > 0) tmp = tmp[D8 %in% v]
    
    tmp
  })

  output$plot = renderPlotly({
    tmp = dataInput()
    tmp = tmp[, .(m = mean(value), s = sd(value), .N), variable]
    print(tmp)

    p0 = ggplot(mapping = aes(x = variable, y = m)) +
      scale_y_continuous('Mean trust score', breaks = 0:10, labels = 0:10, limits = c(0, 10), expand = c(0, 0)) +
      scale_x_discrete(paste('Entity, each N =', tmp$N[1])) + 
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = '#CCCCCC'),
        plot.title = element_text(hjust = 0.5, colour = '#001f3f', size = 20),
        text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 15))
      ) +
      ggtitle('Level of trust towards Australian charities and other entities')
    
    p = p0 +
      geom_bar(data = tmp, stat = 'identity', fill = I('#0074D9'), width = 0.75) +
      geom_point(data = tmp0, color = I('#7FDBFF'), size = I(3.75)) +
      annotate('text', x = 2, y = 9.5, label = 'Blue circle: mean score for all participants')
    
    p = ggplotly(p)
    p$x$layout$margin$b = 120 # increase the bottom margin
    p
  })
}

shinyApp(ui, server)