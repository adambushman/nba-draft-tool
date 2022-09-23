library('shiny')
library('shinyWidgets')
library('DT')
library('tidyverse')
library('lubridate')

# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  setBackgroundImage(
    src = "background-05.jpg"
  ),

  fluidRow(
    column(width = 12,
           fluidRow(
             column(width = 12,
                    align = "center",
                    h1("Utah Jazz Asset Draft 2022"),
                    h4("Draft Reps: Riley G. (@rgiss11), McCade P. (@McCadeP8)"),
                    h4("Host: Adam B. (@adam_bushman)"),
                    h4("Sponsor: Jabber Jazz (@jabber_jazz)"),
                    br())
           )),
    column(
      width = 12,
      fluidRow(
        column(3),
        column(
          width = 2,
          align = "center",
          selectInput("drafter_in", "On the Clock", c("McCade P.", "Riley G.")),
          actionButton("select_in", "Make Selection")
        ),
        column(
          width = 2,
          align = "center",
          textOutput('timeleft'),
          tags$head(tags$style("#timeleft{color:#f6ee26; font-size: 65px;}")),
          actionButton('start_in', 'Start')
        ),
        column(
          width = 2,
          align = "center",
          selectInput('time_in', 'Timer (sec)', c(60, 30)),
          actionButton('reset_in', 'Reset')
        ),
        column(3)
      ),

      br(),
      br(),

      fluidRow(
        column(1),
        column(
          width = 2,
          align = "center",
          DTOutput("playersT")
        ),
        column(
          width = 2,
          align = "center",
          DTOutput("picksT")
        ),
        column(
          width = 2,
          align = "center",
          DTOutput("otherT")
        ),
        column(
          width = 4,
          align = "center",
          DTOutput("selectionsT")
        ),
        column(1)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Time settings
  timer <- reactiveVal(60)
  active <- reactiveVal(FALSE)

  output$timeleft <- renderText({
    as.character(timer())
  })

  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "The Pick Is In!",
            size = "s"
          ))
        }
      }
    })
  })

  observeEvent(input$start_in, {active(TRUE)})
  observeEvent(input$reset_in, {active(FALSE)})
  observeEvent(input$reset_in, {timer(as.integer(input$time_in))})


  # Reactable Tables
  players <- reactiveValues(
    data = data.frame(
      name = c('Stanley Johnson', 'Mike Conley', 'Malik Beasley', 'Jarred Vanderbilt', 'Jared Butler', 'Nickeil Alexander-Walker', 'Udoka Azubuike', 'Leandro Bolmaro', 'Talen Horton-Tucker', 'Rudy Gay', 'Jordan Clarkson', 'Lauri Markkanen', 'Simone Fontecchio', 'Walker Kessler', 'Ochai Agbaji', 'Collin Sexton', 'Saben Lee', 'Kelly Olynyk')
    )
  )

  picks <- reactiveValues(
    data = data.frame(
      name = c('U-1st | 2023 | UTA', 'U-1st | 2023 | MIN', 'P-1st | 2023 | BKN', 'O-1st | 2024 | UTA', 'O-1st | 2025 | UTA', 'U-1st | 2025 | CLE', 'U-1st | 2025 | MIN', 'U-2nd | 2025 | UTA', 'SO-1st | 2026 | UTA, MIN, CLE', 'P-2nd | 2026 | MEM', 'U-2nd | 2026 | UTA', 'U-1st | 2027 | UTA', 'U-1st | 2027 | MIN', 'U-1st | 2027 | CLE', 'S-1st | 2028 | UTA, CLE', 'U-1st | 2029 | UTA', 'U-1st | 2029 | CLE', 'P-1st | 2029 | MIN', 'U-2nd | 2029 | UTA')
    )
  )

  other <- reactiveValues(
    data = data.frame(
      name = c('TE - Joe Ingles', 'TE - Rudy Gobert', 'CS - 2022', 'CS - 2023', 'CS - 2024', 'CS - 2025', 'CS - 2026', 'CS - 2027', 'CS - 2028', 'CS - 2029')
    )
  )

  selections <- reactiveValues(
    data = data.frame(
      name = c("test"),
      selection = c("test"),
      type = c("test")
    )
  )


  # Table Update Logic
      observeEvent(input$select_in, {
        if(!is.null(input$playersT_rows_selected)) {
          selections$data <<- rbind(selections$data, c(input$drafter_in, players$data[input$playersT_rows_selected, 1], "Player"))
        }
      })

      observeEvent(input$select_in, {
        if(!is.null(input$playersT_rows_selected)) {
          players$data <<- players$data %>% slice(-input$playersT_rows_selected)
        }
      })

      observeEvent(input$select_in, {
        if(!is.null(input$picksT_rows_selected)) {
          selections$data <<- rbind(selections$data, c(input$drafter_in, picks$data[input$picksT_rows_selected, 1], "Pick"))
        }
      })

      observeEvent(input$select_in, {
        if(!is.null(input$picksT_rows_selected)) {
          picks$data <<- picks$data %>% slice(-input$picksT_rows_selected)
        }
      })

      observeEvent(input$select_in, {
        if(!is.null(input$otherT_rows_selected)) {
          selections$data <<- rbind(selections$data, c(input$drafter_in, other$data[input$otherT_rows_selected, 1], "Other"))
        }
      })

      observeEvent(input$select_in, {
        if(!is.null(input$otherT_rows_selected)) {
          other$data <<- other$data %>% slice(-input$otherT_rows_selected)
        }
      })


  # Output to UI

    output$playersT <- renderDT({
      datatable(players$data,
                rownames = FALSE,
                selection = 'single',
                options = list(dom = 'tp',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#f6ee26', 'color': '#000000'});",
                                 "}")),
                colnames = c("Players"))
    })

    output$picksT <- renderDT({
      datatable(picks$data,
                rownames = FALSE,
                selection = 'single',
                options = list(dom = 'tp',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#f6ee26', 'color': '#000000'});",
                                 "}")),
                colnames = c("Picks"))

    })

    output$otherT <- renderDT({
      datatable(other$data,
                rownames = FALSE,
                selection = 'single',
                options = list(dom = 'tp',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#f6ee26', 'color': '#000000'});",
                                 "}")),
                colnames = c("Other Assets"))
    })

    output$selectionsT <- renderDT({
      datatable(selections$data %>% filter(name != "test"),
                selection = 'none',
                options = list(dom = 'tp',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#f6ee26', 'color': '#000000'});",
                                 "}")),
                colnames = c("Draft Rep", "Selection", "Type"))
    })

    output$console_text <- renderPrint(input$itemsT_rows_selected)
}

# Run the application
shinyApp(ui = ui, server = server)
