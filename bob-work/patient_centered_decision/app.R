library(shiny)
library(Matrix)
library(tidyverse)
library(ggdag)

# To Do
# User chooses a state, indicating a column of the PENALTY table.
# Values from the table are loaded into the adverse events sliders.
# Changes in the sliders are copied back into the PENALTY table.

# Think about:
# Each adverse event should have both a likelihood (and/or severity?) and a degree to which this advers event bothers the patient.

# Functions

# Define behavior here to keep the structure of the ui and server objects simpler.

DOC_URL <- "file:///Users/rmhorton/Documents/projects/patient_centered_decision_model/PCD_app/patient_centered_decision/HandN_6state_MM.html"

plot_transition_graph <- function(){
  # "C", "T", "F", "H", "NT", "D"
  g <- dagify(
    T ~ C + T,
    NT ~ C + NT,
    D ~ F + H + NT + D,
    F ~ T + F,
    H ~ F + H,
    coords = list(
      x = c(C=1, D=2, F=3, H=3, T=2, NT=1),
      y = c(C=2, D=1, F=2, H=1, T=2, NT=1)
    )
  ) %>% tidy_dagitty(layout = "fr") %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_node(colour='black', stroke=1, shape=1) +
    geom_dag_text(color='black') +
    geom_dag_edges() +
    theme_dag_blank()
  
  plot(g)
}

STATE <- c(
  'Cancer'='C', 
  'Treatment immediately following surgery'='T',
  'Followup period'='F', # including radiation and chemotherapy
  'Health'='H',
  'Not Treated'='NT',
  'Death'='D'
)

AEs <- c(
  H  = "Hospitalization", 
  PN = "Pain or numbness", 
  L = "Lymphoedema", 
  S  = "Feeling sick",
  T  = "Tiredness", 
  DW = "Difficulty swallowing", 
  DS = "Difficulty speaking", 
  MT = "Missing teeth",
  NS = "Neck/shoulder stiffness",
  HL = "Hearing loss"
)

PENALTY <- matrix(c(
  5,9,0,0,
  0,0,2,4,
  0,7,5,1,
  2,9,5,1,
  4,5,1,1,
  4,5,5,1,
  4,6,8,1,
  2,5,4,0,
  0,5,5,1,
  2,5,5,0
), nrow = 10, byrow = TRUE)
rownames(PENALTY) <- c("H", "PN", "L", "S", "T", "DW", "DS", "MT", "NS", "HL")
colnames(PENALTY) <- STATE[1:4]

AE_STEP <- 1


p12 = 0.9             # T ~ C + T; NT ~ C + NT (p15 = 1 - p12)
p22 = 0.4             # !T ~ T
p23 = 1 - p22         # F ~ T + F
p33 = 0.6             # !F ~ F
p36 = 0.001           # D ~ F + H + NT + D
p34 = 1 - (p33 + p36) # H ~ F + H
p46 = 0.0005          # !D ~ H
p44 = 1 - p46         # !H ~ H
p56 = 0.01            # !D ~ NT
p55 = 1 - p56         # !NT ~ NT

T = matrix(c(
  0.0, p12, 0.0, 0.0, 1-p12, 0.0,
  0.0, p22, p23, 0.0, 0.0, 0.0,
  0.0, 0.0, p33, p34, 0.0, p36,
  0.0, 0.0, 0.0, p44, 0.0, p46,
  0.0, 0.0, 0.0, 0.0, p55, p56,
  0.0, 0.0, 0.0, 0.0, 0.0, 1.0
), nrow = length(STATE), byrow = TRUE)
colnames(T) <- STATE
rownames(T) <- STATE


#############

ui = fluidPage(
  tabsetPanel(
    tabPanel("Introduction",
        fluidPage(
          # tags$iframe(src=DOC_URL, height=1200, width=800) # 
          # includeHTML('HandN_6state_MM.html') # H&N-6state-MM.html'
          # includeHTML('Simple_HandN.html') # Doesn't work if there are self-contained images?
          h1("Put documentation here")
        )
    ),
    
    tabPanel(
      "Transition Graph",
      fluidPage(
        plotOutput("transition_graph_plot"),
        p("This diagram shows the state transitions:"),
        tableOutput("state_table")
      )
    ),
    
    tabPanel("Adverse Events",
      sidebarLayout(
        sidebarPanel(
         selectInput("state", "State", STATE[1:4]) # , width=4
        ),
        mainPanel(
         sliderInput("ae_H", "Hospitalization", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_PN", "Pain or numbness", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_L", "Lymphoedema", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_S", "Feeling sick", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_T", "Tiredness", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_DW", "Difficulty swallowing", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_DS", "Difficulty speaking", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_MT", "Missing teeth", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_NS", "Neck/shoulder stiffness", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE),
         sliderInput("ae_HL", "Hearing loss", 
                     min = 0, max = 9, value = 0, 
                     step=AE_STEP, ticks=FALSE) # , width=10
        ),
        fluid=TRUE
       )
    ),
    
    tabPanel(
      "Penalty Matrix",
      fluidPage( 
        h2("Adverse event penalties for each state"), 
        tableOutput("penalty_table")
      )
    ),
    
    # aspect_min_max <- list(
    #   "Mobility"=c('no problems walking about', 'confined to bed'),
    #   "Self care"=c('no problems with self-care', 'unable to wash or dress myself'),
    #   "Usual activities"=c('no problems doing my usual activitie', 'unable to do my usual activities'),
    #   "Pain/discomfort"=c('no pain or discomfort', 'extreme pain or discomfort'),
    #   "Anxiety/depression"=c('not anxious or depressed', 'extremely anxious or depressed')
    # )


    
    tabPanel("EQ-5D", fluid = FALSE,
        sidebarPanel(
           # 'no problems walking about' to 'confined to bed'
           sliderInput("eq5d_mobility", label="Mobility", 
                       min = 1, max = 5, value = 1, 
                       step=0.1, ticks=FALSE),
           # 'no problems with self-care' to 'unable to wash or dress myself'
           sliderInput("eq5d_self_care", label="Self care", 
                       min = 1, max = 5, value = 1, 
                       step=0.1, ticks=FALSE),
           # Usual activities include work, study, housework, family, or leisure activities .
           # 'no problems doing my usual activities' to 'unable to do my usual activities'.
           sliderInput("eq5d_activities", label="Usual activities", 
                       min = 1, max = 5, value = 1,
                       step=0.1, ticks=FALSE),
           # 'no pain or discomfort' to 'extreme pain or discomfort'
           sliderInput("eq5d_pain", label="Pain/discomfort", 
                       min = 1, max = 5, value = 1,
                       step=0.1, ticks=FALSE),
           # 'not anxious or depressed' to 'extremely anxious or depressed'
           sliderInput("eq5d_depression", label="Anxiety/depression", 
                       min = 1, max = 5, value = 1, 
                       step=0.1, ticks=FALSE),
           width = "200px", fixed_width=TRUE
         )
         
       ),
    
    tabPanel(
      "Time tradeoff",
      fluidPage( 
        h2("Current state for X years vs. perfect health for shorter time?"), 
        p('"Imagine that you are told that you have 10 years left to live. In connection with this you are also told that you can choose to live these 10 years in your current health state or that you can choose to give up some life years to live for a shorter period in full health. Indicate with a cross on the line the number of years in full health that you think is of equal value to 10 years in your current health state."'),
        tags$span('See also:'),
        tags$a(href="https://en.wikipedia.org/wiki/Utility_assessment", "Utility assessment")
      )
    ),
    
    tabPanel(
      "Standard gamble",
      fluidPage( 
        h2("AI Overview"), 
        p("The standard gamble (SG) is a method used to assess health utilities, which represent an individual's subjective valuation of a health state. It involves asking a person to choose between living with a certain health state or taking a risky gamble that could result in perfect health or death. The utility value is determined by finding the point of indifference between the two options, where the risk of death in the gamble is balanced against the disutility of living with the certain health state."),
        h3("How the Standard Gamble Works"),
        tags$ul(
          tags$li(tags$b("Presenting the Choice:")),
          p("The SG method presents a respondent with a choice between two options:
          Living with a specific health state for the remainder of their life (this is the \"certain\" outcome).
          A gamble where they either live with perfect health (or are disease-free) with a certain probability (p) or die immediately with the remaining probability (1-p).
          "),
          tags$li(tags$b("Titrating the Risk:")),
          p("The probability of death (1-p) in the gamble is gradually increased or decreased until the respondent is indifferent between the two choices."),
          tags$li(tags$b("Determining the Utility:")),
          p("The point of indifference is the point where the probability of perfect health in the gamble represents the utility of the certain health state. For example, if the respondent is indifferent between living with a health state and a 75% chance of perfect health and 25% chance of death, the utility of the health state would be 0.75, according to YHEC."),
          tags$li(tags$b("Scaling the Utility:")),
          p("Utilities are typically scaled from 0 to 1, where 0 represents death and 1 represents perfect health.")
        ),
        p("The Standard Gamble approach assumes that individuals will choose the option that maximizes their expected utility, which is the weighted average of the utilities of the possible outcomes."),
        h3("Limitations"),
        tags$ul(
          tags$li(tags$b("Cognitive Complexity:")),
          p("It relies on the respondent's ability to understand and interpret complex probabilities."),
          tags$li(tags$b("Potential Biases:")),
          p("It may be susceptible to biases, such as a tendency to prefer certain outcomes over risky ones."),
          p(tags$b("(Bob thinks the AI got this exactly wrong; eliciting risk preference is the point.)")),
          tags$li(tags$b("Time Independence Assumption:")),
          p("It assumes that the utility of a health state is independent of the length of time spent in that state, which may not always be the case.")
        )

      )
    )
  )
)


ae_slider_names <- setNames(paste('ae', names(AEs), sep='_'), nm=names(AEs))

get_penalty_dataframe <- function(){
  as.data.frame(
    bind_cols(
      data.frame(
        aspect=dimnames(PENALTY)[[1]],
        description=AEs[dimnames(PENALTY)[[1]]]
      ), 
      as.data.frame(PENALTY)
    )
  )
}

server <- function(input, output, session) {
  # Show the plot of the transition graph
  output$transition_graph_plot <- renderPlot(
    plot_transition_graph()
  )
  
  # Show the table of different states
  output$state_table <- renderTable(tibble(node=STATE, description=STATE))
  
  # Display the PENALTY matrix
  output$penalty_table <- renderTable(
    get_penalty_dataframe()
  )
  
  # Copy PENALTY matrix values to sliders
  observe({
    my_state <- input$state # 'C'
    for (my_ae in names(AEs)){
      my_slider <- paste0('ae_', my_ae)
      updateSelectInput(session, my_slider, selected=PENALTY[my_ae, my_state])
    }
  })
  
  # Update matrix with slider values
  
  ## This works:
  # ae_name <- 'L'
  # slider_name <- paste("ae", ae_name, sep='_')
  # observeEvent(input[[slider_name]], { 
  #   message(slider_name)
  #   PENALTY[ae_name, input$state] <<- input[[slider_name]]
  #   output$penalty_table <- renderTable(
  #     get_penalty_dataframe()
  #   )
  # })
  
  ## This does not work:
  # observeEvent(input[[slider_name]], { 
  #   ae_name <- 'L'
  #   slider_name <- paste("ae", ae_name, sep='_')
  #   # message(slider_name)
  #   PENALTY[ae_name, input$state] <<- input[[slider_name]]
  #   output$penalty_table <- renderTable(
  #     get_penalty_dataframe()
  #   )
  # })

  # "H"  "PN" "L"  "S"  "T"  "DW" "DS" "MT" "NS" "HL"
  # This does not work
  # for (ae_name in names(AEs)){
  #   slider_name <- paste("ae", ae_name, sep='_')
  #   observeEvent(input[[slider_name]], { 
  #     PENALTY[ae_name, input$state] <<- input[[slider_name]]
  #     output$penalty_table <- renderTable(
  #       get_penalty_dataframe()
  #     )
  #   })
  # }
  
  ## This works:
  observeEvent(input$ae_H, {
    PENALTY['H', input$state] <<- input$ae_H
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })

  observeEvent(input$ae_PN, {
    PENALTY['PN', input$state] <<- input$ae_PN
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_L, {
    PENALTY['L', input$state] <<- input$ae_L
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_S, {
    PENALTY['S', input$state] <<- input$ae_S
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_T, {
    PENALTY['T', input$state] <<- input$ae_T
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_DW, {
    PENALTY['DW', input$state] <<- input$ae_DW
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_DS, {
    PENALTY['DS', input$state] <<- input$ae_DS
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_MT, {
    PENALTY['MT', input$state] <<- input$ae_MT
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_NS, {
    PENALTY['NS', input$state] <<- input$ae_NS
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
  observeEvent(input$ae_HL, {
    PENALTY['HL', input$state] <<- input$ae_HL
    output$penalty_table <- renderTable( get_penalty_dataframe() )
  })
  
}

shinyApp(ui, server, options=list(height=1000, width=1600))

