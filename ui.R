
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyalert)
library(shinybusy)

# Create js function to collapse boxes
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

# Create function to register state of collapsible boxes
collapseInput <- function(inputId, boxId) {
    tags$script(
        sprintf(
            "$('#%s').closest('.box').on('hidden.bs.collapse', 
            function () {Shiny.onInputChange('%s', true);})",
            boxId, inputId
        ),
        sprintf(
            "$('#%s').closest('.box').on('shown.bs.collapse', 
            function () {Shiny.onInputChange('%s', false);})",
            boxId, inputId
        )
    )
}

# Define UI for application
shinyUI(
    fluidPage(
        theme = shinytheme("cerulean"),
        
        useShinyalert(),
        useShinydashboard(),   
        withMathJax(),
        useShinyjs(),
        shinyjs:::useShinyjs(), 
        shinyjs:::extendShinyjs(text = jscode, functions = "collapse"),
        
        # Customize Notification
        tags$head(tags$style("
                             .shiny-notification{
                             position: fixed; top: 40% ;left: 43%; 
                             opacity: 1;
                             width: 350px;
                             height: 75px;
                             font-size: 17px")),
        
        # Add spinner when busy
        add_busy_spinner(spin = "orbit", position = "bottom-right"),
        
        # Application title
        titlePanel("likertBF: Bayes Factors for Ordinal Scales"),
        
        hr(),
        
        # Sidebar
        sidebarLayout(
            sidebarPanel(
                
                id = "input",
                
                h4("Data Input"),
                
                p("Enter observed response frequencies (and category labels)",
                  "for each condition."),
                
                # Frequencies in Condition 1
                textInputIcon("Y1",
                              label = "Condition 1",
                              placeholder = "20, 20, 20, 20",
                              icon = list("Frequencies")),
                
                # Frequencies in Condition 2
                textInputIcon("Y2",
                              label = "Condition 2",
                              placeholder = "20, 20, 20, 20",
                              icon = list("Frequencies")),
                
                # Logical; Provide category labels?
                materialSwitch("labels", 
                               label = "Category labels", 
                               status = "primary", 
                               right = TRUE),
                
                # If yes, input category labels
                conditionalPanel(
                    "input.labels == true",
                    textInputIcon("cats", 
                                  label = NULL,
                                  placeholder = "never, rarely, often, always",
                                  icon = list("Labels"))
                ),
                
                # Button to initiate data visualization
                actionBttn("show_data", 
                           label = "Plot Data",
                           style = "pill", color = "warning",
                           no_outline = F, size = "sm"),
                
                hr(),
                
                h4("Prior Settings"),
                
                p("Specify the prior variances \\( b_1 \\) and \\( b_2 \\) on",
                  "\\( \\alpha_j\\) and \\( \\theta_j\\), respectively."),
                
                fluidRow(
                    column(
                        width = 6,
                        
                        # Prior variance on alpha
                        numericInputIcon(
                            "b1", label = NULL,
                            value = 1,
                            min = 0,
                            step = .1,
                            icon = list("\\( b_1 \\)"),
                            help_text = "Must be positive."
                        )
                    ),
                    column(
                        width = 6,
                        
                        # Prior variance on theta
                        numericInputIcon(
                            "b2", label = NULL,
                            value = .1,
                            min = 0,
                            step = .01,
                            icon = list("\\( b_2 \\)"),
                            help_text = "Must be positive."
                        )
                    )
                ),
                
                hr(),
                
                h4("MCMC Settings"),
                
                # Number of posterior samples, M
                numericInput(
                    "M", label = "Number of posterior samples",
                    value = 2e5
                ),
                
                p(strong("Variance of proposal distributions"), br(),
                  "Specify the variances for the proposal distributions of the",
                  "Metropolis-Hastings sampling algorithm. If 'Auto-tuning' is selected,",
                  "the values represent lower bounds on the variances."),
                
                fluidRow(
                    column(width = 6,
                           
                           # Variance of alpha proposal distribution
                           numericInputIcon(
                               "tune1", label = NULL,
                               value = .0005,
                               min = 0,
                               step = .0005,
                               icon = list("\\( \\alpha \\)"),
                               help_text = "Must be positive."
                           )
                    ),
                    
                    column(width = 6,
                           
                           # Variance of theta proposal distribution
                           numericInputIcon(
                               "tune2", label = NULL,
                               value = .0005,
                               min = 0,
                               step = .0005,
                               icon = list("\\( \\theta \\)"),
                               help_text = "Must be positive."
                           )
                    )
                ),
                
                # Logical; Auto-tune variances of proposal distributions?
                materialSwitch(
                    "tuning", "Auto-tuning", 
                    status = "primary", right = TRUE, value = TRUE),
                
                hr(),
                
                fluidRow(
                    column(4, align = "center",
                           
                           # Start sampling and calculate Bayes factors
                           actionBttn("analyze", "Start Analysis",
                                      style = "pill", color = "success",
                                      no_outline = F, size = "sm")),
                    
                    column(4, align = "center",
                           
                           # Initiate MCMC visualization
                           actionBttn("show_mcmc", "Plot MCMC",
                                      style = "pill", color = "warning",
                                      no_outline = F, size = "sm")),
                    
                    column(4, align = "center",
                           
                           # Reset inputs and clear output
                           actionBttn("reset", "Reset",
                                      style = "pill", color = "danger",
                                      no_outline = F, size = "sm"))
                )
            ),
            
            # Show Manual, About, Results, and Plot
            mainPanel(
                
                # Manual box with instructions
                box(
                    includeHTML("www/manual.html"), 
                    title = "How to Use This App", status = "primary", 
                    solidHeader = F, collapsible = TRUE, collapsed = F,
                    width = 8
                ),
                
                # Info box with copyright and Github link
                box(
                    includeHTML("www/about.html"),  
                    title = "About", status = "primary", 
                    solidHeader = F, collapsible = TRUE, collapsed = F,
                    width = 4
                ),
                
                column(8,
                       box(
                           id = "bf_results",
                           # Dropdown menu for output customization
                           dropdownButton(
                               h4("Output Options"),
                               
                               # BF against unconstrained or preferred model
                               prettyRadioButtons(
                                   "bftype", "Bayes factors", 
                                   choices = c("against unconstrained Model" = "b_u",
                                               "against best Model" = "b_p"),
                                   selected = "b_u", inline = F),
                               
                               # Stochastic dominance directed or two-sided
                               prettyRadioButtons(
                                   "dominance", "Stochastic Dominance", 
                                   choices = c("two-sided" = "two",
                                               "1 > 2" = "1_2",
                                               "2 > 1" = "2_1"),
                                   selected = "two", inline = F),
                               
                               circle = TRUE, status = "danger", 
                               icon = icon("gear"), width = "250px",
                               size = "xs", 
                               tooltip = tooltipOptions(title = "Output Options")
                           ),
                           
                           column(12, align = "center",
                                  
                                  # Display Bayes factor table
                                  uiOutput("results")
                           ),
                           title = "Results", status = "primary", 
                           solidHeader = FALSE, collapsible = TRUE,
                           collapsed = TRUE, width = NULL),
                       
                       box(
                           id = "plot_mcmc",
                           column(12, align = "center",
                                  
                                  # Display MCMC plot
                                  plotOutput("plot_mcmc", height = "300px", 
                                             width = "100%")
                           ),
                           title = "MCMC Chains", status = "primary",
                           solidHeader = FALSE, collapsible = TRUE,
                           width = NULL, collapsed = TRUE
                       )
                ),
                
                column(4,
                       box(
                           id = "plot_data",
                           column(12, align = "center",
                                  # Display data plot
                                  plotOutput("plot_data", height = "450px", 
                                             width = "100%")
                           ),
                           title = "Data Visualization", status = "primary",
                           solidHeader = FALSE, collapsible = TRUE,
                           width = NULL, collapsed = TRUE
                       )
                ),
                
                # Register state of collapsible boxes
                collapseInput(inputId = "collapsed_data_box", boxId = "plot_data"),
                collapseInput(inputId = "collapsed_mcmc_box", boxId = "plot_mcmc"),
                collapseInput(inputId = "collapsed_results_box", boxId = "bf_results"),
                
            )
        )
    ))
