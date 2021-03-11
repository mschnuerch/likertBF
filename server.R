
library(shiny)
library(shinyalert)
library(shinyjs)
library(bridgesampling)
library(knitr)
library(kableExtra)
source("00_helper.R")
source("01_sampling_functions.R")
source("02_plot_functions.R")
source("03_bf_functions.R")

shinyServer(function(input, output, session) {
    
    # Reactive Functions ------------------------------------------------------
    
    # Reactive object to store output 
    values <- reactiveValues(
        res = list(),
        samples = list(),
        diff = list(),
        roc = list(),
        mcmc = list()
    )
    
    # Coerce text input to numeric vector (Condition 1)
    get_y1 <- reactive({
        input$Y1 %>% 
            stri_replace_all_regex(pattern = c(";", "/"),
                                   replacement = ",",
                                   vectorize_all = F) %>% 
            paste0("c(", ., ")") %>% 
            parse(text = .) %>% 
            eval()
    })    
    
    # Coerce text input to numeric vector (Condition 2)
    get_y2 <- reactive({
        input$Y2 %>% 
            stri_replace_all_regex(pattern = c(";", "/"),
                                   replacement = ",",
                                   vectorize_all = F) %>% 
            paste0("c(", ., ")") %>% 
            parse(text = .) %>% 
            eval()
    })
    
    # Coerce text input to character vector (Category labels)
    get_cats <- reactive({
        input$cats %>% 
            stri_replace_all_regex(pattern = c(" ,", " ;", ", ", "; ", ";", "/ ", "/"),
                                   replacement = ",",
                                   vectorize_all = F) %>% 
            str_split(pattern = ",") %>% 
            unlist()
    })
    
    # Check whether data plot box is collapsed
    check_plot_box_data <- eventReactive(input$show_data, {
        input$collapsed_data_box
    })
    
    # Check whether MCMC plot box is collapsed
    check_plot_box_mcmc <- eventReactive(input$show_mcmc, {
        input$collapsed_mcmc_box
    })
    
    # Check whether results box is collapsed
    check_box_results <- eventReactive(input$analyze, {
        input$collapsed_results_box
    })
    
    # Check whether data plot box is collapsed when resetting
    check_plot_box_data_on_reset <- eventReactive(input$reset, {
        if(is.null(input$collapsed_data_box))
            return(TRUE)
        else
            return(input$collapsed_data_box)
    })
    
    # Check whether MCMC plot box is collapsed when resetting
    check_plot_box_mcmc_on_reset <- eventReactive(input$reset, {
        if(is.null(input$collapsed_mcmc_box))
            return(TRUE)
        else
            return(input$collapsed_mcmc_box)
    })
    
    # Check whether results box is collapsed when resetting
    check_results_box_on_reset <- eventReactive(input$reset, {
        if(is.null(input$collapsed_results_box))
            return(TRUE)
        else
            return(input$collapsed_results_box)
    })
    
    # Check input for sampling and BF calculations
    check_bf <- eventReactive(input$analyze, {
        
        check_input_bf(
            Y1    = input$Y1,
            Y2    = input$Y2,
            b1    = input$b1,
            b2    = input$b2,
            tune1 = input$tune1,
            tune2 = input$tune2,
            M     = input$M
        )
        
    })
    
    # Check input for data visualization
    check_data_plot <- eventReactive(input$show_data, {
        
        check_input_data_plot(
            Y1     = input$Y1,
            Y2     = input$Y2,
            cats   = input$cats,
            labels = input$labels
        )
        
    })
    
    # Check input for MCMC visualization
    check_mcmc_plot <- eventReactive(input$show_mcmc, {
        
        check_input_mcmc_plot(
            samples = values$samples
        )
        
    })
    
    # Get input for MCMC visualization 
    get_mcmc_samples <- eventReactive(input$show_mcmc, {
        
        values$samples
        
    })
    
    # Analyze data (MCMC sampling + BF calculations)
    res <- eventReactive(input$analyze, {
        
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Running Analysis", value = 0)
        
        updateProgress <- function(value, detail) {
            
            progress$set(value = value, detail = detail)
            
        }
        
        dat <- list(
            Y1 = get_y1(),
            Y2 = get_y2()
        )
        
        b <- c(input$b1, input$b2)
        M <- input$M
        max_iter <- input$max_iter
        
        if(input$tuning){
            small <- c(input$tune1, input$tune2)
            tune = NULL
        }else{
            small <- NULL
            tune = c(input$tune1, input$tune2)
        }
        
        likertBF(dat, b, M, tune, small, max_iter, updateProgress)
        
    })
    
    # Create descriptive plots of data
    data_plot <- eventReactive(input$show_data, {
        
        dat <- list(
            Y1 = get_y1(),
            Y2 = get_y2()
        )
        
        p1 <- roc_plot(dat)
        
        if(input$labels){
            p2 <- diff_plot(dat, get_cats())
        }else{
            p2 <- diff_plot(dat)
        }
        
        list(p1 = p1, p2 = p2)
        
    })
    
    # React to click on "Start analysis" button (check input, create output)
    observe({
        if(input$analyze == 0){
            return()
        }else{
            tmp <- check_bf()
            if(tmp$error){
                shinyalert("Oops...", 
                           text = tmp$message, 
                           type = "error")
            }else if(!is.null(tmp$warning)){
                shinyalert("Warning", 
                           text = paste(tmp$warning, collapse = "\n"), 
                           type = "warning",
                           showCancelButton = TRUE,
                           confirmButtonText = "Continue Analysis",
                           cancelButtonText = "Change Settings",
                           inputId = "continue",
                           callbackR = function(x) {
                               if(x){
                                   tmp <- safely(quietly(res))()
                                   if(is.null(tmp$error)){
                                       collapsed_results_box <- check_box_results()
                                       values$res <- tmp$result$result$bf
                                       values$samples <- tmp$result$result$samples
                                       if(any(is.null(collapsed_results_box), collapsed_results_box))
                                           js$collapse("bf_results")
                                       if(length(tmp$result$warnings) != 0){
                                           shinyalert("Warning", 
                                                      text = paste(unique(tmp$result$warnings), 
                                                                   collapse = "\n"), 
                                                      type = "warning")
                                       }
                                   }else{
                                       shinyalert("Oops...", 
                                                  text = tmp$error$message, 
                                                  type = "error")
                                   }
                               }
                           })
            }else{
                tmp <- safely(quietly(res))()
                if(is.null(tmp$error)){
                    collapsed_results_box <- check_box_results()
                    values$res <- tmp$result$result$bf
                    values$samples <- tmp$result$result$samples
                    if(any(is.null(collapsed_results_box), collapsed_results_box))
                        js$collapse("bf_results")
                    if(length(tmp$result$warnings) != 0){
                        shinyalert("Warning", 
                                   text = paste(unique(tmp$result$warnings), 
                                                collapse = "\n"), 
                                   type = "warning")
                    }
                }else{
                    shinyalert("Oops...", 
                               text = tmp$error$message, 
                               type = "error")
                }
            }
        }
    })
    
    # React to click on "Plot data" button (check input and create output)
    observe({
        if(input$show_data == 0){
            return()
        }else{
            tmp <- check_data_plot()
            if(tmp$error){
                shinyalert("Oops...",
                           text = tmp$message,
                           type = "error")
            }else{
                tmp <- data_plot()
                collapsed_data_box <- check_plot_box_data()
                values$roc <- tmp$p1
                values$diff <- tmp$p2
                if(any(is.null(collapsed_data_box), collapsed_data_box))
                    js$collapse("plot_data")
            }
        }
    })
    
    # React to click on "Plot MCMC" button (check input and create output)
    observe({
        if(input$show_mcmc == 0){
            return()
        }else{
            tmp <- check_mcmc_plot()
            if(tmp$error){
                shinyalert("Oops...",
                           text = tmp$message,
                           type = "error")
            }else{
                values$mcmc <- get_mcmc_samples()
                collapsed_mcmc_box <- check_plot_box_mcmc()
                if(any(is.null(collapsed_mcmc_box), collapsed_mcmc_box))
                    js$collapse("plot_mcmc")
            }
        }
    })
    
    # React to click on "Reset" button 
    observe({
        if(input$reset == 0){
            return()
        }else{
            updateTextInput(session, "Y1", 
                            value = character(0))
            updateTextInput(session, "Y2", 
                            value = character(0))
            values$res <- list()
            values$diff <- list()
            values$roc <- list()
            values$samples <- list()
            values$mcmc <- list()
            if(!check_plot_box_data_on_reset())
                js$collapse("plot_data")
            if(!check_plot_box_mcmc_on_reset())
                js$collapse("plot_mcmc")
            if(!check_results_box_on_reset())
                js$collapse("bf_results")
            shinyjs::reset("input")
        }
    })  
    
    
    # Render Output -----------------------------------------------------------
    
    # Create BF table with results from analysis
    output$results <- renderPrint({
        
        bf <- values$res
        
        if(is_empty(bf)){
            return(HTML(paste(em("No results to display yet"))))
        }else{
            
            if(input$dominance == "two"){
                bf <- bf[-(3:4)]
            }else if(input$dominance == "1_2"){
                bf <- bf[-c(2, 4)]
            }else{
                bf <- bf[-(2:3)]
            }
            
            if(input$bftype == "b_p"){
                bf <- bf / max(bf)
            }
            
            models <- c("Unconstrained", "Dominance", "Shift", "Null")
            tab <- data.frame(models, bf) %>% 
                knitr::kable(format = "html", digits = 3,
                             col.names = c("Model", "BF\\(^a\\)"),
                             align = "c", escape = F, booktabs = TRUE) %>%
                kable_styling("hover", full_width = F) %>%
                add_footnote(ifelse(input$bftype == "b_p",
                                    "Bayes factor against best model",
                                    "Bayes factor against unconstrained model"),
                             notation="alphabet",
                             escape = FALSE)
            withMathJax(HTML(tab))
        }
        
    })
    
    # Plot data (ROC + difference plot)
    output$plot_data <- renderPlot({
        tryCatch(
            {cowplot::plot_grid(values$roc, values$diff, 
                                nrow = 2, align = "v")},
            error = function(x) {""} 
        )
        
    })
    
    # Plot MCMC from unconstrained model
    output$plot_mcmc <- renderPlot({
        tryCatch(
            {mcmc_plot(values$mcmc)},
            error = function(x) {""}
        )
    })
    
})
