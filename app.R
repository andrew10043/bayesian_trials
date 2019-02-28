# Bayesian Re-Analysis of Clinical Trials
# Adapted from code by Dan Lane

library(shiny)
library(tidyverse)
library(shinyjs)

ui <- bootstrapPage(
  useShinyjs(),
  shinyUI(
    navbarPage("Bayesian Re-Analysis of Clinical Trials", 
               id = "tabs",
    tabPanel("Study Data",
             fluidPage(
               sidebarPanel(
                 selectInput("est_type", 
                             "Estimate Type",
                             choices = c("HR", "OR"),
                             selected = "HR"),
                 hr(),
                 numericInput("pt_est",
                              "Treatment Effect Point Estimate",
                              value = 0.5),
                 numericInput("upper_ci",
                              "Upper Confidence Limit",
                              value = 0.9),
                 numericInput("ci_width",
                              "Confidence Interval Width",
                              value = 0.95,
                              min = 0.9,
                              max = 1,
                              step = 0.01),
                 hr(),
                 numericInput("or_a",
                              "Events in Intervention Group",
                              value = 10,
                              min = 0,
                              max = NA,
                              step = 1),
                 numericInput("or_b",
                              "Events in Control Group",
                              value = 0,
                              min = 0,
                              max = NA,
                              step = 1),
                 numericInput("trt_n",
                              "Intervention Group Size",
                              value = 50,
                              min = 1,
                              max = NA,
                              step = 1),
                 numericInput("ctrl_n",
                              "Control Group Size",
                              value = 50,
                              min = 1,
                              max = NA,
                              step = 1)
               ),
               mainPanel(
                 h4("About this Application:"),
                 uiOutput("link_twitter"),
                 hr(),
                 h5("Step 1:"),
                 uiOutput("step_1"),
                 h5("Step 2:"),
                 uiOutput("step_2"),
                 h5("Step 3:"),
                 uiOutput("step_3"),
                 hr(),
                 uiOutput("link_email"),
                 br(),
                 renderText(expr = output$paper_link)
               )
             )
             ),
    tabPanel("Distributions",
             fluidPage(
               tags$style(HTML(".irs-bar {width: 100%; height: 5px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}")),
               tags$style(HTML(".irs-bar-edge {background: black; border: 1px solid black; height: 5px; border-radius: 15px 15px 15px 15px;}")),
               tags$style(HTML(".irs-line {border: 1px solid black; height: 5px;}")),
               tags$style(HTML(".irs-grid-text {font-family: 'arial'; color: black}")),
               tags$style(HTML(".irs-max {font-family: 'arial'; color: black;}")),
               tags$style(HTML(".irs-min {font-family: 'arial'; color: black;}")),
               tags$style(HTML(".irs-single {color:white; background:black;}")), 
               sidebarPanel(
                 sliderInput("theta",
                             "Prior Mean:",
                             min = 0.5,
                             max = 1.25,
                             value = 1,
                             step = 0.01,
                             ticks = FALSE),
                 hr(),
                 sliderInput("hr",
                             "Value of interest for computing the width of the prior distribution (e.g., MCID):",
                             min = 0.25,
                             max = 1.25,
                             value = 0.5,
                             step = 0.01,
                             ticks = FALSE),
                 sliderInput("pr",
                             "Probability that the prior is less than this value:",
                             min = 0,
                             max = 1,
                             value = 0.05,
                             step = 0.01,
                             ticks = FALSE),
                 hr(),
                 sliderInput("sd",
                             "Prior SD:",
                             min = 0.1,
                             max = 1,
                             value = 0.42,
                             step = 0.01,
                             ticks = FALSE),
                 hr(),
                 sliderInput("ci",
                             "Posterior Credible Interval: ",
                             value = 89,
                             min = 60,
                             max = 99,
                             step = 1,
                             post = "%",
                             ticks = FALSE),
                 sliderInput("hr_post",
                             "Posterior Value of Interest: ",
                             min = 0.5,
                             max = 1.25,
                             value = 0.9,
                             step = 0.01,
                             ticks = FALSE)
                 
                 ),
               
               # Show a plot of the generated distributions
               mainPanel(plotOutput("distPlot")
                         )
               )
             ),
    
    tabPanel("Heat Map",
             fluidPage(
               fluidRow(column(12,
                               h4("Interactive Heat Map:"),
                               uiOutput("heat_text"),
                               hr()
                               ),
               sidebarPanel(
                 sliderInput("hr_heat",
                             "Posterior Value of Interest:",
                             min = 0.5,
                             max = 1.25,
                             value = 0.9,
                             step = 0.01,
                             ticks = FALSE)
               ),
               mainPanel(
                 plotOutput("heatPlot")
                 )
               )
               )
               )
             )
    )
)

server <- function(input, output, session) {
  
  # Toggles
  est_type <- reactive({input$est_type})
  
  observeEvent(input$est_type, {
    
    toggleState(id = "upper_ci", condition = est_type() == "HR")
    toggleState(id = "pt_est", condition = est_type() == "HR")
    toggleState(id = "ci_width", condition = est_type() == "HR")
    
    toggleState(id = "or_a", condition = est_type() == "OR")
    toggleState(id = "or_b", condition = est_type() == "OR")
    toggleState(id = "trt_n", condition = est_type() == "OR")
    toggleState(id = "ctrl_n", condition = est_type() == "OR")
    
  }
    
  )

  # Publication Data
  pt_est <- reactive({input$pt_est})
  upper_ci <- reactive({input$upper_ci})
  ci_width <- reactive({input$ci_width})
  likelihood_p <- reactive({(1 - (1 - ci_width()) / 2)})
  
  or_a <- reactive({input$or_a})
  or_b <- reactive({input$or_b})
  trt_n <- reactive({input$trt_n})
  ctrl_n <- reactive({input$ctrl_n})
  or_c <- reactive({trt_n() - or_a()})
  or_d <- reactive({ctrl_n() - or_b()})
 
  # Calculate Priors
  theta_in <- reactive({input$theta})
  sd_in <- reactive({input$sd})
  hr_in <- reactive({input$hr})
  pr_in <- reactive({input$pr})
  
  prior_theta <- reactive({log(theta_in())})
  prior_sd <- reactive({sd_in()})
  
  # Estimate Type Labels
  est_type <- reactive({input$est_type})
  short_lab <- reactive({est_type()})
  long_lab <- reactive({
    if (short_lab() == "HR"){
      "Hazard Ratio"
    } else if (short_lab() == "OR"){
      "Odds Ratio"
    }
  })
  
  # Update sliders based on SD and Pr and HR
  observeEvent(input$sd, {
    updateSliderInput(session,
                      inputId = "pr",
                      label = "Probability that the prior is less than this value:",
                      value = round(pnorm(log(hr_in()), log(theta_in()), sd_in()), 3)
    )
  })
  
  observeEvent(input$hr, {
    updateSliderInput(session,
                      inputId = "pr",
                      label = "Probability that the prior is less than this value:",
                      value = round(pnorm(log(hr_in()), log(theta_in()), sd_in()), 3),
                      min = round(pnorm(log(hr_in()), log(theta_in()), 0.1), 3),
                      max = round(pnorm(log(hr_in()), log(theta_in()), 1), 3)
                      )

  })
  
  observeEvent(input$theta, {
    updateSliderInput(session,
                      inputId = "pr",
                      label = "Probability that the prior is less than this value:",
                      value = round(pnorm(log(hr_in()), log(theta_in()), sd_in()), 3),
                      min = round(pnorm(log(hr_in()), log(theta_in()), 0.1), 3),
                      max = round(pnorm(log(hr_in()), log(theta_in()), 1), 3)
                      )
  })
  
  observeEvent(input$pr, {
    updateSliderInput(session,
                      inputId = "sd",
                      label = "Prior SD:",
                      value = round((log(hr_in()) - log(theta_in()))/qnorm(pr_in()), 3)
    )
  })
  
  # Calculate Likelihood Parameters
  likelihood_theta <- reactive({
    
    if (est_type() == "HR") {
      reactive({log(pt_est())})
    } else if (est_type() == "OR") {
      log(
          ((or_a() + 0.5) * (or_d() * 0.5)) / 
            ((or_b() + 0.5) * (or_c() * 0.5))
        )
      }
  })
  
  likelihood_sd <- reactive({
    
    if (est_type() == "HR") {
      (log(upper_ci()) - log(pt_est())) / qnorm(likelihood_p())
    } else if (est_type() == "OR") {
      sqrt(
          ((1 / (or_a() + 0.5)) + 
             (1 / (or_b() + 0.5)) + 
             (1 / (or_c() + 0.5)) + 
             (1 / (or_d() + 0.5)))
        )
    }
    })
  
  # Calculate Posterior Parameters
  post_theta <- reactive({
    ((prior_theta() / (prior_sd())^2)+(likelihood_theta() / likelihood_sd()^2)) / 
      ((1 / (prior_sd())^2) + (1 / likelihood_sd()^2))
    })
  post_sd <- reactive({
    sqrt(1 / ((1 / (prior_sd())^2) + (1 / likelihood_sd()^2)))
    })
  
  # Plot data
  x <- seq(-3, 3, by = 0.01)
  prior_plot <- reactive({dnorm(x, prior_theta(), prior_sd())})
  likelihood_plot <- reactive({dnorm(x, likelihood_theta(), likelihood_sd())})
  posterior_plot <- reactive({dnorm(x, post_theta(), post_sd())})
  
  plot_data <- reactive({
    tibble(
      x = rep(x, 3)
    ) %>%
      mutate(
        dist = rep(c("prior", "likelihood", "posterior"), each = nrow(.) / 3),
        y = c(prior_plot(), likelihood_plot(), posterior_plot()),
        x = exp(x),
        y = exp(y)
      )
      
  })
  
  # Credible interval
  ci_in <- reactive({input$ci})
  
  # HR Post
  hr_post <- reactive({input$hr_post})
  
  # Dynamic Plot
   output$distPlot <- renderPlot({
     plot_data() %>%
       ggplot(aes(x = x, y = y, group = dist)) + 
       geom_vline(xintercept = 1, linetype = "dashed",
                  color = "grey50", alpha = 0.75) + 
       geom_line(aes(color = dist),
                 size = 1.1) + 
       scale_color_brewer(name = NULL, type = "qual", palette = "Dark2",
                          breaks = c("prior", "likelihood", "posterior"),
                          labels = c("Prior", "Likelihood", "Posterior")) + 
       xlim(0, 2) + 
       labs(
         x = long_lab(),
         y = "Probability Density"
       ) + 
       annotate(geom = "text",
                label = paste("Posterior probability ", short_lab(),
                              paste(" < 1: ", 
                              round(pnorm(log(1), post_theta(), post_sd(), 
                                          lower.tail = TRUE), 3), sep = ""), sep = ""),
                x = 2, y = max(plot_data()$y), hjust = 1,
                fontface = "bold") + 
       annotate(geom = "text",
                label = paste("Posterior probability ", short_lab(),
                              paste(" < ", hr_post(), sep = ""), 
                              paste(": ", round(pnorm(log(hr_post()), post_theta(), post_sd(),
                                                          lower.tail = TRUE), 3), sep = ""), sep = ""),
                x = 2, y = max(plot_data()$y) - max(plot_data()$y/25), hjust = 1,
                fontface = "bold") + 
       annotate(geom = "text",
                label = paste("Posterior median (", ci_in(),
                              paste("% credible interval): ",
                                    round(exp(qnorm(0.5, post_theta(), post_sd())), 2),
                                    paste(" (", round(exp(qnorm((1 - (ci_in()/100)) / 2, post_theta(), post_sd())), 2), sep = ""),
                              paste(", ", round(exp(qnorm(1 - (1 - (ci_in()/100)) / 2, post_theta(), post_sd())), 2), sep = ""),
                              paste(")", sep = ""), sep = ""), sep = ""),
                x = 2, y = max(plot_data()$y) - (2 * max(plot_data()$y)/25), hjust = 1,
                fontface = "bold") + 
       theme_classic() + 
       theme(
         legend.position = "bottom",
         text = element_text(family = "Gill Sans MT"),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title = element_text(size = 15),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 15)
       )
   }, height = 620)
   
   # HR Heat
   hr_heat <- reactive({input$hr_heat})
   
   # Heat data
   theta_list <- seq(from = 0.5, to = 1.5, by = 0.01)
   sd_list <- seq(from = 0.1, to = 0.8, length = length(theta_list))
   
    heat_data <- reactive({
     tibble(
       prior_theta = rep(theta_list, each = length(theta_list)),
       prior_sd = rep(sd_list, times = length(sd_list))
     ) %>%
     mutate(
       post_theta = ((log(prior_theta) / (prior_sd)^2)+(likelihood_theta() / likelihood_sd()^2)) / 
         ((1 / (prior_sd)^2)+(1 / likelihood_sd()^2)),
       post_sd = sqrt(1 / ((1 / (prior_sd)^2) + (1 / likelihood_sd()^2))),
       p_hr = pnorm(log(hr_heat()), post_theta, post_sd, lower.tail = TRUE)
     )
    })
  
   # Dynamic Heat Plot
   output$heatPlot <- renderPlot({
     heat_data() %>%
       ggplot(aes(x = prior_theta, y = prior_sd)) + 
       geom_tile(aes(fill = p_hr)) + 
       scale_fill_viridis_c(name = paste("Posterior Probabilty ", short_lab(),
                            paste(" < ", hr_heat(), sep = ""), sep = ""),
                            begin = min(heat_data()$p_hr),
                            end = max(heat_data()$p_hr)) + 
       labs(
         x = "Prior Mean",
         y = "Prior SD"
       ) + 
       theme_classic() + 
       theme(
         text = element_text(family = "Gill Sans MT"),
         axis.title = element_text(size = 15),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.position = "right"
       )
   }, width = 750, height = 550)
   
   # Link for paper
   url_paper <- a("JAMA", 
                  href="https://jamanetwork.com/journals/jama/fullarticle/2724361")
   url_discourse <- a("DataMethods", 
                      href="https://discourse.datamethods.org/t/andromeda-shock-or-how-to-intepret-hr-0-76-95-ci-0-55-1-02-p-0-06/1349")
   url_email <- a("benjamin.andrew@duke.edu", 
                  href="mailto:benjamin.andrew@duke.edu")
   url_bat <- a("(@BenYAndrew).", 
                href="https://twitter.com/BenYAndrew")
   url_dlt <- a("(@DanLane911)", 
                href="https://twitter.com/DanLane911")
   
    output$link_paper <- renderUI({
     tagList("Original paper: ", url_paper)
   })
   output$link_discourse <- renderUI({
     tagList("Discussion: ", url_discourse)
   })
   output$link_email <- renderUI({
     tagList("Questions & Improvements: ", url_email)
   })
   output$link_twitter <- renderUI({
     tagList("This is an interactive tool for Bayesian re-analysis clinical trials. Code by Dan Lane", url_dlt, "and adapted by Ben Andrew", url_bat) 
   })
   output$step_1 <- renderUI({
     tagList("Enter basic results from the clinical trial on this page. This will allow for approximation of the likelihood distribution.") 
   })
   output$step_2 <- renderUI({
     tagList("On the 'distributions' tab use the sliders to dynamically adjust the prior distribution by selecting the mean and either (1) the SD or (2) an outcome level of interest and the desired probability mass of the prior below that level.") 
   })
   output$step_3 <- renderUI({
     tagList("One the 'heat map' tab, use the slider to select an outcome level of interest to visualize the posterior probability of the outcome falling below that level for various combinations of the prior's mean and SD.") 
   })
   output$heat_text <- renderUI({
     tagList("Use the slider below to select a posterior value of interest. The heat map will display the probability the posterior is below your selected value for all combinations of the prior's mean and SD.") 
   })

}
# Run the application 
shinyApp(ui = ui, server = server)
