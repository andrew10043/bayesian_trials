# Bayesian Re-Analysis of Clinical Trials
# Adapted from code by Dan Lane

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(RColorBrewer)
library(tidybayes)

ui <- bootstrapPage(
  useShinyjs(),
  withMathJax(),
  shinyUI(
    navbarPage("Bayesian Re-Analysis of Clinical Trials", 
               id = "tabs",
    tabPanel("Home",
             fluidPage(
               fluidRow(column(12,
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
                               uiOutput("test"),
                               br(),
                               renderText(expr = output$paper_link)
               )
             ))),
    tabPanel("Study Data",
             fluidPage(
               fluidRow(
                 column(width = 4,
                        h4("Study Details"),
                        hr(),
                        prettyRadioButtons("est_type",
                                "What type of study is being analyzed?",
                                choices = list("Time to Event (HR)" = 1, 
                                               "Dichotomous Outcome (OR)" = 2,
                                               "Dichotomous Outcome (RR)" = 3),
                                selected = 1),
                   hr(),
                   prettyRadioButtons("se_avail", 
                                label = "Is the standard error of the point estimate available?",
                                choices = list("Yes" = 1, "No" = 2), 
                                inline = FALSE,
                                selected = 2),
                   conditionalPanel(
                     condition = "input.est_type == 1",
                   hr(),
                   prettyRadioButtons("rates_avail", 
                                label = "Are groupwise event rates available?",
                                choices = list("Yes" = 1, "No" = 2), 
                                inline = FALSE,
                                selected = 2)
                   )
                 ),
                 column(width = 4,
                        conditionalPanel(
                            condition = "input.est_type == 1",
                          h4("Time to Event Data"),
                          hr(),
                          numericInput("pt_est",
                                       "HR Point Estimate",
                                       value = 0.5),
                          conditionalPanel(
                            condition = "input.se_avail == 1",
                            numericInput("se_hr",
                                         "Standard Error of HR",
                                         value = NA)),
                          numericInput("ci_width",
                                       "Confidence Interval Width",
                                       value = 0.95,
                                       min = 0.9,
                                       max = 1,
                                       step = 0.01),
                          numericInput("upper_ci",
                                       "Upper Confidence Limit",
                                       value = 0.9),
                          conditionalPanel(
                            condition = "input.rates_avail == 1",
                          numericInput("hr_a",
                                       "Events in Intervention Group",
                                       value = 10,
                                       min = 0,
                                       max = NA,
                                       step = 1),
                          numericInput("hr_b",
                                       "Events in Control Group",
                                       value = 30,
                                       min = 0,
                                       max = NA,
                                       step = 1)
                          )
                          ),
                          conditionalPanel(
                            condition = "input.est_type != 1",
                            conditionalPanel(
                              condition = "input.est_type == 2",
                            h4("Dichotomous Outcome Data (OR)")),
                            conditionalPanel(
                              condition = "input.est_type == 3",
                              h4("Dichotomous Outcome Data (RR)")),
                            hr(),
                            conditionalPanel(
                              condition = "input.se_avail == 1",
                              numericInput("se_or",
                                           "Standard Error of OR",
                                           value = NA)),
                            numericInput("or_a",
                                         "Events in Intervention Group",
                                         value = 10,
                                         min = 0,
                                         max = NA,
                                         step = 1),
                            numericInput("or_b",
                                         "Events in Control Group",
                                         value = 30,
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
                          hr()
                   ),
                 column(width = 4,
                          h4("Technical Notes"),
                          hr(),
                        conditionalPanel(
                          condition = "input.est_type == 1",
                          uiOutput("tech_notes_1")),
                        conditionalPanel(
                          condition = "input.est_type == 2",
                          uiOutput("tech_notes_2")),
                        conditionalPanel(
                          condition = "input.est_type == 3",
                          uiOutput("tech_notes_3")),
                          br(),
                          uiOutput("lhsd_3"),
                          uiOutput("lhsd_1"),
                        conditionalPanel(
                          condition = "input.est_type != 3",
                          uiOutput("lhsd_2")),
                          br(),
                          conditionalPanel(
                            condition = "input.est_type == 1",
                            uiOutput("eqn_1a"),
                            uiOutput("eqn_2a")
                          ),
                          conditionalPanel(
                            condition = "input.est_type == 2",
                            uiOutput("eqn_1b"),
                            uiOutput("eqn_2b")
                          ),
                        conditionalPanel(
                          condition = "input.est_type == 3",
                          uiOutput("eqn_1c")
                        ),
                          hr()
                          )
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
               sidebarPanel(width = 4,
                 sliderInput("theta",
                             "Prior Mean:",
                             min = 0.1,
                             max = 2,
                             value = 1,
                             step = 0.01,
                             ticks = FALSE),
                 sliderInput("hr",
                             "Value of interest for computing the width of the prior distribution (e.g., MCID):",
                             min = 0.1,
                             max = 2,
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
                 sliderInput("sd",
                             "Prior SD:",
                             min = 0.1,
                             max = 1,
                             value = 0.42,
                             step = 0.01,
                             ticks = FALSE),
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
                             ticks = FALSE),
                 prettyRadioButtons("post_alpha",
                                    label = "Show Posterior Area of Interest?",
                                    choices = list("Yes" = 1,
                                                   "No" = 0),
                                    selected = 1,
                                    inline = TRUE,
                                    fill = FALSE,
                                    outline = FALSE,
                                    status = "primary"),
                 prettyRadioButtons("cred_alpha",
                                    label = "Show Posterior Credible Interval?",
                                    choices = list("Yes" = 1,
                                                   "No" = 0),
                                    selected = 1,
                                    inline = TRUE,
                                    fill = FALSE,
                                    outline = FALSE,
                                    status = "primary")
                 # prettyCheckboxGroup("dist_display", 
                 #                    label = "Distribution Toggle:", 
                 #                    choices = list("Prior" = 1, 
                 #                                   "Likelihood" = 2, 
                 #                                   "Posterior" = 3),
                 #                    selected = c(1, 2, 3),
                 #                    inline = TRUE,
                 #                    fill = FALSE,
                 #                    outline = FALSE,
                 #                    status = "primary")
                 
                 ),
               
               # Show a plot of the generated distributions
               mainPanel(width = 8,
                         plotOutput("distPlot")
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
  rates_avail <- reactive({input$rates_avail})
  se_avail <- reactive({input$se_avail})

  
  observeEvent(input$est_type, {
    
    toggleState(id = "upper_ci", condition = est_type() == 1)
    toggleState(id = "lower_ci", condition = est_type() == 1)
    toggleState(id = "pt_est", condition = est_type() == 1)
    toggleState(id = "ci_width", condition = est_type() == 1)
    
    toggleState(id = "hr_a", condition = all(c(est_type() == 1,
                                               rates_avail() == 1)))
    toggleState(id = "hr_b", 
                condition = all(c(est_type() == 1,
                                  rates_avail() == 1)))
    
    toggleState(id = "or_a", condition = est_type() %in% c(2, 3))
    toggleState(id = "or_b", condition = est_type() %in% c(2, 3))
    toggleState(id = "ctrl_n", condition = est_type() %in% c(2, 3))
    toggleState(id = "trt_n", condition = est_type() %in% c(2, 3))
    
  })
  
  observeEvent(input$rates_avail, {
    
    toggleState(id = "hr_a", condition = all(c(est_type() == 1,
                                             rates_avail() == 1)))
    toggleState(id = "hr_b", 
              condition = all(c(est_type() == 1,
                                rates_avail() == 1)))
    
  })
  
  observeEvent(input$se_avail, {
    
    toggleState(id = "se_est", condition = se_avail() == 1)
    
  })
  
  # Curve Toggle Plot
  # dist_display <- reactive({input$dist_display})
  # 
  # dist_list <-
  #   reactive({
  #     temp <- as.numeric(c(dist_display()[1],
  #                  dist_display()[2],
  #                  dist_display()[3]))
  #     
  #     c("prior", "likelihood", "posterior")[temp[!is.na(temp)]]
  #   })

  # Publication Data
  pt_est <- reactive({input$pt_est})
  upper_ci <- reactive({input$upper_ci})
  ci_width <- reactive({input$ci_width})
  likelihood_p <- reactive({(1 - ((1 - ci_width()) / 2))})
  
  hr_a <- reactive({input$hr_a})
  hr_b <- reactive({input$hr_b})
  
  se_hr <- reactive({input$se_hr})
  se_or <- reactive({input$se_or})
  
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
  short_lab <- reactive({
    if (est_type() == 1){
      "HR"
    } else if (est_type() == 2){
      "OR"
    } else if (est_type() == 3){
      "RR"
    }
  })
  long_lab <- reactive({
    if (est_type() == 1){
      "Hazard Ratio"
    } else if (est_type() == 2){
      "Odds Ratio"
    } else if (est_type() == 3){
      "Risk Ratio"
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
    
    if (est_type() == 1) {
      log(pt_est())
    } else if (est_type() == 2) {
      log(
          ((or_a() + 0.5) * (or_d() + 0.5)) / 
            ((or_b() + 0.5) * (or_c() + 0.5))
        )
    } else if (est_type() == 3){
      log(
        (or_a() / (or_a() + or_c())) / 
          (or_b() / (or_b() + or_d()))
      )
      
      }
  })
  
  likelihood_sd <- reactive({
    
    if (est_type() == 1) {
      
      if (rates_avail() == 1){
        
        sqrt((1 / hr_a()) + (1 / hr_b()))
        
      } else {
        
        (log(upper_ci()) - log(pt_est())) / qnorm(likelihood_p())
        
      }
      
    } else if (est_type() == 2) {
      sqrt(
          ((1 / (or_a() + 0.5)) + 
             (1 / (or_b() + 0.5)) + 
             (1 / (or_c() + 0.5)) + 
             (1 / (or_d() + 0.5)))
        )
    } else if (est_type() == 3) {
      sqrt(
        (or_c() / (or_a() * (or_a() + or_c()))) + 
          (or_d() / (or_b() * (or_b() + or_d())))  
      )
    }
    })
  
  # Calculate alternative likelihood SE's for display
  
  lhsd_1 <- reactive({
    
    if (est_type() == 1) {
      
      if (rates_avail() == 1){
        
        round(
          sqrt((1 / hr_a()) + (1 / hr_b())),
          3)
        
      } else {
        
        NA
        
      }
      
    } else if (est_type() == 2) {
      round(
      sqrt(
        ((1 / (or_a() + 0.5)) + 
           (1 / (or_b() + 0.5)) + 
           (1 / (or_c() + 0.5)) + 
           (1 / (or_d() + 0.5)))
      ),
      3)
    } else if (est_type() == 3) {
      round(
        sqrt(
          (or_c() / or_a() * (or_a() + or_c())) + 
          (or_d() / or_b() * (or_b() + or_d()))  
          ),
        3
      )
    }
  })
  
  lhsd_2 <- reactive({
    
    if (est_type() == 1) {
      round(
        (log(upper_ci()) - log(pt_est())) / qnorm(likelihood_p()),
        3)
      
    } else if (est_type() == 2) {
      
      round(
      sqrt(
        ((1 / (or_a())) + 
           (1 / (or_b())) + 
           (1 / (or_c())) + 
           (1 / (or_d())))
      ),
      3)
    } else if (est_type() == 3) {
      
      NA # No secondary option for RR for the time-being
      
    }
  })
  
  lhsd_3 <- reactive({
    
    if (se_avail() == 1) {
      
      if (est_type() == 1){
        
        round(se_hr(), 3)
        
      } else if (est_type() == 2){
        
        round(se_or(), 3)
        
      } else if (est_type() == 3){
        
        round(se_or(), 3) # Same input used for SE OR/RR
        
      }
      
    } else {
      
      NA
      
    }
  })
  
  output$lhsd_1 <- renderUI({
    tagList("Equation 1 SE value: ", lhsd_1()) 
  })
  
  output$lhsd_2 <- renderUI({
    tagList("Equation 2 SE value: ", lhsd_2()) 
  })
  
  output$lhsd_3 <- renderUI({
    tagList("Reported SE value: ", lhsd_3())
  })

  
  output$eqn_1a <- renderUI({
    withMathJax(paste0("SE Equation 1:", "$$s = \\sqrt{\\frac{1}{E_1} + \\frac{1}{E_2}}$$"))
  })
  
  output$eqn_2a <- renderUI({
    withMathJax(paste0("SE Equation 2:", "$$s = \\frac{log(UCI) - log(HR)}{qnorm(p)}$$"))
  })
  
  output$eqn_1b <- renderUI({
    withMathJax(paste0("SE Equation 1:", "$$s = \\sqrt{\\frac{1}{a + \\frac{1}{2}} + \\frac{1}{b + \\frac{1}{2}} + \\frac{1}{c + \\frac{1}{2}} + \\frac{1}{d + \\frac{1}{2}}}$$"))
  })
  
  output$eqn_2b <- renderUI({
    withMathJax(paste0("SE Equation 2:", "$$s = \\sqrt{\\frac{1}{a} + \\frac{1}{b} + \\frac{1}{c} + \\frac{1}{d}}$$"))
  })
  
  output$eqn_1c <- renderUI({
    withMathJax(paste0("SE Equation 1:", "$$s = \\sqrt{\\frac{c}{a(a + c)} + \\frac{d}{b(b + d)}}$$"))
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
  x <- seq(-5, 3, by = 0.01)
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
        y = exp(y))
      # %>%
      # filter(
      #   dist %in% dist_list()
      # )
      
  })
  
  # Max xlim
  max_x <- reactive({
    
    # Ensure that at least up to 2 is shown
    max(likelihood_theta() + (exp(likelihood_theta()) * 1.5), 2)
    
  })
  
  # Credible interval
  ci_in <- reactive({input$ci})
  
  lower_cred <- reactive({
    round(exp(qnorm((1 - (ci_in()/100)) / 2, post_theta(), post_sd())), 2)
  })
  
  upper_cred <- reactive({
    round(exp(qnorm(1 - (1 - (ci_in()/100)) / 2, post_theta(), post_sd())), 2)
  })
  
  mid_cred <- reactive({
    round(exp(qnorm(0.5, post_theta(), post_sd())), 2)
  })
  
  # HR Post
  hr_post <- reactive({input$hr_post})
  
  # Colors
  like_col <- brewer.pal(3, "Dark2")[1]
  post_col <- brewer.pal(3, "Dark2")[2]
  prior_col <- brewer.pal(3, "Dark2")[3]
  
  # Posterior alpha (toggle)
  post_alpha <- reactive({as.numeric(input$post_alpha)})
  
  # Credible Interval alpha (toggle)
  cred_alpha <- reactive({as.numeric(input$cred_alpha)})
  
  # Dynamic Plot
   output$distPlot <- renderPlot({
     plot_data() %>%
       ggplot(aes(x = x, y = y, group = dist)) + 
       geom_vline(xintercept = 1, linetype = "dashed",
                  color = "grey50", alpha = 0.75,
                  size = 0.75) + 
       geom_line(aes(color = dist),
                 size = 1.1) + 
       geom_ribbon(data = plot_data() %>%
                     filter(dist == "posterior",
                            x < hr_post()),
                   aes(ymin = 1, ymax = y, x = x),
                   alpha = post_alpha() * 0.5, fill = post_col) + 
       geom_vline(xintercept = hr_post(),
                  color = post_col,
                  size = 0.75, linetype = "dashed", 
                  alpha = post_alpha() * 0.75) + 
       geom_segment(aes(y = 1, yend = 1,
                        x = lower_cred(), xend = upper_cred()),
                    size = 1.5,
                    alpha = cred_alpha() * 1) + 
       geom_point(aes(x = mid_cred(), y = 1),
                  size = 2.5,
                  alpha = cred_alpha() * 1) + 
       scale_color_brewer(name = NULL, type = "qual", palette = "Dark2",
                          breaks = c("likelihood", "prior", "posterior"),
                          labels = c("Prior", "Likelihood", "Posterior")) + 
       xlim(0, max_x()) + 
       labs(
         x = long_lab(),
         y = "Probability Density"
       ) + 
       annotate(geom = "text",
                label = paste("Posterior probability ", short_lab(),
                              paste(" < 1: ", 
                              round(pnorm(log(1), post_theta(), post_sd(), 
                                          lower.tail = TRUE), 3), sep = ""), sep = ""),
                x = max_x(), y = max(plot_data()$y), hjust = 1,
                fontface = "bold") + 
       annotate(geom = "text",
                label = paste("Posterior probability ", short_lab(),
                              paste(" < ", hr_post(), sep = ""), 
                              paste(": ", round(pnorm(log(hr_post()), post_theta(), post_sd(),
                                                          lower.tail = TRUE), 3), sep = ""), sep = ""),
                x = max_x(), y = max(plot_data()$y) - max(plot_data()$y/25), hjust = 1,
                fontface = "bold") + 
       annotate(geom = "text",
                label = paste("Posterior median (", ci_in(),
                              paste("% credible interval): ",
                                    mid_cred(),
                                    paste(" (", lower_cred(), sep = ""),
                              paste(", ", upper_cred(), sep = ""),
                              paste(")", sep = ""), sep = ""), sep = ""),
                x = max_x(), y = max(plot_data()$y) - (2 * max(plot_data()$y)/25), hjust = 1,
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
   url_methods <- a("Wijeysundera et al.", 
                    href="https://www.ncbi.nlm.nih.gov/pubmed/18947971")
   
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
     tagList("This is an interactive tool for Bayesian re-analysis clinical trials. Code by Dan Lane", url_dlt, "and adapted by Ben Andrew", url_bat, "Methods adapted from", url_methods) 
   })
   output$step_1 <- renderUI({
     tagList("Enter basic results from the clinical trial on the Study Data tab. This will allow for approximation of the likelihood distribution.") 
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
   output$tech_notes_1 <- renderUI({
     withMathJax(paste0("The likelihood is constructed as a normal distribution: ",
                        "$$L \\sim N(\\theta, s)$$",
                        "where",
                        "$$\\theta = log(HR)$$",
                        "$$s = \\widehat{SE}_{log(HR)}$$",
                        "SE is estimated using equations 1 and 2 below. When enough data is supplied, equation 1 is preferentially used. For reference purposes, all estimates are displayed.")) 
   })
   output$tech_notes_2 <- renderUI({
     withMathJax(paste0("The likelihood is constructed as a normal distribution: ",
                        "$$L \\sim N(\\theta, s)$$",
                        "where",
                        "$$\\theta = log\\frac{(a + \\frac{1}{2})(d + \\frac{1}{2})}{(b + \\frac{1}{2})(c + \\frac{1}{2})}$$",
                        "$$s = \\widehat{SE}_{log(OR)}$$",
                        "SE is estimated using equations 1 and 2 below. When enough data is supplied, equation 1 is preferrentially used. For reference purposes, all estimates are displayed.")) 
   })
   output$tech_notes_3 <- renderUI({
     withMathJax(paste0("The likelihood is constructed as a normal distribution: ",
                        "$$L \\sim N(\\theta, s)$$",
                        "where",
                        "$$\\theta = log\\frac{\\frac{a}{a + c}}{\\frac{b}{b + d}}$$",
                        "$$s = \\widehat{SE}_{log(RR)}$$",
                        "SE is estimated using equation 1 below. For reference purposes, the reported SE (if available) and estimated SE are both displayed.")) 
   })
}
# Run the application 
shinyApp(ui = ui, server = server)
