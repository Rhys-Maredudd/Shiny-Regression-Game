library(shiny)
library(shinyWidgets)
library(shinyvalidate)
library(bslib)
library(bsicons)
library(bsplus)
library(tidyverse)
library(plotly)
library(gt)
library(broom)



#### Setting UI

ui <- page_navbar(
  ## Challenge Title
  title = "Regression Challenge",
  fillable = FALSE,
  nav_spacer(),
  nav_panel(
    title = "Overview",
    fillable = FALSE,
    card(
      card_header(markdown("**Game Aims**")),
      card_body(markdown("The aim of this game is to build confidence in reading regression summary tables. Your task will be to read the summary tables to shape the regression lines in the plots.
<br>
<br>
With each level, we will build in difficulty; starting with a simple univariate regression, and building to a ***continuous*** * ***categorical*** interaction effect.
<br>
<br>
To get the most out of your learning, be sure to play around with each interactive plot after solving each challenge. Your goal should be to get a feeling for how the regression table translates into each plot.
<br>
<br>
By working through these challenges, you'll ***regress*** to being a stats ninja in no time (sorry, not sorry...).
<br>
<br>
Along the way, do reflect on the changes we see in each plot - and consider where else in research we might need to be wary of the [***Simpson Paradox***](https://en.wikipedia.org/wiki/Simpson%27s_paradox).")
                ))),
 #### First Challenge
  nav_panel(
    title = "Challenge 1",
    fillable = FALSE,
    card(
      card_header("Challenge 1"),
      card_body(markdown("Here is the regression model for this challenge:
      <br>
      
-   `lm(Petal.Length ~ Sepal.Width , data = iris)`
                     

Everything is almost ready to go, but unfortunately our plot is broken!
<br>
<br>
We need your help to correctly set the the regression line of the plot, using the **sliders** and the **summary table**."))
    ),
    
  ## Sidebar panel for challenge 1 inputs ----
  layout_sidebar(
    
   sidebar = sidebar(
  
    # intercept slider
    sliderInput("intercept", "What is the intercept?", 
                min = .0, max = 10, value = 0, step = .1),
    
    # br() to bring extra vertical spacing
    br(),
    
    # slope slider
    sliderInput("slope", "What is the slope?", 
                min = -3, max = 3, value = 0, step = 0.1)
  ),
  
  # Main panel for displaying outputs
  # Output: A tabset that contains a table panel and plot panel
  
  navset_card_underline(
    id = "nav_tab_1",
    title = "Broken Plot",
    
    # Plot Panel
    nav_panel("Broken Plot",
              plotlyOutput("Plot")
    ),
    
    # Regression Table Panel
    nav_panel("Regression Results",
              tableOutput("summary_table")
    ),
    
    # Submit Button Panel
    nav_panel("Check Plot",
              actionButton(inputId = "check_answer1",
                           label = "Check the Plot",icon("edit"))
    )
  )
  ))
  ,
  ## Challenge_2
  nav_panel(
    title = "Challenge 2",
    card(
    card_header("Challenge 2"),
    card_body(markdown(
    "Those of you with the keen eye will have noticed that perhaps a negative association isn't quite the best fit across the 3 species. Let's see what happens when we account for these species in the regression model!
<br>
<br>
Interpreting the data for this model will be a little trickier, as we are working `Species`, which is categorical. Behind the scenes in analyses of these sorts, categorical variables are dummy coded (i.e., **1** for `Setosa`, **0** for **not** `Setosa`). The regression table will show you the change that occurs as each layer goes from **0** to **1** (That is, think of the named species in the table as being relative to *reference* species).
<br>
<br>
Considering that `virginica` and `versicolour` are in the regression table for `Species`, think about how you might interpret the `intercept` value and what it might represent in this context.
<br>
<br>
Here is the regression model used for this challenge:

-   `lm(Petal.Length ~ Sepal.Width + Species, data = iris)`

The challenge here is to set the slope and intercept values for the above regression model using the **summary table** and the **sliders**:"
    ))),
    layout_sidebar(
    sidebar = sidebar(

      # intercept slider
      sliderInput("intercept_a2", "What is the intercept for Setosa?",
                  min = -1, max = 5, value = 0, step = .1),

      # br() for vertical line break
      br(),


      sliderInput("intercept_b2", "What is the intercept for Versicolor?",
                  min = -1, max = 5, value = 0, step = .1),

      # br() for vertical line break
      br(),

      sliderInput("intercept_c2", "What is the intercept for Virginica?",
                  min = -1, max = 5, value = 0, step = .1),

      # br() for vertical line break
      br(),

      sliderInput("slope_a2", "What is the slope?",
                  min = -2, max = 2, value = 0, step = 0.1)
    ),
    # Main panel for displaying outputs
    # Output: A tabset that contains a table panel and plot panel

    navset_card_underline(
      id = "nav_tab_2",
      title = "The Second Broken Plot",

      # Plot Panel
      nav_panel("Broken Plot #2",
                plotlyOutput("Plot2")
      ),



      # Regression Table Panel
      nav_panel("Regression Results",
                tableOutput("summary_table2")
      ),

      # Submit Button Panel
      nav_panel("Check Plot",
                actionButton(inputId = "check_answer2",
                             label = "Check the Plot",icon("edit"))
      )
    )
  )
  ),
  
  #### Multiple choice Q's
  nav_panel(
    title = "Challenge 3",
    card(
    card_header(markdown("**So what is the association between Sepal Width and Petal Length?**")),
    card_body(
      markdown("With a keen eye may have noticed that we contradictory associations between `Sepal.Width` and `Petal.Length` in our analyses... 
       <br>        
               Under some circumstances it's negative, and in others it's positive. This leads us to the important question of how should we interpret this?")
      ),
    br(),
      radioButtons("radio_1",
                           label = markdown("**Q1) What is the association between `Sepal.Width` and `Petal.Length`?**"),
                           width = "800px",
                           choices = c(
                             'Positive' ,
                             'Negative' ,
                             'It depends...' ,
                             'There is no association'#,
                            # textOutput("txt")
                           )),
              actionButton(inputId = "check_answer_q1",
                           label = "submit",icon("edit") )
    ),
  br(),
  #### Question 2
  card(
    card_header(markdown("**Ehhh?!**")),
    card_body(markdown("**So what is going on here?** 
    <br>
    <br>
    ***Why does it depend?!***
    <br>
    <br>
Well the answer requires a degree of pedantic pettifogging and to make sure we're asking the **correct question** to align with the analysis inference.
<br>
<br>
So let's try again with rephrased questions to make sure the answers can be meaningful."),
              br(),
              ),
    radioButtons("radio_2",
                 label = markdown("**Q2) If the association between `Sepal.Width` and `Petal.Length` is negative, what is the correct question?**"),
                 width = "800px",
                 choices = c(
                   'A) What is the association between Sepal.Width and Petal.Length across all Species?',
                   'B) What is the association between Sepal.Width and Petal.Length within each Species?' 
                 )),
    actionButton(inputId = "check_answer_q2",
                 label = "submit",icon("edit"), size = "small" ),
  
  br(),
  radioButtons("radio3",
               label = markdown("**Q3) If the association between `Sepal.Width` and `Petal.Length` is positive, what is the correct question?**"),
               width = "800px",
               choices = c(
                 'A) What is the association between Sepal.Width and Petal.Length across all Species?',
                 'B) What is the association between Sepal.Width and Petal.Length within each Species?')),
             
  actionButton(inputId = "check_answer_q3",
               label = "submit",icon("edit"), size = "small" ))),
  
  #### Challenge 4 UI
  nav_panel(
    title = "Challenge 4",
    card(
      card_header("Challenge 4"),
      card_body(markdown(
"Looks like we're getting closer to understanding the data. However, it is odd that every `species` has the same slope. And because every `species` has its own slope, poor `Setosa` is defying all laws of nature as it somehow predicting it to have a **negative** `Petal.Length` until the `Sepal.Width` grows beyond ***0.4mm***.(Go on - check by hovering the mouse cursor on the `orange` regression line from Challenge 2).
<br>
<br>
To resolve these strange predictions, the model has been set with an interaction. This gives every `species` it's own slope. Your task is to use the regression table to set the slope and intercept for all **3** of the flower species.
<br>
<br>
Here is the regression model for this challenge:

-   `lm(Petal.Length ~ Sepal.Width * Species, data = iris)`

Here you will need to compare the `Species` level variables to the reference level of the `intercept`, as well as comparing the interaction terms to the `Sepal.Width`. When reading a regression table that includes a *continuous* * *categorical* interaction, the coefficent value of our *continuous* predictor variable will only apply to the **reference** value of our *categorical* variable. To help, think of the named species and interaction terms as values that are *relative* to the **intercept** and to `Sepal.Width`. You will need to do some some manual calculating here - simple addition is all you need.
<br>
<br>
Set the *slope* and *intercept* for each *species* using the **regression table** and the **sliders**:"
      ))),
    layout_sidebar(
    ## Challenge Title
    title = "Challenge 4",
    
    ## Sidebar panel for plot inputs ----
    
    sidebar = sidebar(accordion(
      accordion_panel(
        "Setosa", icon = bsicons::bs_icon("sliders"),
        sliderInput("intercept_a3", "What is the intercept for Setosa?",
                    min = -.01, max = 5, value = 0, step = .1),
        br(),
        sliderInput("slope_a3", "What is the slope for Setosa?", 
                    min = -2, max = 2, value = 0, step = 0.1),
        br()
      ),
      accordion_panel(
        "Versicolor", icon = bsicons::bs_icon("sliders"),
        sliderInput("intercept_b3", "What is the intercept for Versicolor?",
                    min = -.01, max = 5, value = 0, step = .1),
        br(),
        sliderInput("slope_b3", "What is the slope for Versicolor?", 
                    min = -2, max = 2, value = 0, step = 0.1),
        br()
      ),
      accordion_panel(
        "Virginica", icon = bsicons::bs_icon("sliders"),
        sliderInput("intercept_c3", "What is the intercept for Virginica?",
                    min = -.01, max = 5, value = 0, step = .1), 
        br(),
        sliderInput("slope_c3", "What is the slope for Virginica?", 
                    min = -2, max = 2, value = 0, step = 0.1),
        br()
      ))),
    
    # Main panel for displaying outputs
    # Output: A tabset that contains a table panel and plot panel
    
    navset_card_underline(
      id = "nav_tab_3",
      title = "The Third Broken Plot",
      
      # Plot Panel
      nav_panel("Broken Plot #3",
                plotlyOutput("Plot3")
      ),
      
      # Regression Table Panel
      nav_panel("Regression Results",
                tableOutput("summary_table3")
      ),
      
      # Submit Button Panel
      nav_panel("Check Plot",
                actionButton(inputId = "check_answer3",
                             label = "Check the Plot",icon("edit"))
      )
    )
  )
  ),
nav_panel(
  title = "Epilogue",
  fillable = FALSE,
  card(
    card_header(markdown("**Learning Reflection**")),
    card_body(markdown("You've made it to the end! Give yourself a good pat on the back, as we've covered some tricky concepts here, and wrestled with the Simpson's Paradox. 
<br>
Through this app, we have covered:
<br>
-   How to interpret and visualise the intercept and predictor coefficients in a one predictor regression.
-   How to interpret and visualise the intercept and predictor coefficients when we add a categorical covariate to our linear regression.
-   Why an interaction term might be needed to make more accurate and valid predictions from our data.
-   How to interpret and visualise the intercept and predictor coefficients when we add an interaction term between our categorical and continuous variables.
-   The importance of having the correct question when trying to resolve the Simpson's Paradox.
<br>
Well done!")
    )))

)

server <- function(input, output) {
  
  # Data set, model and regression results
    x <- seq(from = 0, to = 4.4, by = .1)
    y <- seq(from = 0, to = 10, by = .1 )
  m1 <- lm(Petal.Length ~ Sepal.Width, data = iris)
  reg_summary <- broom::tidy(m1)
  # model_fit_stats <- broom::glance(m1)
  
  # Plot code
  # thematic::thematic_shiny()
  output$Plot <- plotly::renderPlotly({
    ggplot(mapping = aes(x = x,
                         y =  ((input$slope*x) + input$intercept))) +
      geom_point(data = iris, aes(x = Sepal.Width, y = Petal.Length,
                                  color = Species, shape = Species), alpha = .7)+
      geom_line(color = "grey1") +
      geom_vline(xintercept = 0, color = "black") +
      labs(y = "Petal Length (mm)",
           x = "Sepal Width (mm)") +
      coord_cartesian(xlim = c(0,4.4),
                      ylim = c(0,10)) +
      theme(legend.position = "bottom") + 
      theme_bw()
  })
  
  # Table code
  
  output$summary_table <- render_gt({
    return(
      reg_summary %>%
        gt(rowname_col = "term") %>% 
        tab_header(title = "Regression Summary",
                   subtitle = "Use this information to set the slope on the plot") %>%
        opt_align_table_header(align = "left") %>%
        cols_label(estimate = "Estimate",
                   std.error = md("*se*"),
                   statistic = md("*t*"),
                   p.value = md("*p*")) %>%   
        cols_align(align = "center") %>%
        fmt_number(columns = c(estimate, std.error, statistic), decimals = 1) %>% 
        fmt_number(columns = p.value, 
                   decimals = 3)
    )
  })
  
  ### Answer check
  
  iv <- InputValidator$new()
  
  iv$add_rule("intercept", function(intercept) {
    if (intercept != 9.1){""}
  })
  
  iv$add_rule("slope", function(slope) {
    if (slope != -1.7){""}
  })
  
  # # Validation of answer check  
  observeEvent(input$check_answer1, {
    if (iv$is_valid()) {
      showModal(
        modalDialog(title = "Congratulations!",
                    "Your have correctly fitted the regression line for Challenge 1",
                    size = "s", fade = FALSE
        )
      )
    } else {
      iv$enable() # Start showing validation feedback
      showNotification(
        "Try again!",
        id = "check_answer1", type = "error")
    }
  }
  )
  
  ###### Challenge 2
  
  m2 <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
  reg_summary2 <- broom::tidy(m2)
  #model_fit_stats2 <- broom::glance(m2)
  
  # Plot output
  # thematic::thematic_shiny()
  output$Plot2 <- renderPlotly({
    ggplot(mapping = aes(x = x,  y = y )) +
      geom_point(data = iris, 
                 aes(x = Sepal.Width, y = Petal.Length,
                     color = Species, shape = Species), alpha = .7)+
      geom_line(color = "orange", linetype = "longdash", alpha = .7,
                aes(x, y = (input$slope_a2*x) + input$intercept_a2)) +
      geom_line(color = "green", linetype = "dotdash", alpha = .7, aes(x, y = (input$slope_a2*x) + input$intercept_b2)) +
      geom_line(color = "blue", linetype = "dotted", alpha = .7,
                aes(x, y = (input$slope_a2*x) + input$intercept_c2)) +
      geom_vline(xintercept = 0, color = "black") +
      labs(y = "Petal Length (mm)",
           x = "Sepal Width (mm)") +
      coord_cartesian(xlim = c(0,4.4),
                      ylim = c(0,10))+ 
      theme_bw()
  })
  
  # Regression Summary Table output
  output$summary_table2 <- render_gt({
    return(
      reg_summary2 %>%
        gt(rowname_col = "term") %>% 
        tab_header(title = "Regression Summary",
                   subtitle = "Use this information to set the slope on the plot") %>%
        opt_align_table_header(align = "left") %>%
        cols_label(estimate = "Estimate",
                   std.error = md("*se*"),
                   statistic = md("*t*"),
                   p.value = md("*p*")) %>%   
        cols_align(align = "center") %>%
        fmt_number(columns = c(estimate, std.error, statistic), 
                   decimals = 1) %>% 
        fmt_number(columns = p.value, 
                   decimals = 3) %>%
        tab_footnote(
          footnote = md("This is the reference point for our `Species` varaible"),
          locations = cells_stub(rows = "(Intercept)")
        ) %>% 
        tab_source_note(
          source_note = md("***Clue***: Remember that the estimate is showing us the ***B***, when all other values are held at 0")
        )
    )
  })
  
  
  ### Answer check  
  
  iv2 <- InputValidator$new()
  
  iv2$add_rule("intercept_a2", function(intercept_a2) {
    if (intercept_a2 != -.2){""} 
  })
  
  
  iv2$add_rule("intercept_b2", function(intercept_b2) {
    if (intercept_b2 != 2.9){""} 
  })
  
  
  iv2$add_rule("intercept_c2", function(intercept_c2) {
    if (intercept_c2 != 4.1){""} 
  })
  
  iv2$add_rule("slope_a2", function(slope_a2) {
    if (slope_a2 != .5){""} 
  })
  
  
  observeEvent(input$check_answer2, {
    if (iv2$is_valid()) {
      showModal(
        modalDialog(title = "Congratulations!",
                    "Your have correctly fitted the regression line for Challenge 2.",
                    size = "s", fade = FALSE
        )
      )
    } else {
      iv2$enable() # Start showing validation feedback
      showNotification(
        "Try again!",
        id = "check_answer2", type = "error")
    }
  }
  )
  
  #### Challenge 3 Q1
  
    renderText({ input$radio_1 })
    
    ivq1 <- InputValidator$new()
    
    ivq1$add_rule("radio_1", function(radio_1) {
      if (radio_1 != 'It depends...'){""}
    })
    
    #
    observeEvent(input$check_answer_q1, {
      if (ivq1$is_valid()) {
        showModal(
          modalDialog(title = "Congratulations!",
                      markdown("The answer *to the question* depends **on the question**.
<br>
Currently the wording is too vague, therefor: it depends!"),
                      size = "s", fade = FALSE
          )
        )
      } else {
        ivq1$enable() # Start showing validation feedback
        showNotification(
          "Try again!",
          id = "check_answer_q1", type = "error")
      }
    }
    )
    
    #### Challenge 3 Q2
    
    renderText({input$radio_2})
    ## Answer check
    ivq2 <- InputValidator$new()
    
    ivq2$add_rule("radio_2", function(radio_2) {
      if (radio_2 != 'A) What is the association between Sepal.Width and Petal.Length across all Species?'){""}
    })
    
    observeEvent(input$check_answer_q2, {
      if (ivq2$is_valid()) {
        showModal(
          modalDialog(title = "Congratulations!",
                      markdown("When the association is negative it is because we are interested in the association between `Sepal.Width` and `Petal.Length` across all Iris species"),
                      size = "s", fade = FALSE
          )
        )
      } else {
        ivq2$enable() # Start showing validation feedback
        showNotification(
          "Try again!",
          id = "check_answer_q2", type = "error")
      }
    }
    )
    
  #### Challenge 3 Q3
  
    renderText({input$radio3})
    
    ivq3 <- InputValidator$new()
    
    ivq3$add_rule("radio3", function(radio3) {
      if (radio3 != 'B) What is the association between Sepal.Width and Petal.Length within each Species?'){""}
    })
    
    observeEvent(input$check_answer_q3, {
      if (ivq3$is_valid()) {
        showModal(
          modalDialog(title = "Congratulations!",
                      markdown("In this condition it is positive - within each Iris species, we see that Petal Length **increasess** as Sepal Width **increases**"),
                      size = "s", fade = FALSE
          )
        )
      } else {
        ivq3$enable() # Start showing validation feedback
        showNotification(
          "Try again!",
          id = "check_answer_q3", type = "error")
      }
    }
    )
    
    
  #### Challenge 4
  
  
  m3 <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
  reg_summary3 <- tidy(m3)
  
  # Plot output
  # thematic::thematic_shiny()
  output$Plot3 <- renderPlotly({
    ggplot(mapping = aes(x = x,
                         y = y 
    )) +
      geom_point(data = iris, aes(x = Sepal.Width, y = Petal.Length,
                                  color = Species, shape = Species), alpha = .7)+
      geom_line(color = "orange", linetype = "longdash", alpha = .7,
                aes(x, y = (input$slope_a3*x) + input$intercept_a3)) +
      geom_line(color = "green", linetype = "dotdash", alpha = .7, aes(x, y = (input$slope_b3*x) + input$intercept_b3)) +
      geom_line(color = "blue", linetype = "dotted", alpha = .7,
                aes(x, y = (input$slope_c3*x) + input$intercept_c3)) +
      geom_vline(xintercept = 0, color = "black") +
      labs(y = "Petal Length (mm)",
           x = "Sepal Width (mm)") +
      coord_cartesian(xlim = c(0,4.4),
                      ylim = c(0,10))+ 
      theme_bw()
  })
  
  
  # Regression Summary Table output
  output$summary_table3 <- render_gt({
    return(
      reg_summary3 %>%
        gt(rowname_col = "term") %>% 
        tab_header(title = "Regression Summary",
                   subtitle = "Use this information to set the slope on the plot") %>%
        opt_align_table_header(align = "left") %>%
        cols_label(estimate = "Estimate",
                   std.error = md("*se*"),
                   statistic = md("*t*"),
                   p.value = md("*p*")) %>%   
        cols_align(align = "center") %>%
        fmt_number(columns = c(estimate, std.error, statistic), 
                   decimals = 1) %>% 
        fmt_number(columns = p.value, 
                   decimals = 3) %>%
        tab_footnote(
          footnote = md("This is the reference point for our `Species` varaible"),
          locations = cells_stub(rows = "(Intercept)")
        ) %>% 
        tab_source_note(
          source_note = md("***Clue***: Remember that the estimate is showing us the *B*, when all other values are held at 0")
        )
    )
  })
  
  ### Answer check
  iv3 <- InputValidator$new()
  
  iv3$add_rule("intercept_a3", function(intercept_a3) {
    if (intercept_a3 != 1.2){""} 
  })
  
  iv3$add_rule("slope_a3", function(slope_a3) {
    if (slope_a3 != .1){""} 
  })
  
  iv3$add_rule("intercept_b3", function(intercept_b3) {
    if (intercept_b3 != 2){""} 
  })
  
  iv3$add_rule("slope_b3", function(slope_b3) {
    if (slope_b3 != .9){""} 
  })
  
  iv3$add_rule("intercept_c3", function(intercept_c3) {
    if (intercept_c3 != 3.5){""} 
  })
  
  iv3$add_rule("slope_c3", function(slope_c3) {
    if (slope_c3 != .7){""} 
  })
  
  observeEvent(input$check_answer3, {
    if (iv3$is_valid()) {
      showModal(
        modalDialog(title = "Congratulations!",
                    "Your have correctly fitted the regression line for Challenge 3!",
                    size = "s", fade = FALSE
        )
      )
    } else {
      iv3$enable() # Start showing validation feedback
      showNotification(
        "Try again!",
        id = "check_answer3", type = "error")
    }
  }
  )
  
}

# Run App!
shinyApp(ui = ui, server = server)