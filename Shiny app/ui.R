################################################################################
# User interface script for Scaling up uncertain predictions Shiny application
# Shiny app to accompany to paper: https://doi.org/10.1101/2020.05.26.117200
# James Orr - February 2021 
################################################################################

# Required packages 
library(shiny)            # For shiny app
library(rclipboard)       # For the copy to clipboard tool


fluidPage(
  
  # For equations in written in LaTeX 
  withMathJax(),
  
  # To set title of page (for browsers)
  title = "Scaling up uncertain predictions",
  
  ############# Title  #############
  titlePanel(h2(paste("Scaling up uncertain predictions"), align = "center")),
  
  ############# Authors  #############
  p("James Orr, Jeremy Piggott, Andrew Jackson, and Jean-François Arnoldi", 
    align = "center"),
  
  br(),
  
  sidebarLayout(
    
    ############# Sidebar  #############
    sidebarPanel(
      p("Using this Shiny app you can interactively explore the 
      results for our paper:",
        a(href="https://doi.org/10.1101/2020.05.26.117200",
      "\"Scaling up uncertain predictions to higher levels of organisation tends 
      to underestimate change.\""), "Our research shows that the process of 
      scaling up predictions to higher levels of organization has 
      a surprising consequence: it tends to systematically underestimate the 
      magnitude of system-level change, an effect whose signiffcance grows with 
      the system's dimensionality. This stems from a geometrical observation:", 
      em("in high dimensions there are more ways to be more different, than ways 
      to be more similar."),"", 
        align="justify"),
      br(),
      strong("Geometric Approach"),
      p("Use our geometric model to explore how prediction error influences the 
      bias towards underestimation, even for a two-dimensional system"),
      br(),
      strong("Theoretical Expectations"),
      p("Then see how increasing dimesions will also increase the bias towards
        underestimation."),
      br(),
      strong("Implementation"),
      p("Finally, you can copy code (for R, Python or MATLAB) that can be used 
      to apply our theoretical expectations."),
      br(),
      p("Code:",
        a(href="https://github.com/jamesaorr/scaling-up-uncertain-predictions",
        "GitHub"),""),
      p("Preprint:",a(href="https://doi.org/10.1101/2020.05.26.117200", 
                      "bioRxiv, recommended by PCI Ecology"),"")
    ),
    
    mainPanel(
      tabsetPanel(
        
        
        ############################## Tab 1  ##################################
        tabPanel("Geometric Approach", 
                 fluidRow(br(),
                          br(),
                          br(),
                          column(12, plotOutput("geometric2D", width = "100%")),
                          br(),
                          column(12, sliderInput("rel_error", "Error:", 
                                                 min = 0, max = 2.0, 
                                                 value = 0.8, step= 0.05),
                                 align = "center"),
                          br(),
                          column(11, offset = .5, p("
        The centre of the blue circle is the initial state and its radius is the
predicted magnitude of change. The centre of the red circle is the predicted 
state and its radius is the magnitude of error made by the prediction. By 
definition, final states fall on the edge of the red circle. If a final state 
falls inside the blue circle then there has been an overestimation of change. 
If a final state falls outside the blue circle, as in the figure, then there has 
been an underestimation of change. As error increases, as the red circle becomes 
larger, there are more configurations leading to the underestimation of 
change.", align="justify")),
                          )),
        
        
        
        ############################## Tab 2  ##################################
        tabPanel("Theoretical Expectations",
                 fluidRow(br(),
                          br(),
                          column(6, uiOutput("equation1"), align="center"),
                          column(6, uiOutput("equation2"), align="center")),
                 
                 fluidRow(column(6, plotOutput("plot2", width = "100%")),
                          column(6, plotOutput("plot3", width = "100%"))),
                 
                 fluidRow(column(6, sliderInput("rel_error2", "Error:", 
                                                min = 0, max = 2.0, 
                                                value = 0.8, step= 0.05),
                                 align = "center"),
                          column(6, sliderInput("dimensions", "Dimensions:", 
                                                min = 2, max = 100, value = 2, 
                                                step= 1),
                                 align = "center")),
                 
                 fluidRow(br(),
                          column(11, offset = .5, p("
        The median expectation of the relationship between relative error 
and relative underestimation is the same for all dimensions above one. However, 
as dimensionality increases, the width of the distribution decreases and 
converges towards its median, which effectively increases the probability of 
underestimation. With a fixed magnitude of error, the probability of 
underestimation will increase as dimensionality increases.", align="justify"))
                 )),
        
        
        
        ############################## Tab 3  ##################################
        tabPanel("Implementation",
                 fluidRow(br(),
                          br(),
                          column(6, uiOutput("equation3"), align="center"),
                          column(6, uiOutput("equation4"), align="center")),
                 
                 fluidRow(br(),
                          column(6, p("Median expectation",
                                      style="font-family:Times New Roman;
                                      font-size:130%"), align="center"),
                          column(6, p("Probability of underestimation",
                                      style="font-family:Times New Roman;
                                      font-size:130%"), align="center")),
                 
                 fluidRow(
                   br(),
                   column(12, selectInput(
                     inputId = "language",
                     label = "Language",
                     choices = c("R", "Python", "MATLAB"),
                     selected = "R",
                     width = "25%"), align = "center"),
                   
                   column(6, rclipboardSetup(),
                          uiOutput("median"), align = "center"),
                   column(6, rclipboardSetup(),
                          uiOutput("prob"), align = "center"),
                 
                   column(6, uiOutput("clip1"), align = "center"),
                   column(6, div(style = "font-family:courier", 
                                 uiOutput("clip2")), align = "center")),
                   

                 fluidRow(
                   br(),
                   br(),
                   column(11, offset = 1.5, p("Code required to apply (i) the 
median expectation of the relationship between relative error and relative 
underestimation, and (ii) the function predicting the probability of 
underestimation (for S=10)."), 

p("- The ",strong("R code")," and ",strong("MATLAB code"),"can be implemented 
directly without any additional packages."), 

p("- The ",strong("Python code")," requires ",
code("matplotlib.pyplot")," to be imported as \"plt\", ", code("numpy")," 
to be imported as \"np\" and the ", code("betainc")," function from ",
code("scipy.special"),"", align = "justified"))
                   ))
        
      )
    )
  )
)