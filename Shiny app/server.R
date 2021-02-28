################################################################################
# Server script for Scaling up uncertain predictions Shiny application
# Shiny app to accompany to paper: https://doi.org/10.1101/2020.05.26.117200
# James Orr - February 2021 
################################################################################

# Required packages 
library(shiny)             # For shiny app
library(plotrix)           # For plot.circle() function



function(input, output) {
  
  #################### Geometric Approach - 2D+3D ###################### 
  output$geometric2D <- renderPlot({
    par(pty="s", mar = c(5, 0, 0, 0))
    plot(1, type="n", xlab="", ylab="", 
          xlim=c(-12, 12), ylim=c(-12, 12), axes=F, cex.lab = 1.25)
    mtext("Component 1", side = 1, line = 1, cex = 1.5, 
          family="Times New Roman")
    mtext("Component 2", side = 2, line = 1, cex = 1.5, 
          family="Times New Roman")
    text(0, 12, 
          paste0("Proportion of configurations\nleading to underestimation: "
                ,round((1. -0.5 * pbeta((1-(input$rel_error*input$rel_error)/4), 
                                  ((2 - 1)/2), 0.5)), 2),""),
                  family="Times New Roman", cex = 1.1)
    arrows(x0 = c(-12, -12),
            x1 = c(12, -12),
            y0 = c(-12, -12),
            y1 = c(-12, 12),
            lwd = 1.75, code = 2,
            angle = 20)
    initial = -5
    prediction = 5
      
    # Converting relative error into coordinates in 2D 
    rel_error = input$rel_error
    error = prediction*rel_error
    realised = sqrt((error*error)/2)
  
    # Circles and points
    draw.circle(initial,0,
                prediction,
                lty=1,
                lwd=2, 
                border=rgb(0.75,0.8,1, alpha=1),
                col=rgb(0.75,0.8,1, alpha=0.6))
    draw.circle(initial+prediction,0,
                error,
                lty=1,
                lwd=2, 
                border=rgb(1,0,0, alpha=.4),
                col=rgb(1,0,0, alpha=0.25))
    draw.circle(initial,0,
                0.2,
                lty=1,
                lwd=1, 
                border=rgb(0,0,0, alpha=1.),
                col=rgb(0,0,0, alpha=1.))
    draw.circle(initial+prediction,0,
                0.2,
                lty=1,
                lwd=1, 
                border=rgb(0,0,0, alpha=1.),
                col=rgb(0,0,0, alpha=1.))
    draw.circle(realised, realised,
                0.2,
                lty=1,
                lwd=1, 
                border=rgb(0,0,0, alpha=1.),
                col=rgb(0,0,0, alpha=1.))
    # Lines
    segments(x0 = initial, 
              x1 = initial+prediction,
              y0 = 0, 
              y1 = 0)
    segments(x0 = 0, 
              x1 = realised,
              y0 = 0, 
              y1 = realised)
    # Text
    text(initial-2, -1, labels = "Initial", cex = 1, family="Times New Roman")
    text(0+2.5, -1, labels = "Predicted", cex = 1, family="Times New Roman")
    text(realised+1.25, realised+1.25, labels = "Final", cex = 1, 
           family="Times New Roman")
    })

  
  #################### Error v Underestimation ###################### 
  output$plot2 <- renderPlot({
    par(pty="s", mar = c(5, 5, 5, 5))
    plot(1, type="n", xlab="Error", ylab="Underestimation",
         xlim=c(0, 2), ylim=c(-1, 2),
         las = 1, cex.axis = 1, cex.lab = 1.)
    mtext("Median expectation", side = 3, line = 3, cex = 1.5, 
          family="Times New Roman")
    segments(x0 = c(0, 0, 1), 
             x1 = c(2, 1, 2),
             y0 = c(0, 0, -1),
             y1 = c(2, -1, 0))
    
    # The median relationship between error and underestimation
    curve(sqrt(x*x + 1) - 1, from = 0, to = 2, add = T, col = 'red', lwd = 3)
    # Spread around the median (as a polygon)
    X = c(seq(0, 2, 0.02), seq(2, 0, -0.02))
    Y = c(sqrt(X[1:101]*X[1:101] + 1 + 2*X[1:101]/sqrt(input$dimensions)) - 1,
          sqrt(X[102:202]*X[102:202] + 1 - 2*X[102:202]/sqrt(input$dimensions))- 1)
    polygon(X,Y, col = rgb(1, 0, 0, 0.2), border = NA)
    
    # Highlighting the x value (relative error)  
    segments(x0 = input$rel_error2,
             x1 = input$rel_error2, 
             y0 = -1.1, 
             y1 = sqrt(input$rel_error2*input$rel_error2 + 1) - 1,
             col = rgb(1, 0, 0, alpha = .8),
             lwd = 2,
             lty = 2)
    points(input$rel_error2, sqrt(input$rel_error2*input$rel_error2 + 1) - 1,
           col = 'red', bg = rgb(1, 0.7, 0.7, alpha = 1), 
           cex = 1.5, pch = 21, lwd=2)
  })
  
  

  ################# Error v Probability of Underestimation ################### 
  output$plot3 <- renderPlot({
    par(pty="s", mar = c(5, 5, 5, 5))
    plot(1, type="n", xlab="Error", ylab="Probability of Underestimation", 
         xlim=c(0, 2), ylim=c(0.48, 1),
         las = 1, cex.axis = 1, cex.lab = 1.)
    mtext("Probability of underestimation", side = 3, line = 3, cex = 1.5, 
          family="Times New Roman")
    
    # Main theoretical prediction - I_x(a,b) is pbeta(x, a, b)
    curve(1. -0.5 * pbeta((1-(x*x)/4), 
                          ((input$dimensions - 1)/2),
                          0.5), 
                          from = 0, to = 2, add = T, col = 'red', lwd = 3)
    # Loop across dimensions for faded lines 
    for (n in 2:95) {
      curve(1. -0.5 * pbeta((1-(x*x)/4), 
                            ((n - 1)/2),
                            0.5), 
            from = 0, to = 2, add = T, lwd = 1, col = rgb(0.8, 0, 0, 0.2))
    }
    
    # Highlighting the x value (relative error)  
    segments(x0 = input$rel_error2,
             x1 = input$rel_error2, 
             y0 = -1.1, 
             y1 = 1. -0.5 * pbeta((1-(input$rel_error2*input$rel_error2)/4), 
                                  ((input$dimensions - 1)/2), 0.5),
             col = rgb(1, 0, 0, alpha = .8),
             lwd = 2,
             lty = 2)
    points(input$rel_error2, 
           1. -0.5 * pbeta((1-(input$rel_error2*input$rel_error2)/4), 
                           ((input$dimensions - 1)/2), 0.5),
           col = 'red', bg = rgb(1, 0.7, 0.7, alpha = 1), 
           cex = 1.5, pch = 21, lwd=2)

  })
  
  

  ################# Equations  ################### 
  output$equation1 <- renderUI({
    withMathJax(paste0("$$\\tilde{y}=\\sqrt{x^2+1}-1$$"))
  })
  
  output$equation2 <- renderUI({
    withMathJax(paste0("$$P_{>0}(x)=1-\\frac{1}{2}I_{1-\\frac{x^2}{4}} 
                       \\left( \\frac{",input$dimensions,
                       "-1}{2};\\frac{1}{2} \\right)$$"))
  })
  
  output$equation3 <- renderUI({
    withMathJax(paste0("$$\\tilde{y}=\\sqrt{x^2+1}-1$$"))
  })
  
  output$equation4 <- renderUI({
    withMathJax(paste0("$$P_{>0}(x)=1-\\frac{1}{2}I_{1-\\frac{x^2}{4}} 
                       \\left( \\frac{10-1}{2};\\frac{1}{2} \\right)$$"))
  })
  
  output$S <- renderUI({
    paste0(input)
  })
  

  
  ################# Text boxes with code  ################### 
  # Median expectation 
  output$median <- renderUI({
    if (input$language == "R"){
      div(style = "font-family:courier", textInput("Rmedian", "", 
                "curve(sqrt(x*x + 1) - 1, from = 0, to = 2)"))
    }
    else if (input$language == "Python"){
      div(style = "font-family:courier", textInput("Pmedian", "", 
                "plt.plot(np.linspace(0, 2), np.sqrt(np.linspace(0, 2)**2+1)-1)"))
    }
    
    else if (input$language == "MATLAB"){
      div(style = "font-family:courier", textInput("Mmedian", "", 
                "fplot(@(x) sqrt(x.^2 + 1) - 1, [0, 2])"))
    }
    
    })
  
  # Probability of underestimation 
  output$prob <- renderUI({
    if (input$language == "R"){
      div(style = "font-family:courier", textInput("Rprob", "", 
                "curve(1. -0.5 * pbeta((1-(x*x)/4), ((10 - 1)/2),0.5), from = 0, to = 2)"))
    }
    else if (input$language == "Python"){
      div(style = "font-family:courier", textInput("Pprob", "", 
                "plt.plot(np.linspace(0, 2), 1.-0.5*betainc((10-1.)/2., 0.5, 1.-(np.linspace(0, 2)**2)/4))"))
    }
    
    else if (input$language == "MATLAB"){
      div(style = "font-family:courier", textInput("Mprob", "", 
                    "fplot(@(x) 1 - 0.5 .* betainc(1-((x.^2)/4), (10-1)/2, 0.5), [0, 2])"))
    }
    
  })
  
  ################# Copy buttons  ################### 
  output$clip1 <- renderUI({
    if (input$language == "R"){
      rclipButton("clipbtn", "Copy", input$Rmedian, icon("copy"), 
                width = "25%")
    }
    else if (input$language == "Python"){
      rclipButton("clipbtn", "Copy", input$Pmedian, icon("copy"), 
                  width = "25%")
    }
    
    else if (input$language == "MATLAB"){
      rclipButton("clipbtn", "Copy", input$Mmedian, icon("copy"), 
                  width = "25%")
    }
  })
  
  output$clip2 <- renderUI({
    if (input$language == "R"){
      rclipButton("clipbtn", "Copy", input$Rprob, icon("copy"), 
                  width = "25%")
    }
    else if (input$language == "Python"){
      rclipButton("clipbtn", "Copy", input$Pprob, icon("copy"), 
                  width = "25%")
    }
    
    else if (input$language == "MATLAB"){
      rclipButton("clipbtn", "Copy", input$Mprob, icon("copy"), 
                  width = "25%")
    }
    
  })
  
  
}
