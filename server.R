# The MIT License (MIT)
# 
# Copyright (c) 2014 Brandon Gavett
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

pkg <- c("shiny", "shinyIncubator")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

library(shiny)
library(shinyIncubator)

shinyServer(function(input, output, session) {
  
  post.vector <- reactive({
    if (input$calc == 0)
      return()
    isolate({
        a.sub.m <- c(rep(10,11), 10.1, 10, 10, 10)
        a.sub.s <- c(rep(3, 11), 3.1, 3.1, 3, 3)
        sg.sub.m <- c(7.7, 7.4, 8, 8, 7.5, 7.8, 8.3, 8.3, 8.1, 8.4, 8.5, 7.6, 7.7, 7.8, 7.8)
        sg.sub.s <- c(3.9, 4, 3.7, 3.9, 3.8, 3.5, 3.3, 3.3, 3.3, 3.4, 3.2, 3.8, 3.8, 3.4, 3.5)
        
        pct.a <- data.frame(read.csv("Pct_A.csv", header=T)[2:18])
        pct.sg <- data.frame(read.csv("Pct_SG.csv", header=T)[2:18])
              
        clin.br <- input$Prior
        
        post.var <- data.frame(Cutoff = rep(input$Cutoff, 16), 
                               kATS = 0:15, 
                               pNC = rep(clin.br, 16))
        post.var$pIN <- t(pct.a[pct.a$cutoff == input$Cutoff, 2:17])
        post.var$pIC <- t(pct.sg[pct.sg$cutoff == input$Cutoff, 2:17])
        post.var$pPost <- with(post.var, (pIN * pNC)/((pIN * pNC)+(pIC * (1-pNC))))
        return(post.var)
    })
  })
  
  
  output$Posterior <- renderText({
    if (input$calc == 0)
      return()
    isolate({
      paste0("Post-test Probability of Normal Cognition: ", 
             round(100*post.vector()$pPost[input$kATS+1], 2), "%")})
  })
  
  output$plot <- renderPlot({
    if (input$calc == 0)
      return()
    isolate({
      plot(0:15, post.vector()$pPost, type = "l", 
           xlab = "Number of Abnormal WMS-IV Adult Battery Test Scores", 
           ylab = "Post-test Probability of Normal Cognition",
           main = paste0("Base Rate of Normal Cognition = ", round(input$Prior*100,2), "%"))
      axis(1, at = 0:15, labels = 0:15)
      abline(v = input$kATS, col = "red", lty = 2)})
  })
})