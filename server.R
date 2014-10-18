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

library(shiny)
library(shinyIncubator)

# Define server logic required to calculate various reliable change indices
shinyServer(function(input, output, session) {
  
  post.vector <- reactive({
    if (input$calc == 0)
      return()
    isolate({
      withProgress(session, min = 1, max = 100, {
        setProgress(message = "Loading...", detail = "Completion time depends on the number of Monte Carlo simulations.")
        for(i in 1:100) {
          setProgress(value = i)
          Sys.sleep(.25)
        }
        a.sub.m <- c(rep(10,11), 10.1, 10, 10, 10)
        a.sub.s <- c(rep(3, 11), 3.1, 3.1, 3, 3)
        sg.sub.m <- c(7.7, 7.4, 8, 8, 7.5, 7.8, 8.3, 8.3, 8.1, 8.4, 8.5, 7.6, 7.7, 7.8, 7.8)
        sg.sub.s <- c(3.9, 4, 3.7, 3.9, 3.8, 3.5, 3.3, 3.3, 3.3, 3.4, 3.2, 3.8, 3.8, 3.4, 3.5)
        
        cm.a.sub <- read.csv("WMSIV_A_Sub.csv", header=F)
        cm.sg.sub <- read.csv("WMSIV_SG_Sub.csv", header=F)
        
        chol.a.sub <- chol(cm.a.sub)
        chol.sg.sub <- chol(cm.sg.sub)
        
        nsims <- input$NSims
        
        set.seed(80918)
        a.sub.random <- matrix(rnorm(nsims*15, mean = a.sub.m, sd = a.sub.s), ncol = nsims, nrow = 15)
        a.sub.random.z <- (a.sub.random-10)/3
        set.seed(80920)
        sg.sub.random <- matrix(rnorm(nsims*15, mean = sg.sub.m, sd = sg.sub.s), ncol = nsims, nrow = 15)
        sg.sub.random.z <- (sg.sub.random-10)/3
        
        sim.data.a <- t(t(chol.a.sub) %*% a.sub.random.z)
        sim.data.a <- (sim.data.a*3)+10
        a.sub.data <- data.frame(sim.data.a)
        
        sim.data.sg <- t(t(chol.sg.sub) %*% sg.sub.random.z)
        sim.data.sg <- (sim.data.sg*3)+10
        sg.sub.data <- data.frame(sim.data.sg)
        
        a.sub.data$Imp1 <- rowSums(ifelse(a.sub.data[1:15] < input$Cutoff, 1, 0))    
        sg.sub.data$Imp1 <- rowSums(ifelse(sg.sub.data[1:15] < input$Cutoff, 1, 0))
        
        a.cumpct.Imp1 <- c(sum(table(a.sub.data$Imp1)[1:16])/nsims, 
                           sum(table(a.sub.data$Imp1)[2:16])/nsims, 
                           sum(table(a.sub.data$Imp1)[3:16])/nsims,
                           sum(table(a.sub.data$Imp1)[4:16])/nsims,
                           sum(table(a.sub.data$Imp1)[5:16])/nsims,
                           sum(table(a.sub.data$Imp1)[6:16])/nsims,
                           sum(table(a.sub.data$Imp1)[7:16])/nsims,
                           sum(table(a.sub.data$Imp1)[8:16])/nsims,
                           sum(table(a.sub.data$Imp1)[9:16])/nsims,
                           sum(table(a.sub.data$Imp1)[10:16])/nsims,
                           sum(table(a.sub.data$Imp1)[11:16])/nsims,
                           sum(table(a.sub.data$Imp1)[12:16])/nsims,
                           sum(table(a.sub.data$Imp1)[13:16])/nsims,
                           sum(table(a.sub.data$Imp1)[14:16])/nsims,
                           sum(table(a.sub.data$Imp1)[15:16])/nsims,
                           sum(table(a.sub.data$Imp1)[16:16])/nsims)
        
        sg.cumpct.Imp1 <- c(sum(table(sg.sub.data$Imp1)[1:16])/nsims, 
                            sum(table(sg.sub.data$Imp1)[2:16])/nsims, 
                            sum(table(sg.sub.data$Imp1)[3:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[4:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[5:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[6:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[7:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[8:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[9:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[10:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[11:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[12:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[13:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[14:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[15:16])/nsims,
                            sum(table(sg.sub.data$Imp1)[16:16])/nsims)
        
        clin.br <- input$Prior
        
        post.var <- data.frame(Cutoff = rep(input$Cutoff, 16), 
                               kATS = 0:15, 
                               pNC = rep(clin.br, 16), 
                               pIN = a.cumpct.Imp1, 
                               pIC = sg.cumpct.Imp1)
        post.var$IpNC <- 1-post.var$pNC
        post.var$pPost <- with(post.var, (pIN * pNC)/((pIN * pNC)+(pIC * (1-pNC))))
        return(post.var)
      })
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