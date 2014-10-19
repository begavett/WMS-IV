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
options(scipen = 10)

shinyUI(fluidPage(
  # Application Title
  title= "Bayesian Estimation of Post-test Probabilities for Normal Cognition using the Wechsler Memory Scales - 4th Edition (WMS-IV)",
  fluidRow(h2("Bayesian Estimation of Post-test Probabilities for Normal Cognition using the Wechsler Memory Scales - 4th Edition (WMS-IV)")),
  fluidRow(HTML("Based on Gavett, B. E. (2014). A Bayesian Approach to Interpreting Base Rates of Abnormal Test Scores in Cognitively Healthy and Clinical Samples. <i>Manuscript Under Review</i>.")),
  tags$hr(),
  fluidRow(column(5,
                  numericInput("Cutoff", "Scaled Score Cutoff (<):",7,min=2,max=19),
                  numericInput("kATS", "Number of Abnormal Test Scores:", 0, min = 0, max = 15),
                  numericInput("Prior", "Pre-test Probability of Normal Cognition:", .25, min = 0, max = 1, step = .01),
                  br(),
                  actionButton("calc", "Calculate")),
           column(7, 
                  h3(textOutput("Posterior")),
                  plotOutput("plot"))
           ),
  fluidRow(div(img(src = "http://www.uccs.edu/Images/brand/uccs-logo.png", width=400, height=58, align = "center"),style = "text-align: center;"))
  )
)