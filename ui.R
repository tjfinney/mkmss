# ui.R
# Tim Finney, July 2015

library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("mkmss"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      
      submitButton("Run"),
      br(),
      
      sliderInput("n.seed", "Seed (for random numbers)", 
                  min = 0, max = 10, value = 0, step = 1
      ),
      sliderInput("n.chh", "Characters / text", 
                  min = 10, max = 150, value = 30, step = 10
      ),
      sliderInput("n.gg", "Generations / simulation", 
                  min = 1, max = 10, value = 5, step = 1
      ),
      sliderInput("p.import", "P(import) / unit (of demand)", 
                  min = 0, max = 1, value = 0, step = 0.1
      ),
      sliderInput("p.change", "P(change) / character", 
                    min = 0, max = 1, value = 0.15, step = 0.05
      ),
      sliderInput("p.corr", "P(correction) / generation", 
                  min = 0, max = 1, value = 0, step = 0.1
      ),
      sliderInput("p.edit", "P(edition) / generation", 
                  min = 0, max = 1, value = 0, step = 0.1
      ),
      sliderInput("n.trend", "Trend (toward preferred text)", 
                  min = 0, max = 10, value = 0, step = 0.5
      ),
      sliderInput("p.loss", "P(loss) / generation", 
                  min = 0, max = 1, value = 0.5, step = 0.1
      ),
      sliderInput("n.grow", "Growth / generation (logistic)", 
                  min = 0, max = 2, value = 1, step = 0.5
      ),
      sliderInput("n.ratio", "Lost / extant (for recovery)", 
                  min = 0, max = 4, value = 1, step = 0.5
      )
      
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Data matrix", tableOutput("data.mx")),
                  tabPanel("Dist. matrix", tableOutput("dist.mx")),
                  tabPanel("CMDS", plotOutput("cmds.plot")),
                  tabPanel("DC", plotOutput("dc.plot")),
                  tabPanel("NJ", plotOutput("nj.plot"))
      )
    )
  )
))
