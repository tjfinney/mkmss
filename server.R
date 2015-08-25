# server.R
# Tim Finney, July 2015

library("shiny")
require("graphics")
require("ape")
source("helpers.R")

# define server logic
shinyServer(function(input, output) {
  
  mss <- function() { mkmss(reactiveValuesToList(input)) }

  output$summary <- renderPrint({ cat(mss()$summary, fill=TRUE) })

  # data matrix
  output$data.mx <- renderTable({ mss()$data.mx })
  
  # dist matrix
  output$dist.mx <- renderTable({ mss()$dist.mx }, digits=3)

  # CMDS plot
  output$cmds.plot <- renderPlot({
    dist.mx <- mss()$dist.mx
    MDS <- cmdscale(dist.mx, k = 2, eig = TRUE)
    x <- MDS$points[,1]
    y <- MDS$points[,2]
    par(bg="white")
    plot(x, y, type="n", asp = 1)
    text(x, y, rownames(dist.mx))
 })
  
  # DC plot
  output$dc.plot <- renderPlot({
    dist <- as.dist(mss()$dist.mx)
    par(bg="white")
    plot(diana(dist), which=2, main="", cex=0.8)
  })
  
  # NJ plot
  output$nj.plot <- renderPlot({
    dist.mx <- mss()$dist.mx
    par(bg="white")
    par(mar=c(.1,.1,.1,.1))
    plot(nj(dist.mx), "u")
  })

})
