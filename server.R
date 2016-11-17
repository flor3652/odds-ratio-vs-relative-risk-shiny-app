library(shiny)
library(plotly)


shinyServer(function(input, output){
  
  #Graph
  rrp <- reactive({
    if(input$orInpFormat=="slide"){
      p1 <- (10:100) / 100
      p2 <- numeric(length=length(p1))
      rel.risk <- numeric(length=length(p1))
      for(i in 1:length(p1)){
        p2[i] <- p1[i] / (input$slideOddsRatio - p1[i]*input$slideOddsRatio + p1[i])
        rel.risk[i] <- p1[i]/p2[i]
      }
    } else if (input$orInpFormat=="num"){
      p1 <- (10:100) / 100
      p2 <- numeric(length=length(p1))
      rel.risk <- numeric(length=length(p1))
      for(i in 1:length(p1)){
        p2[i] <- p1[i] / (input$numOddsRatio - p1[i]*input$numOddsRatio + p1[i])
        rel.risk[i] <- p1[i]/p2[i]
      }
    }
    if(input$p2onx){
      data.frame(p2,rel.risk)
    } else{
      data.frame(p1,rel.risk)
    }
  })
  
  output$rrPlot <- renderPlot({
    if(input$setylim){
      plot(rrp(), main="Relative Risk vs. Probability", ylab="Relative Risk", xlab="Probability", ylim=c(input$lowery, input$uppery), type="l")
    } else {
      plot(rrp(), main="Relative Risk vs. Probability", ylab="Relative Risk", xlab="Probability", type="l")
    }
  })
  
  
  
  #Table
  rrt <- reactive({
    p1 <- (10:100) / 100
    p2 <- numeric(length=length(p1))
    rel.risk <- numeric(length=length(p1))
    if(input$orInpFormat=="slide"){
      or = as.numeric(input$slideOddsRatio)
    } else if(input$orInpFormat=="num"){
      or = as.numeric(input$numOddsRatio)
    }
    for(i in 1:length(p1)){
      p2[i] <- p1[i] / (or - p1[i]*or + p1[i])
      rel.risk[i] <- p1[i]/p2[i]
    }
    or <- (p1/(1-p1))/(p2/(1-p2))
    dat <- data.frame(p1,p2,rel.risk,or)
    dat <- dat[dat$p1 %in% ((1:10)/10),]
    names(dat) <- c("P1", "P2", "Relative Risk", "Odds Ratio")
    dat
  })
  
  output$rrTable <- renderDataTable({
    rrt()
  })

  
  
  #3d Plot
  p1 <- (1:100)/100
  or <- (10:100)/10
  inp <- expand.grid(p1,or)
  names(inp) <- c("p1", "or")
  p2 <- numeric(length=nrow(inp))
  rel.risk <- numeric(length=nrow(inp))
  for(i in 1:nrow(inp)){
    p2[i] <- inp$p1[i] / (inp$or[i] - inp$p1[i]*inp$or[i] + inp$p1[i])
    rel.risk[i] <- inp$p1[i]/p2[i]
  }
  out <- data.frame(inp, p2, rel.risk)
  out$calcor <- (out$p1/(1-out$p1))/(out$p2/(1-out$p2))
  
  output$threeDPlot <- renderPlotly({
    if(input$p2onx){
      plot_ly(x=out$p2, y=out$or, z=out$rel.risk, type="scatter3d", mode="markers", width = 750, height=750) %>% layout(scene=list(  xaxis = list(title="P(Denominator=1)"),  yaxis = list(title="Odds Ratio"), zaxis = list(title="Relative Risk")))
    } else {
      plot_ly(x=out$p1, y=out$or, z=out$rel.risk, type="scatter3d", mode="markers", width = 750, height=750) %>% layout(scene=list(  xaxis = list(title="P(Numerator=1)"),  yaxis = list(title="Odds Ratio"), zaxis = list(title="Relative Risk")))
    }
  })
  
})









