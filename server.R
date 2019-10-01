server<-function(input, output, session){

  isMobile <- reactive({
    test = mobileDetect()
    return(test)
  })

  dataInput <- reactive({
    switch(input$dataset,
           "SL: x1/x2 Sublethal" = SL,
           "NB: Neoblasts" = NB
    )
  })

  # Set color template based on object
  color <- reactive({
    obj = dataInput()
    if(ncol(obj) > 7000){
      cols <- NB_colors
    }
    else{
      cols <- SL_colors
    }
  })

 tSNEPlotOutput  <- reactive({
	 obj = dataInput()
	 p1  <- DimPlot(obj, reduction = "tsne", label = TRUE, label.size = 6, pt.size = 0.5, cols = color())
	 p1
 })

 output$tSNEPlot  <- renderPlot({
    plot1 = tSNEPlotOutput()
    plot(plot1)}, height= function(){session$clientData$output_tSNEPlot_width})

output$downloadtSNEPlot  <- downloadHandler(
filename = function() { paste("tSNE_plot_", input$dataset, ".pdf", sep = "") },
content = function(file) {
	pdf(file, height = 8, width = 8)
	print(tSNEPlotOutput())
	dev.off()
}
)

  output$downloadtsnePlot <- downloadHandler(
    filename = function() { paste("tSNE_Plot_", input$dataset, ".pdf", sep="") },
    content = function(file) {
      pdf(file, height=8, width=8)
      print(tSNEPlotOutput())
      dev.off()
    }
  )

  vlnPlotOutput<-reactive({
    obj=dataInput()
    mygene<-input$gene
    p2 <- VlnPlot(obj, mygene, pt.size = 0.5, cols = color())
    return(p2)
  })

  output$violinPlot <- renderPlot({
    plot2 = vlnPlotOutput()
    print(plot2)}, height = function(){round(session$clientData$output_violinPlot_width)})

  output$downloadVlnPlot <- downloadHandler(
    filename = function() { paste("violin_plot_", input$gene, ".pdf",sep="") },
    content = function(file) {
      pdf(file, height=8, width=8)
      print(vlnPlotOutput())
      dev.off()
    }
  )

  featurePlotOutput<-function(){
    obj=dataInput()
    mygene<-input$gene
    p3<-FeaturePlot(obj, mygene, pt.size = 0.5, cols = mycolors)
    return(p3)
  }

  output$featurePlot <- renderPlot({
    plot3 = featurePlotOutput()
    print(plot3)},  height= function(){session$clientData$output_tSNEPlot_width})

  output$downloadFeaturePlot <- downloadHandler(
    filename = function() { paste("feature_plot_", input$gene, ".pdf",sep="") },
    content = function(file) {
      pdf(file, height=8, width=8)
      print(featurePlotOutput())
      dev.off()
    }
  )

# Mobile friendly versions

  output$tSNEPlot1  <- renderPlot({
    plot1 = tSNEPlotOutput()
    plot(plot1)})

  output$downloadtSNEPlot1  <- downloadHandler(
    filename = function() { paste("tSNE_plot_", input$dataset, ".pdf", sep = "") },
    content = function(file) {
      pdf(file, height = 8, width = 8)
      print(tSNEPlotOutput())
      dev.off()
    }
  )

  output$violinPlot1 <- renderPlot({
    plot2 = vlnPlotOutput()
    print(plot2)
  })

  output$downloadVlnPlot1 <- downloadHandler(
    filename = function() { paste("violin_plot_", input$gene, ".pdf",sep="") },
    content = function(file) {
      pdf(file, height=8, width=8)
      print(vlnPlotOutput())
      dev.off()
    }
  )

  output$featurePlot1 <- renderPlot({
    plot3 = featurePlotOutput()
    print(plot3)})

  output$downloadFeaturePlot1 <- downloadHandler(
    filename = function() { paste("feature_plot_", input$gene, ".pdf",sep="") },
    content = function(file) {
      pdf(file, height=8, width=8)
      print(featurePlotOutput())
      dev.off()
    })

}
