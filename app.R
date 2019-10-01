library(shiny)
library(pheatmap)
library(ggplot2)
library(RColorBrewer)
library(Seurat)

options(shiny.maxRequestSize=50*1024^2)

SL <- readRDS("data/xins_v3.Rds") # Shiny doesn't want the full path, it just wants the relative path apparently
NB <- readRDS("data/NB_Diet.rds")

# FeaturePlot_Color <- colorRampPalette(brewer.pal(n = 9, name = "RdYlBu")[2:9])(30)
SL_colors <-c("yellow3","darkcyan","wheat4","plum","navy","magenta","tomato","burlywood","lawngreen","slategray3")
NB_colors <-c("lightskyblue2","steelblue","palegreen2","seagreen","lightpink2","red3","peachpuff","orange2","thistle","purple","gray44","tan4")
mycolors=(rev(brewer.pal(n = 11, name ="RdYlBu"))[2:11])

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

ui<-fluidPage(
  titlePanel("PGES: Planarian Gene Expression in Single-Cell"),
  ## Create a new Row in the UI for selectInputs
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(inputId="dataset", label="Dataset",
                             choices = c("NB: Neoblasts", "SL: x1/x2 Sublethal"), selected = "NB: Neoblasts"),
                 textInput("gene", label="Please enter SMED ID:", value="SMED30012332"),
                 HTML(paste0("<br/>")),
                 tags$a(href = "https://planosphere.stowers.org/find/genes", "Please click here for a link to our list of Planarian genes"),
                 HTML(paste0("<br/>")),
                 HTML(paste0("<br/>")),
                 tags$a(href = "https://www.cell.com/cell/fulltext/S0092-8674(18)30583-X#secsectitle0270", HTML("Zeng A, Li H, Guo L, Gao X, McKinney S, Wang Y, Yu Z, <br/>Park J, Semerad C, Ross E, Cheng LC, Davies E, Lei K, <br/> Wang W, Perera A, Hall K, Peak A, Box A, <br/> Sánchez Alvarado A. <br/> Prospectively Isolated Tetraspanin(+) Neoblasts Are Adult <br/> Pluripotent Stem Cells Underlying Planaria Regeneration. <br/>Cell.2018 173:1593-1608.e1520.")),
                 HTML(paste0("<br/>")),
                 HTML(paste0("<br/>")),
                 div(img(src = "image.png", width = 200, align = "center"))
    ),

    mainPanel(
      conditionalPanel(condition = "output.isMobile", # Condition should check the javascript function to see if user is on mobile

      plotOutput("tSNEPlot", width = "auto", height = "auto"),
      br(),
      column(5, downloadButton(outputId = "downloadtSNEPlot", label = "Download tSNE Plot (pdf)")),

      plotOutput("violinPlot", width = "auto", height = "auto"),
      br(),
      column(5, downloadButton(outputId="downloadVlnPlot",label="Download Violin Plot (pdf)")),

      br(),
      plotOutput("featurePlot", width = "auto", height = "auto"),

      br(),
      column(8, downloadButton(outputId="downloadFeaturePlot", label="Download Feature Plot (pdf)"))),

      conditionalPanel(condition = "!output.isMobile", # Need different plotOutput function names

      plotOutput("tSNEPlot1", width = "600px", height = "500px"),
      br(),
      column(5, downloadButton(outputId = "downloadtSNEPlot1", label = "Download tSNE Plot (pdf)")),

      plotOutput("violinPlot1", width = "600px", height = "500px"),
      br(),
      column(5, downloadButton(outputId="downloadVlnPlot1",label="Download Violin Plot (pdf)")),

      br(),
      plotOutput("featurePlot1", width = "600px", height = "500px"),

      br(),
      column(8, downloadButton(outputId="downloadFeaturePlot1", label="Download Feature Plot (pdf)"))
        )
    )
  ))

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
shinyApp(ui=ui, server=server)
