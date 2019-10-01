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
