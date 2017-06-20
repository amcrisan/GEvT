library(shiny)
library(shinyAce)
library(dplyr)

## Function from Joe Cheng
## https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content,
                      placement = c('right', 'top', 'left', 'bottom'),
                      trigger = c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover()})"),
        tags$style(type = "text/css", ".popover{max-width:500px; position: fixed;}")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-link",
      `data-toggle` = "popover", `data-html` = "true",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1],
      "More..."
    )
  )
}

shinyUI(fluidPage(

  tags$head(includeScript(file.path("www", "js", "app.js"))#,
            #includeScript(file.path("www", "js", "google-analytics.js"))
            ),

  includeCSS(file.path("www", "css", "app.css")),

  titlePanel("GEviT Prototype"),
  sidebarLayout(
    sidebarPanel(
      id = "sidepanel",
      width = 3,
      h3("About"),
      includeMarkdown("about.md"),
      tags$div(id = "popup",
               helpPopup(strong("Additional Information"),
                         includeMarkdown("about-extended.md"),
                         placement = "right", trigger = "click")),
      br(),
      h3("What"),
      uiOutput("whatLevelOne"),
      selectizeInput(inputId="selectWhatLevelTwo",label = "What - Level 2",choices="Show All",selected="Show All",multiple=TRUE),
      br(),
      h3("How"),
      uiOutput("How")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabset",
        tabPanel("Catalog", tableOutput("mytable")),
        # tabPanel("Figure & Code",
        #          fluidRow(
        #            column(width = 5, imageOutput("figImage", height = "auto")),
        #            column(width = 7,
        #                   aceEditor("fig_and_code",
        #                             value = "Please select a figure" ,
        #                             readOnly = TRUE, height = "450px"),
        #                   htmlOutput("link")))),
        tabPanel("Figure", 
                 #br(),
                 #fluidRow(
                 #  actionButton("showAnnotations", "Show Annotations")#,
                   #actionButton("annotateGo", "Edit or Add Annotations Tags")
                 #),
                 br(),
                 htmlOutput("figPaper_info"),
                 #htmlOutput("figPaper_annotation"),
                 br(),
                 imageOutput("figImage_only", height = "100%"),
                 br(),
                 h4("GEviT Terms"),
                 dataTableOutput("codeTable"))#,
        #tabPanel("Annotate",htmlOutput("annotate_interface"))#,
        #tabPanel("Paper Info",htmlOutput("figPaper_info"))
        #tabPanel("Code", htmlOutput("code_only"))
      )


    )
  )
))

