library(shiny)
library(markdown) #includeMarkdown()
library(plyr) #llply()
library(shinyAce) #updateAceEditor()


######################################################
# Loading Figures and their annotations at start
######################################################

## Figures
fig_list <- read.delim(file.path("data", "rawDB.txt"),
                       colClasses = "character")

# get thumbnail files
figs <- file.path("figures", paste0(fig_list$basename, ".png"))

# get figure basenames from thumbnails and make HTML element for each figure
figs_src <- paste0('<img class="chart" src="',
                     figs, '" data-code="', fig_list$basename, '"></img>')

# vector of figures used to fill matrix
txt <- paste(figs_src)

# check if thumbnail file exists ,
# if its not there, replace with name of file
ind <- basename(figs) %in% list.files(file.path("www", "figures"))

txt[!ind] <- basename(figs)[!ind]

## Tags

tag_list<-read.csv(file="data/tags.csv",header=T,colClasses="character",stringsAsFactors = FALSE)

#collapse all the tags for one document
for(i in 1:nrow(fig_list)){
  pmid<-fig_list$basename[i]
  tagString<-"Show All"
  
  tmp<-filter(tag_list,basename==pmid) 
   
   if(nrow(tmp)>0){

    tagString<-tmp %>%
      select(contains("WhatLvl"),contains("How1")) %>%
      unlist()%>%
      unique() %>%
      na.omit() %>%
      paste0(.,collapse=",")
    
    tagString<-paste0(c("Show All",unlist(tagString)),collapse = ",")
   }
  
  fig_list[i,]$tags<-tagString   
}

######################################################
# Added Functions
######################################################

filter.tags <- function(x,showAll=TRUE) {
  # Get filtered figures based on list of tags selected.
  #
  # Args:
  #   x: list of tags selected
  #
  # Returns:
  #   A matrix containing filtered figures.
  
  if(!showAll){
    x<-setdiff(x,"Show All")
  }
  
  list_of_ind <- llply(x, function(x) which(grepl(x, fig_list$tags, ignore.case = TRUE)))
  id_ind <- Reduce(intersect, list_of_ind) # intersection
  ind_set <- unique(unlist(list_of_ind)) # get set
  ind_set
}

delete.NULLs  <-  function(x_list) {
  # Remove empty elements from list
  #
  # Args:
  #   x.list: list
  #
  # Returns:
  #   List with no empty elements
  x_list[unlist(lapply(x_list, length) != 0)]
}

pad.Vector <- function(x) {
  # Pad vector so its length is multiple of 3
  # (the # of cols of display matrix)
  #
  # Args:
  #   x: vector
  #
  # Returns:
  #   Vector of length multiple of 3

  # determine length of padding needed to fill matrix of ncol = 3
  n <- 3 * ceiling(length(x) / 3) - length(x)

  x_padded <- c(x, rep(NA, n))
  x_padded

}


shinyServer(function(input, output, session){

  datasetInput <- reactive({
    if((length(input$selectWhatLevelOne) + length(input$selectWhatLevelTwo)) == 0) {

      txt_padded <- pad.Vector(txt)
      mat <- matrix(txt_padded , ncol = 3, byrow = TRUE)

    } else {
      index_whatOne <- as.list(input$selectWhatLevelOne) # indices of selected type
      index_whatTwo <- as.list(input$selectWhatLevelTwo) # indices of selected type
      index_how <- as.list(input$selectHow)
      
      #just checking how often it says to "show all
      tmp<-c(index_whatOne,index_whatTwo,index_how)
      temp2<-sapply(tmp,function(val){
        showAllNum<-llply(val, function(x) which(grepl("Show All", x, ignore.case = TRUE)))
      })
      
      showAllNum<-sum(unlist(temp2))
      showAllNum<-ifelse(showAllNum== 2, TRUE,FALSE)
      
      
      list_of_indices <- list(filter.tags(index_whatOne,showAllNum),
                              filter.tags(index_whatTwo,showAllNum),
                              filter.tags(index_how,showAllNum))
      
      # remove empty elements from list of indices
      list_of_indices_clean <- delete.NULLs(list_of_indices)

      index_final <- Reduce(intersect, list_of_indices_clean)

      index_padded <- pad.Vector(index_final)

      if(length(index_padded)/3 > 0) {
        matrix(txt[index_padded], ncol = 3, nrow = (length(index_padded)/3), byrow = TRUE)
      } else {
        empty_figs <- "There are no graphs that match the selected criteria."
        matrix(empty_figs, nrow = 1, ncol = 1) # return empty matrix to avoid console warning
      }
    }
  })

  output$mytable <- renderTable({

    dataset <- datasetInput()
    dataset <- data.frame(dataset) # can just use matrix
    dataset

  }, sanitize.text.function = function(x) x,
  include.colnames = FALSE, include.rownames = FALSE)
  
  # Create a reactiveValues object, to let us use settable reactive values
  values <- reactiveValues()
  values$clicked <- FALSE
  #values$code <- NULL

  # When the app loads, if there is a hash value then load that figure
  observe({
    urlHash <- session$clientData$url_hash_initial
    if (!is.null(urlHash) && substring(urlHash, 1, 1) == "#") {
      values$code <- substring(urlHash, 2)
    }
  })

  # When a figure is clicked, load that figure
  observe({
    if (!is.null(input$clicked) && input$clicked == TRUE) {
      values$code <- input$code

    # Add figure name to URL so it can be retrieved later
    session$sendCustomMessage("figClick", values$code)
    updateTabsetPanel(session, "tabset", selected = "Figure")  
    }
  })

  # Load a figure and open the Figure tab
  observeEvent(values$code, {
    updateTabsetPanel(session, "tabset", selected = "Figure & Code")
  })
  

   output$whatLevelOne<-renderUI({
     choices<-c(unique(tag_list$WhatLvl1),"Show All")
     selectizeInput(inputId="selectWhatLevelOne",label = "What (Level 1)",choices=choices,selected="Show All",multiple=TRUE)
     
     #choices<-c(unique(tag_list$WhatLvl1),"Show All")
     #if(is.null(input$selectHow) ||input$selectHow == "Show All"){
     #  selectizeInput(inputId="selectWhatLevelOne",label = "What (Level 1)",choices=choices,selected="Show All",multiple=TRUE)
     #}else{
     #  optsChoices<-tag_list %>% filter(How1 %in% input$selectHow) %>% select(WhatLvl1)
      # optsChoices<-optsChoices$WhatLvl1[!is.na(optsChoices$WhatLvl1)]
      # selectizeInput(inputId="selectWhatLevelOne",label = "What (Level 1)",choices=choices,selected=optsChoices,multiple=TRUE)
     #}
     
   })
  
   
   output$whatLevelTwo<-renderUI({
     if(is.null(input$selectWhatLevelOne) ||input$selectWhatLevelOne == "Show All"){
       choices<-unique(tag_list$WhatLvl2)
       choices<-c(choices[!is.na(choices)],"Show All")
       selectizeInput(inputId="selectWhatLevelTwo",label = "What (Level 2)",choices=choices,selected="Show All",multiple=TRUE)
     }else{
       choices<-tag_list %>% filter(WhatLvl1 %in% input$selectWhatLevelOne) %>% select(WhatLvl2)
       choices<-choices$WhatLvl2[!is.na(choices$WhatLvl2)]
       selectizeInput(inputId="selectWhatLevelTwo",label = "What (Level 2)",choices=choices,selected=choices,multiple=TRUE)
     }
     
   })
   
   
   output$How<-renderUI({
    choices<-unique(tag_list$How1)
    choices<-c(choices[!is.na(choices)],"Show All")
    selectizeInput(inputId="selectHow",label = "How Visualized",choices=choices,selected="Show All",multiple=TRUE)

   })
   
   
  
  output$figImage <- renderImage({

    if(length(values$code) == 0) {
      filename <- normalizePath(file.path('www',
                                          paste("please-select-figure.png")))
    } else {
      filename <- normalizePath(file.path('www/figures',
                                          paste0(values$code, ".png")))
    }
    list(src = filename,
         width = 600,
         height = 600)
  }, deleteFile = FALSE)
  

  observeEvent(input$annotateGo,{
    updateTabsetPanel(session, "tabset", selected = "Annotate")
  })
  
  
  output$figImage_only <- renderImage({

    if(length(values$code) == 0) {
      filename <- normalizePath(file.path('www',
                                          paste("please-select-figure.png")))
    } else {
      filename <- normalizePath(file.path('www/figures',
                                          paste0(values$code, ".png")))
    }
    list(src = filename,
         width = 800,
         height = "auto")
  }, deleteFile = FALSE)
  
  
  
  output$figPaper_info<-renderText({
    if(length(values$code) == 0) {
      x<- ""
      return(x)
    } else {
      print(values$code)
      #x<-paste("Source:", toString(fig_list[fig_list$basename == values$code,]$PMID))
      record<-fig_list[fig_list$basename == values$code,]
      
      link <- paste(a(paste("Source:", toString(record$PMID)),
                      href = paste(record$url), target = "_blank"))
      
      return(link)
    }
  })
  
  output$codeTable<-renderDataTable({
    if(length(values$code) == 0){
      return(NA)
    }else{
      pmid<-filter(fig_list,basename == values$code) %>% select(basename)
      
      tmp<-filter(tag_list,basename == pmid$basename) %>%
        select(contains("Lvl"),contains("How"))

      return(tmp)
    }
  },options = list(dom = 't'))

 
})

