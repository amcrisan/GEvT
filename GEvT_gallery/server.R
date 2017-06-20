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
  n <- 2 * ceiling(length(x) / 2) - length(x)

  x_padded <- c(x, rep(NA, n))
  x_padded

}


shinyServer(function(input, output, session){

  # Create a reactiveValues object, to let us use settable reactive values
  values <- reactiveValues()
  values$clicked <- FALSE
  #values$code <- NULL
  
  datasetInput <- reactive({
    if((length(str_extract(input$selectWhatLevelOne,"[A-Z,a-z]+")) + length(input$selectWhatLevelTwo)) == 0) {

      txt_padded <- pad.Vector(txt)
      mat <- matrix(txt_padded , ncol = 2, byrow = TRUE)

    } else {
      index_whatOne <- as.list(str_extract(input$selectWhatLevelOne,"[A-Z,a-z]+")) # indices of selected type
      index_whatTwo <- as.list(input$selectWhatLevelTwo) # indices of selected type
      index_how <- as.list(str_extract(input$selectHow,"[A-Z,a-z]+"))
      
      list_of_indices <- list(filter.tags(index_whatOne,showAllNum),
                              filter.tags(index_whatTwo,showAllNum),
                              filter.tags(index_how,showAllNum))

      # remove empty elements from list of indices
      list_of_indices_clean <- delete.NULLs(list_of_indices)

      index_final <- Reduce(intersect, list_of_indices_clean)
    
      index_padded <- pad.Vector(index_final)

      if(length(index_padded)/2 > 0) {
        matrix(txt[index_padded], ncol = 2, nrow = (length(index_padded)/2), byrow = TRUE)
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
     #gonna set up the tag list based on absolute, not relative, numbers
     choices<-colSums(with(tag_list,table(basename,WhatLvl1))!=0)
     choices<-sprintf("%s (%d)", names(choices),choices)
     #choices<-sort(unique(tag_list$WhatLvl1))
     
     checkboxGroupInput(inputId="selectWhatLevelOne",label = "What - Level 1",choices=choices,selected=choices)
     
   })
  
   output$How<-renderUI({
     tmp<-colSums(with(tag_list,table(basename,How1))!=0)
     choices<-data.frame(type=names(tmp),
                            total=tmp)
     
     #for those stupid instances where there is comma
     tmp1<-choices %>%
       mutate(type = strsplit(as.character(type), ",")) %>% 
       tidyr::unnest(type) %>%
       group_by(type)%>%
       tally(total)

     choices<-sprintf("%s (%d)", tmp1$type,tmp1$n)
     
     
    #choices<-strsplit(paste0(unique(tag_list$How1),collapse=","),",") %>% unlist() %>% unique()
    #choices<-choices[!is.na(choices)]
    checkboxGroupInput(inputId="selectHow",label = "How - Level 1",choices=choices,selected=choices)
   })
   
   
   #update What level 1 and how level 1 based upon  How
   observe({
     
     #choices<-unique(tag_list$WhatLvl2)
     choices<-colSums(with(tag_list,table(basename,WhatLvl2))!=0)
     
     selected<-tag_list %>% filter(WhatLvl1 %in% str_extract(input$selectWhatLevelOne,"[A-Z,a-z]+")) %>% 
       filter(How1 %in% str_extract(input$selectHow,"[A-Z,a-z]+"))%>%
       select(WhatLvl2)
     
     #selected<-selected$WhatLvl2[!is.na(selected$WhatLvl2)]
     selected<-choices[selected$WhatLvl2[!is.na(selected$WhatLvl2)]]
     
     choices<-names(choices)
     selected<-names(selected)
     #choices<-sprintf("%s (%d)", names(choices),choices)
     #selected<-sprintf("%s (%d)", names(selected),selected)
     
     updateSelectizeInput(session,"selectWhatLevelTwo",choices=choices,selected=selected)
     
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
  
  output$figPaper_annotation<-renderText({
    if(length(values$code) == 0) {
      x<- ""
      return(x)
    } else {
      print(values$code)
      #x<-paste("Source:", toString(fig_list[fig_list$basename == values$code,]$PMID))
      record<-fig_list[fig_list$basename == values$code,]
      
      url=paste0(paste0("http://labelme2.csail.mit.edu/Release3.0/tool.html?collection=LabelMe&mode=f&folder=users/amcrisan///gevit&image=",record$basename),".jpg")
      link <- paste(a("Show Annotations", href = url, target = "_blank"))
      
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

