#' Netreader
#'
#' \code{Netreder} is a RStudio-Addin that allows you to read network files.
#'
#' @details To run the addin, select \code{Netreader} from the Addins-menu within RStudio.
#' @return \code{Netreader} returns the created network as igraph object.
#' @import miniUI
#' @import shiny
#' @import ggplot2
#' @import ggraph
#' @import rstudioapi
#' @import igraph
#' @name Netbuilder
NULL

Netreader <- function(){
  rv <- reactiveValues(g = NULL)
  #ui ----
  ui <- miniPage(
    tags$script(jscodeWidth),
    tags$script(jscodeHeight),
    tags$style(type = "text/css", ".selectize-dropdown{ width: 200px !important; }"),
    tags$style(type = "text/css",".form-group.shiny-input-container{width:50%;}"),
    tags$style(type='text/css', '#preview {background-color: rgba(255,255,0,0.40); color: green;}'),

    gadgetTitleBar("Netreader"),
    miniTabstripPanel(selected = 'Read Network',
      miniTabPanel("Read Network",
        fillRow(height = line.height, width = '100%',
          fileInput("netfile", "Choose network file")
        ),
        fillRow(height = line.height, width = '100%',
          h4("File Preview (first 5 lines)")
        ),
        fillRow(height = "120px", width = '100%',
          verbatimTextOutput("preview")
        ),
        fillRow(height = line.height, width='50%',
                checkboxInput("colnames","Header",value=FALSE),
                checkboxInput("rownames","Rownames",value=FALSE),
                checkboxInput("quotes","Quotes",value=FALSE),
                checkboxInput("directed","Directed",value=FALSE)
        ),
        fillRow(height = line.height, width = '50%',
          radioButtons("readfct","network format", choices = c("edgelist","adjacency matrix")),
          radioButtons("valsep","file delimiter", choices=c("comma"=",","space"=" ","tab"="\t"))
        ),
        tags$hr(),
        fillRow(height = line.height, width = '100%',
              actionButton("readit","Read Network"),
              textAreaInput("text",label = NA,value = "",placeholder = "enter name",height="35px")
        ),
        fillRow(height = "120px", width = '100%',
                verbatimTextOutput("netpreview")
        )
      ),
      miniTabPanel("Add Attributes",
       fillRow(height = line.height, width = '100%',
               h4("File Preview (first 5 lines)")
       )
      )
    )
  )
  #server ----
  server <- function(input, output, session) {
    # file preview ----
    output$preview <- renderText({
      inFile <- input$netfile
      if (is.null(inFile)) return(NULL)
      txt <- readLines(inFile$datapath,n=5)
      txt <- paste(txt,collapse="\n")
      txt
    })

    #network preview ----
    output$netpreview <- renderPrint({
      g <- rv$g
      if (is.null(g)) return("no network created yet.")
      g
    })
    # read function ----
    observeEvent(input$readit,{
      inFile <- input$netfile
      q <- ifelse(input$quotes,"\"","")
      if(input$rownames){
      A <- tryCatch(read.table(inFile$datapath,
                               header = input$colnames,
                               row.names = 1,
                               sep = input$valsep,quote = q),
                    error=function(e) NULL)
      } else{
        A <- tryCatch(read.table(inFile$datapath,
                                 header = input$colnames,
                                 sep = input$valsep,quote = q),
                      error=function(e) NULL)
      }
      if(is.null(A)){
        showNotification("something went wrong reading the file. Check your settings",type = "error")
      } else{
        if(input$readfct=="adjacency matrix"){
          mode <- ifelse(input$directed,"directed","undirected")
          g <- tryCatch(igraph::graph_from_adjacency_matrix(as.matrix(A),mode = mode),error=function(e) NULL)
          if(is.null(g)){
            showNotification("something went wrong creating the network.",type = "error")
          } else{
            rv$g <- g
          }
        }
        else if(input$readfct=="edgelist"){
          mode <- ifelse(input$directed,T,F)
          g <- tryCatch(igraph::graph_from_data_frame(A,directed = mode),error=function(e) NULL)
          if(is.null(g)){
            showNotification("something went wrong creating the network.",type = "error")
          } else{
            rv$g <- g
          }
        }
      }

    })

    # cancel ----
    observeEvent(input$cancel, {
      invisible(stopApp())
    })

    #done ----
    observeEvent(input$done, {
      if(input$text==""){
        showNotification("Please enter a variable name",type="warning")
      }else{
        eval(parse(text = paste0("assign(\"",input$text,"\",rv$g",",envir = .GlobalEnv)")))
        invisible(stopApp())
      }
    })
  }


  viewer <- dialogViewer(dialogName = 'Netreader', width = 990, height = 900)
  runGadget(ui, server, stopOnCancel = FALSE, viewer = viewer)
}


#' @export
#' @rdname Netreader

NetreaderAddin <- function() {
  Netreader()
}
