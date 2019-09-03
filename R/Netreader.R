#' Netreader
#'
#' \code{Netreader} is an RStudio-Addin that allows you to read network files. So far, only plaintext files are supported (e.g. csv,tsv,...).
#'
#' @details To run the addin, select \code{Netreader} from the Addins-menu within RStudio.
#' @return \code{Netreader} returns the created network as igraph object.
#' @import miniUI
#' @import shiny
#' @import rstudioapi
#' @importFrom igraph graph_from_adjacency_matrix graph_from_data_frame vcount vertex_attr_names set_vertex_attr get.vertex.attribute
#' @name Netreader
NULL

Netreader <- function(){
  rv <- reactiveValues(g = NULL,pathN=NULL,pathA=NULL,code = NULL)
  #ui ----
  ui <- miniPage(
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    tags$script(jscodeWidth),
    tags$script(jscodeHeight),
    tags$style(type = "text/css", ".selectize-dropdown{ width: 200px !important; }"),
    tags$style(type = "text/css",".form-group.shiny-input-container{width:50%;}"),
    tags$style(type = 'text/css', '#preview {background-color: rgba(0,0,0,0.50); color: white;}'),
    tags$style(type = 'text/css', '#previewA {background-color: rgba(0,0,0,0.50); color: white;}'),
    tags$style(type = 'text/css', '#netpreview {background-color: rgba(0,0,0,0.50); color: white;}'),
    tags$style(type = 'text/css', '#netpreviewA {background-color: rgba(0,0,0,0.50); color: white;}'),
    tags$style(type = 'text/css', '#codereview {background-color: rgba(0,0,0,1); color: white;}'),
    tags$style(type = 'text/css', '#readit {background-color: rgba(30,144,255,1); color: white}'),
    tags$style(type = 'text/css', '#readitA {background-color: rgba(30,144,255,1); color: white}'),
    tags$style(type = 'text/css', '#netfile {background-color: rgba(30,144,255,1); color: white}'),
    tags$style(type = 'text/css', '#attrfile {background-color: rgba(30,144,255,1); color: white}'),

    gadgetTitleBar("Netreader"),
    miniTabstripPanel(selected = 'Import Network',
      miniTabPanel("Import Network",icon = icon('bezier-curve'),
        fillRow(height="30px",width='100%',
                strong("Choose network file")
        ),
        fillRow(height = "50px", width = '50%',
          # fileInput("netfile", "Choose network file")
          actionButton("netfile","Browse...")
        ),
        fillRow(height = "50px", width = '75%',
          verbatimTextOutput("netfilePath")
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
        hr(),
        fillRow(height = line.height, width = '100%',
              actionButton("readit","Import Network"),
              textAreaInput("text",label = NA,value = "",placeholder = "enter name",height="35px")
        ),
        fillRow(height = "120px", width = '100%',
                verbatimTextOutput("netpreview")
        )
      ),
      #attributes ----
      miniTabPanel("Add Attributes",icon = icon("list-ol"),
           fillRow(height="30px",width='100%',
                   strong("Choose attribute file")
           ),
           fillRow(height = "50px", width = '100%',
                   # fileInput("attrfile", "Choose attribute file")
                   actionButton("attrfile","Browse...")
           ),
           fillRow(height="50px", width = "75%",
                   verbatimTextOutput("attrfilePath")
           ),
           fillRow(height = line.height, width = '100%',
                   h4("File Preview (first 5 lines)")
           ),
           fillRow(height = "120px", width = '100%',
                   verbatimTextOutput("previewA")
           ),
           fillRow(height = line.height, width='50%',
                   checkboxInput("colnamesA","Header",value=TRUE),
                   checkboxInput("quotesA","Quotes",value=FALSE),
                   radioButtons("valsepA","file delimiter", choices=c("comma"=",","space"=" ","tab"="\t"))
           ),
           tags$hr(),
           fillRow(height = line.height, width = '100%',
                   actionButton("readitA","Import Attributes")
           ),
           fillRow(height = "120px", width = '100%',
                   verbatimTextOutput("netpreviewA")
           )
      ),
      #show code ----
      miniTabPanel("Review Code",icon = icon("code"),
          verbatimTextOutput("codereview")
      )
    )
  )
  #server ----

  server <- function(input, output, session) {

    #choose netfile path ----
    observeEvent(input$netfile,{
        rv$pathN <- file.choose()
    })
    #choose attrfile path ----
    observeEvent(input$attrfile,{
      rv$pathA <- file.choose()
    })

    output$netfilePath <- renderPrint(
      if(!is.null(rv$pathN)){
        cat(rv$pathN)
      } else{
        cat("no file selected")
      }
    )

    output$attrfilePath <- renderPrint(
      if(!is.null(rv$pathA)){
        cat(rv$pathA)
      } else{
        cat("no file selected")
      }
    )
    # file preview ----
    output$preview <- renderText({
      # inFile <- input$netfile
      inFile <- rv$pathN
      if (is.null(inFile)) return(NULL)
      txt <- readLines(inFile,n=5)
      txt <- paste(txt,collapse="\n")
      txt
    })

    #attribute preview ----
    output$previewA <- renderText({
      # inFile <- input$attrfile
      inFile <- rv$pathA
      if (is.null(inFile)) return(NULL)
      txt <- readLines(inFile,n=5)
      txt <- paste(txt,collapse="\n")
      txt
    })

    #network preview ----
    output$netpreview <- renderPrint({
      g <- rv$g
      if (is.null(g)) return(cat("no network created yet."))
      summary(g)
    })

    #network2 preview ----
    output$netpreviewA <- renderPrint({
      g <- rv$g
      if (is.null(g)) return(cat("no network created yet."))
      summary(g)
    })

    # codeoutput ----
    output$codereview <- renderPrint({
      cat(rv$code)
    })
    # read network ----
    observeEvent(input$readit,{
      inFile <- rv$pathN
      q <- ifelse(input$quotes,"\"","")
      if(input$rownames){
      A <- tryCatch(utils::read.table(inFile,
                               header = input$colnames,
                               row.names = 1,
                               sep = input$valsep,quote = q,
                               stringsAsFactors = FALSE),
                    error=function(e) NULL)
      head <- "library(igraph)\n\n# load raw network data ----\n"
      cmd <- paste0("A <- utils::read.table(file = '",inFile,"'",
                    ",\n                header = ", input$colnames,", row.names = 1",
                    ", sep = '",input$valsep,"'",", quote = '",q,"', stringsAsFactors = FALSE)\n")
      rv$code <- paste(head,cmd)
      } else{
        A <- tryCatch(utils::read.table(inFile,
                                 header = input$colnames,
                                 sep = input$valsep,quote = q,
                                 stringsAsFactors = FALSE),
                      error=function(e) NULL)
        head <- "library(igraph)\n# load raw network data ----\n"
        cmd <- paste0("A <- utils::read.table(file = '",inFile,"'",
                      ",\n                header = ", input$colnames,", sep = '",input$valsep,"'",
                      ", quote = '",q,"', stringsAsFactors = FALSE)\n")
        rv$code <- paste0(head,cmd)
      }
      if(is.null(A)){
        showNotification("something went wrong reading the file. Check your settings",type = "error",duration = 2)
      } else{
        if(input$readfct=="adjacency matrix"){
          mode <- ifelse(input$directed,"directed","undirected")
          g <- tryCatch(graph_from_adjacency_matrix(as.matrix(A),mode = mode),error=function(e) NULL)
          if(is.null(g)){
            showNotification("something went wrong creating the network.",type = "error",duration = 2)
          } else{
            head <- "# create network ----\n"
            cmd <- paste0("g <- graph_from_adjacency_matrix(as.matrix(A),mode = '",mode,"')\n")
            rv$code <- paste0(rv$code,head,cmd)
            rv$g <- g
            showNotification("network data successfully imported",type = "message",duration = 2)
          }
        }
        else if(input$readfct=="edgelist"){
          mode <- ifelse(input$directed,T,F)
          g <- tryCatch(graph_from_data_frame(A,directed = mode),error=function(e) NULL)
          if(is.null(g)){
            showNotification("something went wrong creating the network.",type = "error",duration = 2)
          } else{
            head <- "# create network ----\n"
            cmd <- paste0("g <- graph_from_data_frame(A,directed = ",mode,")\n")
            rv$code <- paste0(rv$code,"\n",head,cmd)
            rv$g <- g
            showNotification("network data successfully imported",type = "message",duration = 2)
          }
        }
      }

    })

    # read attributes ----
    observeEvent(input$readitA,{
      if(is.null(rv$g)){
        showNotification("please import a network first",type = "error",duration = 2)
      } else{
        # inFile <- input$attrfile
        inFile <- rv$pathA
        q <- ifelse(input$quotesA,"\"","")
        A <- tryCatch(utils::read.table(inFile,
                                 header = input$colnamesA,
                                 sep = input$valsepA,quote = q,
                                 stringsAsFactors = FALSE),
                      error=function(e) NULL)
        head <- "# load raw attribute data ----\n"
        cmd <- paste0("attrs <- utils::read.table(file = '",inFile,"'",
                      ",\n                    header = ", input$colnamesA,", sep = '",input$valsepA,"'",
                      ", quote = '",q,"', stringsAsFactors = FALSE)\n")
        rv$code <- paste0(rv$code,"\n",head,cmd)

        if(is.null(A)){
          showNotification("something went wrong reading the file. Check your settings",type = "error",duration = 2)
        } else{
          if(nrow(A)!=vcount(rv$g)){
            showNotification("The number of rows does not match the number of nodes in the network",type = "error",duration = 2)
          } else{
            if("name"%in%vertex_attr_names(rv$g)){
              vnames <- get.vertex.attribute(rv$g,"name")
              identCol <- which(apply(A,2,function(x) all(x%in%vnames)))[1]
              anames <- A[,identCol]
              A <- A[,-identCol]
              perm <- match(vnames,anames)
              for(attr in names(A)){
                rv$g <- set_vertex_attr(rv$g,name = attr,value = A[[attr]][perm])
              }
              head <- "# add attributes to network ----\n"
              cmd <- AttrNameImport
              rv$code <- paste0(rv$code,"\n",head,cmd)

            } else{
              showNotification("network does not have a name attribute.\nmatching by row number instead",type="warning",duration = 2)
              for(attr in names(A)){
                rv$g <- set_vertex_attr(rv$g,name = attr,value = A[[attr]])
              }
              head <- "# add attributes to network ----\n"
              cmd <- AttrRowImport
              rv$code <- paste0(rv$code,"\n",head,cmd)
            }
            showNotification("Attributes successfully imported",type = "message",duration = 2)
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
        showNotification("Please enter a variable name",type="warning",duration = 2)
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
