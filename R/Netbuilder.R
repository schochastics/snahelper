#' Netbuilder
#'
#' \code{Netbuilder} is a RStudio-Addin that allows you to create small networks.
#'
#' @details To run the addin, highlight an igraph-object in your current script and select \code{Netbuilder} from the Addins-menu within RStudio.
#' @return \code{Netbuilder} returns the created network as igraph object.
#' @import miniUI
#' @import shiny
#' @import ggplot2
#' @import ggraph
#' @import rstudioapi
#' @import igraph
#' @name Netbuilder
NULL

Netbuilder <- function(){
  rv <- reactiveValues(g = igraph::graph.empty(n = 0,directed=FALSE),xy = matrix(0,0,2),
                       start=-1,end=-1,el=matrix(0,0,2))
#ui ----
  ui <- miniPage(
    tags$script(jscodeWidth),
    tags$script(jscodeHeight),
    tags$style(type = "text/css", ".selectize-dropdown{ width: 200px !important; }"),
    tags$style(type = "text/css",".form-group.shiny-input-container{width:50%;}"),

    gadgetTitleBar("Netbuilder"),
    plotOutput("Graph1", width = '80%', height = '80%',click = "add_edge",dblclick = "add_vertex"),
               # hover = hoverOpts(id = "showXY",delay = 500,delayType = "throttle")),
    fillRow(height = line.height, width = '75%',
      textAreaInput("text",label = NA,value = "graph"),
      actionButton("makeU","make undirected"),
      actionButton("makeD","make directed"),
      actionButton("clearG","clear"),
      verbatimTextOutput("coordXY")
    )

  )

#server ----
  server <- function(input, output, session) {
    #--------------------#
    #observe click ----
    #--------------------#
    shiny::observeEvent(input$add_edge,{
      pts <- c(input$add_edge$x,input$add_edge$y)
      g <- rv$g
      xy <- rv$xy
      start <- rv$start
      end <- rv$end
      closest_nID <- closest_node(pts,xy)
      if(start==-1){
        start <- closest_nID
        rv$start <- start
      } else{
        end <- closest_nID
        if(end!=-1){
          g <- igraph::add.edges(g,c(start,end))
          rv$start <- -1
          rv$end <- -1
          rv$g <- g
          rv$el <- rbind(rv$el,c(start,end))
        } else{
          g <- igraph::add.vertices(g,1)
          rv$start <- -1
          rv$end <- -1
          xy <- rv$xy
          xy <- rbind(xy,pts)
          g <- igraph::add.edges(g,c(start,nrow(xy)))
          rv$xy <- xy
          rv$g <- g
        }
      }
      gg_reactive()

    })
    #--------------------#
    #observe dbclick ----
    #--------------------#
    shiny::observeEvent(input$add_vertex,{
      g <- rv$g
      g <- igraph::add.vertices(g,1)
      rv$g <- g
      xy <- rv$xy
      xy <- rbind(xy,c(input$add_vertex$x,input$add_vertex$y))
      rv$xy <- xy
      gg_reactive()

    })
    #--------------------#
    #observe dbclick ----
    #--------------------#
    # output$coordXY <- renderPrint({
    #   cat("Hover:\n")
    #   str(input$showXY)
    # })
    #--------------------#
    # make (un)directed ----
    #--------------------#
    shiny::observeEvent(input$makeD,{
      if(igraph::ecount(rv$g)!=0){
        # el <- igraph::get.edgelist(rv$g)
        el <- rv$el
        g <- igraph::graph_from_edgelist(el,directed = TRUE)
        rv$g <- g
      }
    })
    shiny::observeEvent(input$makeU,{
      if(igraph::ecount(rv$g)!=0){
        # el <- igraph::get.edgelist(rv$g)
        el <- rv$el
        g <- igraph::graph_from_edgelist(el,directed = FALSE)
        rv$g <- g
      }
    })

    shiny::observeEvent(input$clearG,{
        g <- graph.empty(n=0)
        rv$g <- g
        xy <- matrix(0,0,2)
        rv$xy <- xy
        rv$el <- xy
    })
    #-------------------#
    # plot ----
    #-------------------#
    gg_reactive <- reactive({
      code_layout <- "ggraph(rv$g,layout = \"manual\", x = rv$xy[,1], y = rv$xy[,2])"
      code_nodes  <- "geom_node_point(shape = 21,fill = \"grey25\",size=8)"
      code_label <- "geom_node_text(label=1:nrow(rv$xy),size=6,col=\"white\")"
      if(igraph::is.directed(rv$g)){
        code_arrow <- paste0(",\narrow = arrow(angle = 25, length = unit(0.15, \"inches\")",
                                      ",\nends = \"last\", type = \"closed\")",
                                      ",\nend_cap = circle(",8,", \"pt\"))")
        code_edges  <- "geom_edge_link(edge_width=0.4,edge_colour=\"grey66\""

        code_edges <- paste0(code_edges,", ",code_arrow)
      } else{
        code_edges  <- "geom_edge_link0(edge_width=0.4,edge_colour=\"grey66\")"
      }
      code_theme  <- "theme_graph(foreground=\"black\",border=TRUE)"
      code_scale <- "scale_x_continuous(limits=c(0,10)) + scale_y_continuous(limits=c(0,10))"
      code <- paste(code_layout,code_edges,code_nodes,code_label,code_theme,code_scale,sep=" + ")
      p <- code

      return(p)
    })

    # render plot
    ggnet <- renderPlot( {
      eval(parse(text = gg_reactive()))
    })
    output$Graph1 <- ggnet

    # cancel ----
    observeEvent(input$cancel, {
      invisible(stopApp())
    })

    #done ----
    observeEvent(input$done, {
      V(rv$g)$x <- rv$xy[,1]
      V(rv$g)$y <- rv$xy[,2]
      eval(parse(text = paste0("assign(\"",input$text,"\",rv$g",",envir = .GlobalEnv)")))
      invisible(stopApp())
    })
  }


  viewer <- dialogViewer(dialogName = 'Netbuilder', width = 990, height = 900)
  runGadget(ui, server, stopOnCancel = FALSE, viewer = viewer)
}


#' @export
#' @rdname Netbuilder

NetbuilderAddin <- function() {
  Netbuilder()
}
