#' Componentlayouter
#'
#' \code{Componentlayouter} is an RStudio-Addin that facilitates layouting networks with several components.
#'
#' @details To run the addin, highlight an igraph-object in your current script and select \code{Componentlayouter} from the Addins-menu within RStudio.
#' @return \code{Componentlayouter} returns the layout as xy coordinates.
#' @import miniUI
#' @import shiny
#' @import ggplot2
#' @import ggraph
#' @import rstudioapi
#' @import igraph
#' @name Componentlayouter
NULL

Componentlayouter <- function(text){
  if (!requireNamespace("graphlayouts", quietly = TRUE)) {
    stop("graphlayouts package not found. Install it with install.packages('graphlayouts')", call. = FALSE)
  }
  if (any(ls(envir = .GlobalEnv) == text)) {
    g <- get(text, envir = .GlobalEnv)
    if(!igraph::is.igraph(g)){
      stop(paste0(text, ' is not an igraph object'))
    }
    xy <- graphlayouts::layout_with_stress(g)
    #xy[,1] <- xy[,1] - min(xy[,1])#normalise(xy[,1],to = c(2,3))
    #xy[,2] <- xy[,2] - min(xy[,2])#normalise(xy[,2],to = c(2,3))
  } else {
    stop(paste0('Couldn\'t find  the graph ', text, '.'))
  }
  comps <- igraph::components(g)
  V(g)$grp <- comps$membership
  rv <- reactiveValues(xy=xy,grp=comps$membership,placed=rep(FALSE,nrow(xy)),g1=igraph::graph.empty(),xy1=xy)
  #ui ----
  ui <- miniPage(
    tags$script(jscodeWidth),
    tags$script(jscodeHeight),
    tags$style(type = "text/css", ".selectize-dropdown{ width: 200px !important; }"),
    tags$style(type = "text/css",".form-group.shiny-input-container{width:50%;}"),
    tags$style(type = "text/css","#nextC{margin-top:24px;}"),

    gadgetTitleBar("Component Layouter"),
    fillRow(height = line.height, width = '85%',flex = c(2,1,3),
            selectizeInput('compId', label = 'Component', choices = order(comps$csize,decreasing=TRUE),
                           selected = 0, width = input.width),
            actionButton(inputId = "nextC",label = "next"),
            checkboxInput("ggforce","component labels",value=FALSE)
    ),
    fillRow(flex=c(4,1),height = "80%",
      plotOutput("Graph1", width = '100%', height = '100%',click = "move"),
      plotOutput("Graph2", width = '100%',height='50%')
    ),
    fillRow(width = "50%",height = line.height,
      actionButton(inputId = "rotateL",label = "rotate left"),
      actionButton(inputId = "rotateR",label = "rotate right"),
      actionButton(inputId = "shrink",label = "shrink"),
      actionButton(inputId = "grow",label = "grow")
    )
  )

  #server ----
  server <- function(input, output, session) {
    #--------------------#
    #observe next ----
    #--------------------#
    shiny::observeEvent(input$nextC,{
      ord <- order(comps$csize,decreasing=TRUE)
      id <- which(ord==as.numeric(input$compId))
      if(id==length(ord)){
        sel <- ord[1]
      } else{
        sel <- ord[id+1]
      }


      updateSelectizeInput(session = session, inputId = 'compId',label = 'Component',
                           choices = order(comps$csize,decreasing=TRUE), selected = sel)
    })

    #--------------------#
    #observe click ----
    #--------------------#
    shiny::observeEvent(input$move,{
      mxy <- c(input$move$x,input$move$y)
      ids <- which(rv$grp==input$compId)
      xy_grp <- rv$xy[ids,]
      meanx <- mean(xy_grp[,1])
      meany <- mean(xy_grp[,2])
      xy_grp[,1] <- xy_grp[,1]-meanx+mxy[1]
      xy_grp[,2] <- xy_grp[,2]-meany+mxy[2]

      xy_grp <- boundary_check(xy_grp,10,10)

      rv$xy[ids,] <- xy_grp
      rv$placed[ids] <- TRUE
      gg_reactive()

    })
    #--------------------#
    #observe rotators ----
    #--------------------#
    shiny::observeEvent(input$rotateL,{
      ids <- which(rv$grp==input$compId)
      xy_grp <- rv$xy[ids,]
      meanx1 <- mean(xy_grp[,1])
      meany1 <- mean(xy_grp[,2])
      xy_grp <- graphlayouts::layout_rotate(xy_grp,10)
      meanx2 <- mean(xy_grp[,1])
      meany2 <- mean(xy_grp[,2])

      xy_grp[,1] <- xy_grp[,1]-meanx2+meanx1
      xy_grp[,2] <- xy_grp[,2]-meany2+meany1

      xy_grp <- boundary_check(xy_grp,10,10)

      rv$xy[ids,] <- xy_grp
    })

    shiny::observeEvent(input$rotateR,{
      ids <- which(rv$grp==input$compId)
      xy_grp <- rv$xy[ids,]
      meanx1 <- mean(xy_grp[,1])
      meany1 <- mean(xy_grp[,2])
      xy_grp <- graphlayouts::layout_rotate(xy_grp,-10)
      meanx2 <- mean(xy_grp[,1])
      meany2 <- mean(xy_grp[,2])

      xy_grp[,1] <- xy_grp[,1]-meanx2+meanx1
      xy_grp[,2] <- xy_grp[,2]-meany2+meany1

      xy_grp <- boundary_check(xy_grp,10,10)

      rv$xy[ids,] <- xy_grp
    })
    #--------------------#
    #observe shrink ----
    #--------------------#
    shiny::observeEvent(input$shrink,{
      ids <- which(rv$grp==input$compId)
      xy_grp <- rv$xy[ids,]

      xy_grp <- shrink(xy_grp,0.1)
      xy_grp <- boundary_check(xy_grp,10,10)

      rv$xy[ids,] <- xy_grp
    })
    #--------------------#
    #observe grow ----
    #--------------------#
    shiny::observeEvent(input$grow,{
      ids <- which(rv$grp==input$compId)
      xy_grp <- rv$xy[ids,]

      xy_grp <- shrink(xy_grp,-0.1)
      xy_grp <- boundary_check(xy_grp,10,10)

      rv$xy[ids,] <- xy_grp
    })
    #-------------------#
    # plot ----
    #-------------------#
    gg_reactive <- reactive({
      if(all(!rv$placed)){
        xscale <- 10
        yscale <- 10
        code_scale  <- paste0("scale_x_continuous(limits=c(0,",xscale,")) + scale_y_continuous(limits=c(0,",yscale,"))")
        empty <- "ggraph(igraph::graph.empty(),layout = \"circle\")+theme_graph(foreground=\"black\",border=TRUE)"
        p <- paste(empty,code_scale,sep=" + ")
      } else{
        idx <- which(rv$placed)
        rv$g1 <- induced_subgraph(g,idx)
        rv$xy1 <- rv$xy[idx,]
        # print(rv$xy1)
        xscale <- 10
        yscale <- 10

        code_layout <- "ggraph(rv$g1,layout = \"manual\", x = rv$xy1[,1], y = rv$xy1[,2])"
        code_nodes  <- "geom_node_point(shape = 21,fill = \"grey25\",size=2)"
        code_edges  <- "geom_edge_link0(edge_width=0.2,edge_colour=\"grey66\")"
        code_theme  <- "theme_graph(foreground=\"black\",border=TRUE)"
        code_scale  <- paste0("scale_x_continuous(limits=c(0,",xscale,")) + scale_y_continuous(limits=c(0,",yscale,"))")

        if(input$ggforce){
          if (!requireNamespace("ggforce", quietly = TRUE)) {
            stop("ggforce required. Install it with install.packages('ggforce')", call. = FALSE)
          }
          code_grp <- "ggforce::geom_mark_hull(aes(x, y, group = grp, label=grp),concavity = 4,  expand = unit(2, \"mm\"))"
          code <- paste(code_layout,code_edges,code_nodes,code_grp,code_scale,code_theme,sep=" + ")
        } else{
          code <- paste(code_layout,code_edges,code_nodes,code_scale,code_theme,sep=" + ")
        }

        p <- code
      }
      return(p)
    })

    # render plot ----
    ggnet <- renderPlot( {
      eval(parse(text = gg_reactive()))
    })
    output$Graph1 <- ggnet


    previewPlot <- reactive({
      idx <- which(rv$grp==as.numeric(input$compId))
      preview <- igraph::induced_subgraph(g,idx)
      xy_prev <- rv$xy[idx,]
      ggraph(preview,"manual",x=xy_prev[,1],y=xy_prev[,2]) +
        geom_edge_link0(edge_width=0.2,edge_colour="grey66") +
        geom_node_point(shape = 21,fill = "grey25",size=2) +
        theme_graph(foreground = "black",title_size=12,title_face="plain",border = TRUE)+
        labs(title=paste0("Preview of Component ",input$compId))
    })

    output$Graph2 <- renderPlot({previewPlot()})
    #  DONE -----
    observeEvent(input$done, {
      # rv$xy <- round(rv$xy,2)
      # result <- paste0("xy <- ",paste0(deparse(rv$xy),collapse="\n"))
      # result <- formatR::tidy_source(text=result,output = FALSE)$text.tidy
      # rstudioapi::insertText(result)
      V(g)$x <- rv$xy[,1]
      V(g)$y <- rv$xy[,2]
      eval(parse(text = paste0("assign(\"",text,"\",g",",envir = .GlobalEnv)")))
      invisible(stopApp())
    })

    # cancel ----
    observeEvent(input$cancel, {
      invisible(stopApp())
    })

  }
  viewer <- dialogViewer(dialogName = 'Componentlayouter', width = 1200, height = 1000)
  runGadget(ui, server, stopOnCancel = FALSE, viewer = viewer)
}






#' @export
#' @rdname Componentlayouter

ComponentlayouterAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()

  text <- context$selection[[1]]$text
  # text <- "gr"
  if (nchar(text) == 0) {
    stop('Please highlight an igraph object before using this addin.')
  }

  Componentlayouter(text)
}



normalise <- function (x, from = range(x), to = c(0, 1))
{
  x <- (x - from[1])/(from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}


boundary_check <- function(xy,xmax,ymax){
  if(any(xy[,1]<0)){
    xy[,1] <- xy[,1]+abs(min(xy[,1])*1.05)
  }
  if(any(xy[,2]<0)){
    xy[,2] <- xy[,2]+abs(min(xy[,2])*1.05)
  }
  if(any(xy[,1]>xmax)){
    xy[,1] <- xy[,1]-abs(max(xy[,1]-xmax)*1.05)
  }

  if(any(xy[,2]>ymax)){
    xy[,2] <- xy[,2]-abs(max(xy[,2]-ymax)*1.05)
  }

  xy
}

shrink <- function(xy,fac){
  mx <- mean(xy[,1])
  my <- mean(xy[,2])
  xy[,1] <- (1-fac)*xy[,1]+fac*mx
  xy[,2] <- (1-fac)*xy[,2]+fac*my
  xy
}
