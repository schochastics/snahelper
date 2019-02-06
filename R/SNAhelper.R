#' SNAhelper
#'
#' \code{SNAhelper} is a RStudio-Addin that provides a graphical interface for network analysis and visualization.
#'
#' @details To run the addin, highlight an igraph-object in your current script and select \code{SNAhelper} from the Addins-menu within RStudio. After terminating the addin, a character string containing the code for visualization is inserted in your current script.
#'
#' @return \code{SNAelper} returns a character vector with code.
#' @import miniUI
#' @import shiny
#' @import ggplot2
#' @import ggraph
#' @import rstudioapi
#' @import igraph
#' @import colourpicker
#' @importFrom grDevices col2rgb
#' @importFrom grDevices colors
#' @name SNAhelper
NULL

SNAhelper <- function(text){
  if (!requireNamespace("smglr", quietly = TRUE)) {
    stop("smglr package not found. Install it with devtools::install_github('schochastics/smglr')", call. = FALSE)
  }

  if (any(ls(envir = .GlobalEnv) == text)) {
    g <- get(text, envir = .GlobalEnv)
    g_iso <- delete.vertices(g,which(degree(g)==0))
    xy <- smglr::layout_with_stress(g)
    rv <- reactiveValues(g=g,xy=xy,
                         g_iso=g_iso,xy_iso=smglr::layout_with_stress(g_iso),
                         g_all=g,xy_all=xy)
    if(!igraph::is.igraph(g)){
      stop(paste0(text, 'is not an igraph object'))
    }
  } else {
    stop(paste0('Couldn\'t find  the graph ', text, '.'))
  }

  #ui ----
  ui <- miniPage(
    tags$script(jscodeWidth),
    tags$script(jscodeHeight),
    tags$style(type = "text/css", ".selectize-dropdown{ width: 200px !important; }"),
    tags$style(type = "text/css",".form-group.shiny-input-container{width:50%;}"),

    gadgetTitleBar("SNA helper"),
    miniTabstripPanel(selected = 'layout',
                      miniTabPanel("layout", icon = icon('sliders'),
                                   plotOutput("Graph1", width = '100%', height = '55%'),
                                   miniContentPanel(
                                     scrollable = TRUE,
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Choose Layout')
                                     ),
                                     fillRow(height = line.height, width = '100%',
                                             selectizeInput('graph.layout',label="Layout Algorithm",
                                                            choices=layouts.available,
                                                            selected="smglr::layout_with_stress",width=input.width),
                                             selectInput('legendPos', label = 'Show Legend',
                                                         choices = c("none","top","bottom","left","right"),
                                                         width = input.width)
                                     ),
                                     fillRow(height = line.height, width = '100%',
                                             actionButton("do.layout","Calculate Layout"),
                                             checkboxInput("showIso", label = "Show Isolates", value = TRUE)
                                     ),
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Tweak Layout'),
                                             checkboxInput("showLabs", label = "Show NodeIDs", value = FALSE)

                                     ),
                                     fillRow(height = line.height, width = '100%',
                                             selectizeInput('nodeId', label = 'NodeID', choices = 1:vcount(g),
                                                            width = input.width),
                                             numericInput('nudgeX', label = 'moveX',
                                                          min = -5, max = 5, step = 0.01, value = 0,width=input.width),
                                             numericInput('nudgeY', label = 'moveY',
                                                          min = -5, max = 5, step = 0.01, value = 0,width=input.width)
                                     )
                                   )
                      ),
                      miniTabPanel("node attribute manager",icon = icon("list-ol"),
                                   miniContentPanel(
                                     scrollable = TRUE,
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Node Attribute Manager')
                                     ),
                                     dataTableOutput("attrManageN"),
                                     fillRow(height = line.height, width = '100%',
                                             selectizeInput('centindex', label = 'Index',
                                                            choices = c("Degree" = "degree(rv$g)",
                                                                        "Betwenness" = "betweenness(rv$g)",
                                                                        "Closeness" = "closeness(rv$g)",
                                                                        "Eigenvector" = "eigen_centrality(rv$g)$vector"
                                                            ),
                                                            width = input.width),
                                             selectizeInput('clusteralg', label = 'Clustering',
                                                            choices = c("Louvain" = "cluster_louvain(rv$g)"),
                                                            width = input.width)
                                     ),
                                     fillRow(height=line.height, width = '100%',
                                             actionButton("calcIndex","Calculate Index"),
                                             actionButton("calcClust","Calculate Clustering")
                                     )
                                   )
                      ),
                      miniTabPanel("nodes", icon = icon('circle'),
                                   plotOutput("Graph2", width = '100%', height = '55%'),
                                   miniContentPanel(
                                     scrollable = TRUE,
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Manual')
                                     ),
                                     fillRow(height = line.height, width = '100%',
                                             # selectizeInput('nodeColMan', label = 'Colour', choices = NULL,
                                             #                width = input.width),
                                             colourpicker::colourInput('nodeColMan',label="Colour",value = "gray32"),
                                             numericInput('nodeSizeMan', label = 'Size',
                                                          min = 0, max = 20, step = 0.5, value = 5,width = input.width),
                                             colourpicker::colourInput('nodeBorderColMan',label="Border Colour",value = "black"),
                                             numericInput('nodeBorderSizeMan', label = 'Border Size',
                                                          min = 0, max = 2, step = 0.1, value = 0.3,width=input.width)
                                     ),
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Attribute')
                                     ),
                                     fillRow(height = line.height, width = '100%',
                                             selectizeInput('nodeColAttr', label = 'Colour (Cont.)',
                                                            choices = NULL,
                                                            width = input.width),
                                             selectizeInput('nodeColAttrD', label = 'Colour (Discrete)',
                                                            choices = NULL,
                                                            width = input.width),
                                             selectizeInput('nodeSizeAttr', label = 'Size (Cont.)',
                                                            choices = NULL,
                                                            width = input.width),
                                             selectizeInput('nodeLabelAttr', label = 'Node Label',
                                                            choices = NULL,
                                                            width = input.width)
                                     ),
                                     fillRow(height=line.height,width='100%',
                                             shiny::conditionalPanel("input.nodeColAttr!='None'",
                                                                     colourpicker::colourInput('nodeColAttrL',label="Min Colour",value = "skyblue1"),
                                                                     colourpicker::colourInput('nodeColAttrH',label="Max Colour",value = "royalblue4")
                                                                     # selectizeInput('nodeColAttrL',label = 'Min Colour',
                                                                     #                choices = NULL, width = input.width),
                                                                     # selectizeInput('nodeColAttrH',label = 'Max Colour',
                                                                     #                choices = NULL, width = input.width)
                                             ),
                                             shiny::conditionalPanel("input.nodeColAttrD!='None'",
                                                                     selectizeInput('nodeColAttrP',label = 'Palette',
                                                                                    choices = c("Set1","Set2","Set3","Pastel2","Pastel1",
                                                                                                "Paired","Dark2","Accent"),
                                                                                    width = input.width)
                                             ),
                                             shiny::conditionalPanel("input.nodeSizeAttr!='None'",
                                                                     numericInput('nodeSizeAttrL', label = 'Min Size',
                                                                                  min = 0, max = 20, step = 0.5, value = 3,width=input.width),
                                                                     numericInput('nodeSizeAttrH', label = 'Max Size',
                                                                                  min = 0, max = 20, step = 0.5, value = 8,width=input.width)
                                             ),
                                             shiny::conditionalPanel("input.nodeLabelAttr!='None'",
                                                                     colourpicker::colourInput('nodeLabelCol',label="Colour",value = "black"),
                                                                     # selectizeInput('nodeLabelCol',label = 'Colour',
                                                                     #                choices = NULL, width = input.width),
                                                                     numericInput('nodeLabelSize', label = 'Size',
                                                                                  min = 0, max = 20, step = 0.5, value = 6,width=input.width),
                                                                     selectizeInput('nodeLabelFont',label = 'Font',
                                                                                    choices = fonts.available, width = input.width),
                                                                     shiny::checkboxInput('nodeLabelRepel',label = 'Repel Labels?',value = FALSE)
                                             )

                                     )
                                   )
                      ),
                      miniTabPanel("edge attribute manager",icon = icon("list-ol"),
                                   miniContentPanel(
                                     scrollable = TRUE,
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Edge Attribute Manager')
                                     ),
                                     dataTableOutput("attrManageE")#,
                                     # fillRow(height = line.height, width = '100%',
                                     #         selectizeInput('centindex', label = 'Index',
                                     #                        choices = c("Degree" = "degree(rv$g)",
                                     #                                    "Betwenness" = "betweenness(rv$g)",
                                     #                                    "Closeness" = "closeness(rv$g)",
                                     #                                    "Eigenvector" = "eigen_centrality(rv$g)$vector"
                                     #                        ),
                                     #                        width = input.width),
                                     #         selectizeInput('clusteralg', label = 'Clustering',
                                     #                        choices = c("Louvain" = "cluster_louvain(rv$g)"),
                                     #                        width = input.width)
                                     # ),
                                     # fillRow(height=line.height, width = '100%',
                                     #         actionButton("calcIndex","Calculate Index"),
                                     #         actionButton("calcClust","Calculate Clustering")
                                     # )
                                   )
                      ),
                      miniTabPanel("edges", icon = icon('minus'),
                                   plotOutput("Graph3", width = '100%', height = '55%'),
                                   miniContentPanel(
                                     scrollable = TRUE,
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Manual')
                                     ),
                                     fillRow(height = line.height, width = '100%',
                                             colourpicker::colourInput('edgeColMan',label="Colour",value = "gray66"),
                                             # selectizeInput('edgeColMan', label = 'Colour', choices = NULL,
                                             #                width = input.width),
                                             numericInput('edgeSizeMan', label = 'Width',
                                                          min = 0, max = 10, step = 0.1, value = 0.8,width=input.width),
                                             numericInput('edgeAlphaMan', label = 'Alpha',
                                                          min = 0, max = 1, step = 0.01, value = 1.0,width=input.width),
                                             numericInput('edgeNMan', label = 'Pts. per Edge',
                                                          min = 2, max = 100, step = 1, value = 2,width=input.width)
                                     ),
                                     fillRow(height = heading.height, width = '100%',
                                             headingOutput('Attribute')
                                     ),
                                     fillRow(height = line.height, width = '75%',
                                             selectizeInput('edgeColAttr', label = 'Colour (Cont.)',
                                                            choices = NULL,
                                                            width = input.width),
                                             selectizeInput('edgeSizeAttr', label = 'Width (Cont.)',
                                                            choices = NULL,
                                                            width = input.width),
                                             selectizeInput('edgeAlphaAttr', label = 'Alpha (Cont.)',
                                                            choices = NULL,
                                                            width = input.width)
                                     ),
                                     fillRow(height=line.height,width='75%',
                                             shiny::conditionalPanel("input.edgeColAttr!='None'",
                                                                     colourpicker::colourInput('edgeColAttrL',label="Min Colour",value = "skyblue1"),
                                                                     colourpicker::colourInput('edgeColAttrH',label="Max Colour",value = "royalblue4")
                                                                     # selectizeInput('edgeColAttrL',label = 'Min Colour',
                                                                     #                choices = NULL, width = input.width),
                                                                     # selectizeInput('edgeColAttrH',label = 'Max Colour',
                                                                     #                choices = NULL, width = input.width)
                                             ),
                                             shiny::conditionalPanel("input.edgeSizeAttr!='None'",
                                                                     numericInput('edgeSizeAttrL', label = 'Min Width',
                                                                                  min = 0, max = 10, step = 0.1, value = 0.3,width=input.width),
                                                                     numericInput('edgeSizeAttrH', label = 'Max Width',
                                                                                  min = 0, max = 10, step = 0.1, value = 1.2,width=input.width)
                                             ),
                                             shiny::conditionalPanel("input.edgeAlphaAttr!='None'",
                                                                     numericInput('edgeAlphaAttrL', label = 'Min Alpha',
                                                                                  min = 0, max = 1, step = 0.01, value = 0.1,width=input.width),
                                                                     numericInput('edgeAlphaAttrH', label = 'Max Alpha',
                                                                                  min = 0, max = 1, step = 0.01, value = 1,width=input.width)
                                             )

                                     )
                                   )
                      ),
                      miniTabPanel("result", icon = icon('bezier-curve'),
                                   plotOutput("Graph4", width = '100%', height = '95%'))

    ))



  server <- function(input, output, session) {
    #####################
    #constants ----
    #####################
    # colour.choices <- colours2HEX(colours.available)
    vattr.to.aes <- igraph::vertex_attr_names(g)[!grepl("name",igraph::vertex_attr_names(g))]
    if(length(vattr.to.aes)>0){
    idC <- which(sapply(vattr.to.aes,function(x) is.numeric(igraph::get.vertex.attribute(g,x))))
    vattrC.to.aes <- c("None",vattr.to.aes[idC])
    idC <- which(sapply(vattr.to.aes,function(x) !is.numeric(igraph::get.vertex.attribute(g,x))))
    vattrD.to.aes <- c("None",vattr.to.aes[idC])
    } else{
      vattrC.to.aes <- c("None")
      vattrD.to.aes <- c("None")
    }

    eattr.to.aes <- igraph::edge_attr_names(g)
    if(length(eattr.to.aes)>0){
      idC <- which(sapply(eattr.to.aes,function(x) is.numeric(igraph::get.edge.attribute(g,x))))
      eattrC.to.aes <- c("None",eattr.to.aes[idC])
    } else{
      eattrC.to.aes <- c("None")
    }
    #####################
    #initialize selectors ----
    #####################
    # updateSelectizeInput(session = session, inputId = 'nodeColMan',
    #                      choices = colour.choices, selected = "gray26", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))

    # updateSelectizeInput(session = session, inputId = 'nodeBorderColMan',
    #                      choices = colour.choices, selected = "black", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))

    updateSelectizeInput(session = session, inputId = 'nodeColAttr',
                         choices = vattrC.to.aes, selected = "None", server = TRUE,
                         options = list(create = TRUE))

    updateSelectizeInput(session = session, inputId = 'nodeLabelAttr',
                         choices = c("None",igraph::vertex_attr_names(g)),
                         selected = "None", server = TRUE,
                         options = list(create = TRUE))

    updateSelectizeInput(session = session, inputId = 'nodeColAttrD',
                         choices = vattrD.to.aes, selected = "None", server = TRUE,
                         options = list(create = TRUE))

    updateSelectizeInput(session = session, inputId = 'nodeSizeAttr',
                         choices = vattrC.to.aes, selected = "None", server = TRUE,
                         options = list(create = TRUE))

    # updateSelectizeInput(session = session, inputId = 'nodeColAttrL',
    #                      choices = colour.choices, selected = "skyblue1", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))
    #
    # updateSelectizeInput(session = session, inputId = 'nodeColAttrH',
    #                      choices = colour.choices, selected = "royalblue4", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))

    # updateSelectizeInput(session = session, inputId = 'nodeLabelCol',
    #                      choices = colour.choices, selected = "black", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))

    # updateSelectizeInput(session = session, inputId = 'edgeColMan',
    #                      choices = colour.choices, selected = "gray66", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))

    updateSelectizeInput(session = session, inputId = 'edgeColAttr',
                         choices = eattrC.to.aes, selected = "None", server = TRUE,
                         options = list(create = TRUE))

    updateSelectizeInput(session = session, inputId = 'edgeSizeAttr',
                         choices = eattrC.to.aes, selected = "None", server = TRUE,
                         options = list(create = TRUE))

    updateSelectizeInput(session = session, inputId = 'edgeAlphaAttr',
                         choices = eattrC.to.aes, selected = "None", server = TRUE,
                         options = list(create = TRUE))

    # updateSelectizeInput(session = session, inputId = 'edgeColAttrL',
    #                      choices = colour.choices, selected = "skyblue1", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))
    #
    # updateSelectizeInput(session = session, inputId = 'edgeColAttrH',
    #                      choices = colour.choices, selected = "royalblue4", server = TRUE,
    #                      options = list(create = TRUE, labelField = 'name',
    #                                     searchField = 'colour', valueField = 'colour',
    #                                     render = jsColourSelector))

    #####################
    #be sure either discrete or continuos is selected ----
    #####################
    shiny::observe({
      if(input$nodeColAttr!="None"){
        shiny::updateSelectInput(session,"nodeColAttrD",selected="None")
      }
    })

    shiny::observe({
      if(input$nodeColAttrD!="None"){
        shiny::updateSelectInput(session,"nodeColAttr",selected="None")
      }
    })

    #####################
    #calculate initial layout ----
    #####################
    # xy <- smglr::layout_with_stress(g)
    # get_layout <- eventReactive(input$do.layout, {
    #   xy <- eval(parse(text = paste0(input$graph.layout,"(rv$g)")))
    #   gg_reactive()
    # })
    shiny::observeEvent(input$do.layout,{
      xy <- eval(parse(text = paste0(input$graph.layout,"(rv$g)")))
      rv$xy <- xy
      gg_reactive()
    })
    #####################
    #tweak layout ----
    #####################
    shiny::observeEvent(input$nodeId,{
      idN <- as.numeric(input$nodeId)
      xmin <- rv$xy[idN,1]-3
      xmax <- rv$xy[idN,1]+3
      xsel <- rv$xy[idN,1]
      updateNumericInput(session = session,'nudgeX','moveX', value = xsel,
                         min = xmin,max = xmax,step = 0.01)

      ymin <- rv$xy[idN,2]-3
      ymax <- rv$xy[idN,2]+3
      ysel <- rv$xy[idN,2]
      updateNumericInput(session = session,'nudgeY','moveY', value = ysel,
                         min = ymin,max = ymax,step = 0.01)
    })

    shiny::observeEvent(input$nudgeX,{
      indX <- as.numeric(input$nodeId)
      # x <- rv$xy[indX,1]
      # x <- x+input$nudgeX
      # rv$xy[indX,1] <- x
      rv$xy[indX,1] <- input$nudgeX
      gg_reactive()

    })
    shiny::observeEvent(input$nudgeY,{
      indX <- as.numeric(input$nodeId)
      # x <- rv$xy[indX,2]
      # x <- x+input$nudgeY
      # rv$xy[indX,2] <- x
      rv$xy[indX,2] <- input$nudgeY
      gg_reactive()

    })
    #####################
    #calculate centrality/clustering if needed ----
    #####################
    shiny::observeEvent(input$calcIndex, {
      attr_name <- gsub("\\(rv.*","",input$centindex)
      if(!attr_name%in%igraph::vertex_attr_names(rv$g)){
        ind <- eval(parse(text=input$centindex))

        g <- igraph::set.vertex.attribute(graph = rv$g,name = attr_name,value = ind)
        rv$g <- g
        vattr.to.aes <- igraph::vertex_attr_names(rv$g)[!grepl("name",igraph::vertex_attr_names(rv$g))]
        idC <- which(sapply(vattr.to.aes,function(x) is.numeric(igraph::get.vertex.attribute(rv$g,x))))
        vattrC.to.aes <- c("None",vattr.to.aes[idC])
        updateSelectizeInput(session = session, inputId = 'nodeColAttr',
                             choices = vattrC.to.aes, selected = "None", server = TRUE,
                             options = list(create = TRUE))

        updateSelectizeInput(session = session, inputId = 'nodeSizeAttr',
                             choices = vattrC.to.aes, selected = "None", server = TRUE,
                             options = list(create = TRUE))

        updateSelectizeInput(session = session, inputId = 'nodeLabelAttr',
                             choices = c("None",igraph::vertex_attr_names(g)),
                             selected = "None", server = TRUE,
                             options = list(create = TRUE))
      }
    })

    shiny::observeEvent(input$calcClust, {
      attr_name <- gsub("\\(rv.*","",input$clusteralg)
      if(!attr_name%in%igraph::vertex_attr_names(rv$g)){
        ind <- eval(parse(text=input$clusteralg))
        ind <- as.character((igraph::membership(ind)))
        g <- igraph::set.vertex.attribute(graph = rv$g,name = attr_name,value = ind)
        rv$g <- g
        vattr.to.aes <- igraph::vertex_attr_names(rv$g)[!grepl("name",igraph::vertex_attr_names(rv$g))]
        idC <- which(sapply(vattr.to.aes,function(x) !is.numeric(igraph::get.vertex.attribute(rv$g,x))))
        vattrD.to.aes <- c("None",vattr.to.aes[idC])

        updateSelectizeInput(session = session, inputId = 'nodeColAttrD',
                             choices = vattrD.to.aes, selected = "None", server = TRUE,
                             options = list(create = TRUE))
        updateSelectizeInput(session = session, inputId = 'nodeLabelAttr',
                             choices = c("None",igraph::vertex_attr_names(g)),
                             selected = "None", server = TRUE,
                             options = list(create = TRUE))
      }
    })

    ###############################################################
    #####################
    #main plotting function ----
    #####################
    gg_reactive <- reactive({
      validate(
        need(is.validColour(input$nodeColMan), ''),
        need(is.validColour(input$edgeColMan), ''),
        need(is.validColour(input$nodeBorderColMan), ''),
        # need(is.validColour(input$nodeColAttr), ''),
        # need(is.validColour(input$nodeColAttrL), ''),
        # need(is.validColour(input$nodeColAttrH), ''),
        # need(is.validColour(input$edgeColAttr), ''),
        # need(is.validColour(input$edgeColAttrL), ''),
        # need(is.validColour(input$edgeColAttrH), ''),
        need(is.validColour(input$edgeColMan), '')
      )
      if(!input$showIso){
        rv$g <- rv$g_iso
        rv$xy <- rv$xy_iso
      } else{
        rv$g <- rv$g_all
        rv$xy <- rv$xy_all
      }
      #####################
      #layout ----
      #####################
      # xy <- get_layout()
      code_layout <- "ggraph(rv$g,layout = \"manual\",node.positions = data.frame(x = rv$xy[,1], y = rv$xy[,2]))"

      #####################
      #nodes ----
      #####################
      if(input$nodeColAttr=="None" & input$nodeColAttrD=="None" & input$nodeSizeAttr=="None"){
        code_nodes <- paste0("geom_node_point(",
                             "fill = \"",input$nodeColMan,"\"",
                             ",colour = \"",input$nodeBorderColMan,"\"",
                             ",size = ",input$nodeSizeMan,
                             ",stroke = ", input$nodeBorderSizeMan,
                             ",shape = 21",
                             ")")

      } else if(input$nodeColAttr!="None" & input$nodeSizeAttr=="None"){
        code_nodes <- paste0("geom_node_point(",
                             "aes(fill = ",input$nodeColAttr,")",
                             ",\ncolour = \"",input$nodeBorderColMan,"\"",
                             ",\nsize = ",input$nodeSizeMan,
                             ",\nshape = 21",
                             ", stroke = ", input$nodeBorderSizeMan,
                             ")")
        nodes_scale_col <- paste0("scale_fill_gradient(low = \"",input$nodeColAttrL,"\",",
                                  "high = \"",input$nodeColAttrH,"\")")
        code_nodes <- paste(code_nodes,nodes_scale_col,sep=" + ")

      } else if(input$nodeColAttrD!="None" & input$nodeSizeAttr=="None"){
        code_nodes <- paste0("geom_node_point(",
                             "aes(fill = ",input$nodeColAttrD,")",
                             ",\ncolour = \"",input$nodeBorderColMan,"\"",
                             ",\nsize = ",input$nodeSizeMan,
                             ",\nshape = 21",
                             ", stroke = ", input$nodeBorderSizeMan,
                             ")")
        nodes_scale_col <- paste0("scale_fill_brewer(palette = \"",input$nodeColAttrP,"\", na.value = \"gray53\")")
        code_nodes <- paste(code_nodes,nodes_scale_col,sep=" + ")

      } else if(input$nodeColAttr=="None" & input$nodeColAttrD=="None" & input$nodeSizeAttr!="None"){
        code_nodes <- paste0("geom_node_point(",
                             "aes(size = ",input$nodeSizeAttr,")",
                             ",\nfill = \"",input$nodeColMan,"\"",
                             ",\ncolour = \"",input$nodeBorderColMan,"\"",
                             ",\nshape = 21",
                             ", stroke = ", input$nodeBorderSizeMan,
                             ")")
        nodes_scale_size <- paste0("scale_size(range = c(",input$nodeSizeAttrL,",",input$nodeSizeAttrH,"))")
        code_nodes <- paste(code_nodes,nodes_scale_size,sep=" + ")

      } else if(input$nodeColAttr!="None" & input$nodeSizeAttr!="None"){
        code_nodes <- paste0("geom_node_point(",
                             "aes(fill = ",input$nodeColAttr,
                             ",size = ",input$nodeSizeAttr,")",
                             ",\ncolour = \"",input$nodeBorderColMan,"\"",
                             ",\nshape = 21",
                             ", stroke = ", input$nodeBorderSizeMan,
                             ")")

        nodes_scale_col <- paste0("scale_fill_gradient(low = \"",input$nodeColAttrL,"\",",
                                  "high = \"",input$nodeColAttrH,"\")")
        nodes_scale_size <- paste0("scale_size(range = c(",input$nodeSizeAttrL,",",input$nodeSizeAttrH,"))")
        code_nodes <- paste(code_nodes,nodes_scale_col,nodes_scale_size,sep=" + ")
      } else if(input$nodeColAttrD!="None" & input$nodeSizeAttr!="None"){
        code_nodes <- paste0("geom_node_point(",
                             "aes(fill = ",input$nodeColAttrD,
                             ", size = ",input$nodeSizeAttr,")",
                             ",\ncolour = \"",input$nodeBorderColMan,"\"",
                             ",\nshape = 21",
                             ", stroke = ", input$nodeBorderSizeMan,
                             ")")

        nodes_scale_col <- paste0("scale_fill_brewer(palette = \"",input$nodeColAttrP,"\", na.value = \"gray53\")")
        nodes_scale_size <- paste0("scale_size(range = c(",input$nodeSizeAttrL,",",input$nodeSizeAttrH,"))")
        code_nodes <- paste(code_nodes,nodes_scale_col,nodes_scale_size,sep=" + ")
      }
      #####################
      #nodes labels ----
      #####################
      if(input$nodeLabelAttr!="None" & input$nodeLabelAttr!=""){
        code_labels <- paste0("geom_node_text(",
                              "aes(label = ",input$nodeLabelAttr,")",
                              ", colour = \"",input$nodeLabelCol,"\"",
                              ", size = ",input$nodeLabelSize,
                              ", family = \"",input$nodeLabelFont,"\"",
                              ")")
        if(input$nodeLabelRepel){
          insert <- paste0(", repel = ",input$nodeLabelRepel, ",segment.alpha=0)")
          code_labels <- gsub("\\)$",insert,code_labels)
        }
        code_nodes <- paste(code_nodes,code_labels,sep=" + ")
      }
      #####################
      #edges ----
      #####################
      if(input$edgeColAttr=="None" & input$edgeSizeAttr=="None" & input$edgeAlphaAttr=="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "edge_colour = \"",input$edgeColMan,"\"",
                             ",edge_width = ",input$edgeSizeMan,
                             ",edge_alpha = ",input$edgeAlphaMan,
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }

      } else if(input$edgeColAttr!="None" & input$edgeSizeAttr=="None" & input$edgeAlphaAttr=="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "aes(colour = ",input$edgeColAttr,")",
                             ",edge_width = ",input$edgeSizeMan,
                             ",edge_alpha = ",input$edgeAlphaMan,
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }
        edge_scale_col <- paste0("scale_edge_colour_gradient(low = \"",input$edgeColAttrL,"\",",
                                 "high = \"",input$edgeColAttrH,"\")")
        code_edges <- paste(code_edges,edge_scale_col,sep=" + ")

      } else if(input$edgeColAttr=="None" & input$edgeSizeAttr!="None" & input$edgeAlphaAttr=="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "aes(width = ",input$edgeSizeAttr,")",
                             ",\nedge_colour = \"",input$edgeColMan,"\"",
                             ",edge_alpha = ",input$edgeAlphaMan,
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }
        edge_scale_size <- paste0("scale_edge_width(",
                                  "range = c(",input$edgeSizeAttrL,",",input$edgeSizeAttrH,"))")
        code_edges <- paste(code_edges,edge_scale_size,sep=" + ")

      } else if(input$edgeColAttr=="None" & input$edgeSizeAttr=="None" & input$edgeAlphaAttr!="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "aes(alpha = ",input$edgeAlphaAttr,")",
                             ",\nedge_colour = \"",input$edgeColMan,"\"",
                             ",\nedge_width = ",input$edgeSizeMan,
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }
        edge_scale_alpha <- paste0("scale_edge_alpha(",
                                   "range = c(",input$edgeAlphaAttrL,",",input$edgeAlphaAttrH,"))")
        code_edges <- paste(code_edges,edge_scale_alpha,sep=" + ")

      } else if(input$edgeColAttr!="None" & input$edgeSizeAttr!="None" & input$edgeAlphaAttr=="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "aes(width = ",input$edgeSizeAttr,
                             ",\ncolour = ",input$edgeColAttr,")",
                             ",edge_alpha = ",input$edgeAlphaMan,
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }
        edge_scale_size <- paste0("scale_edge_width(",
                                  "range = c(",input$edgeSizeAttrL,",",input$edgeSizeAttrH,"))")
        edge_scale_col <- paste0("scale_edge_colour_gradient(low = \"",input$edgeColAttrL,"\",",
                                 "high = \"",input$edgeColAttrH,"\")")

        code_edges <- paste(code_edges,edge_scale_col,edge_scale_size,sep=" + ")

      } else if(input$edgeColAttr!="None" & input$edgeSizeAttr=="None" & input$edgeAlphaAttr!="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "aes(alpha = ",input$edgeAlphaAttr,
                             ",colour = ",input$edgeColAttr,")",
                             ",\nedge_width = ",input$edgeSizeMan,
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }
        edge_scale_alpha <- paste0("scale_edge_alpha(",
                                   "range = c(",input$edgeAlphaAttrL,",",input$edgeAlphaAttrH,"))")
        edge_scale_col <- paste0("scale_edge_colour_gradient(low = \"",input$edgeColAttrL,"\",",
                                 "high = \"",input$edgeColAttrH,"\")")

        code_edges <- paste(code_edges,edge_scale_col,edge_scale_alpha,sep=" + ")

      } else if(input$edgeColAttr=="None" & input$edgeSizeAttr!="None" & input$edgeAlphaAttr!="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "aes(alpha = ",input$edgeAlphaAttr,
                             ",width = ",input$edgeSizeAttr,")",
                             ",\nedge_colour = \"", input$edgeColMan,"\"",
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }
        edge_scale_alpha <- paste0("scale_edge_alpha(",
                                   "range = c(",input$edgeAlphaAttrL,",",input$edgeAlphaAttrH,"))")
        edge_scale_size <- paste0("scale_edge_width(",
                                  "range = c(",input$edgeSizeAttrL,",",input$edgeSizeAttrH,"))")

        code_edges <- paste(code_edges,edge_scale_size,edge_scale_alpha,sep=" + ")

      } else if(input$edgeColAttr!="None" & input$edgeSizeAttr!="None" & input$edgeAlphaAttr!="None"){
        code_edges <- paste0("geom_edge_fan(",
                             "aes(alpha = ",input$edgeAlphaAttr,
                             ",width = ",input$edgeSizeAttr,
                             ",\ncolour = ",input$edgeColAttr,")",
                             ",n = ",input$edgeNMan,")")
        if(is.directed(g)){
          arrow_code <- paste0(",\narrow = arrow(angle = 30, length = unit(0.15, \"inches\")",
                               ",\nends = \"last\", type = \"closed\")",
                               ",\nend_cap = circle(",input$nodeSizeMan+2,", \"pt\"))")
          code_edges <- gsub(")$",arrow_code,code_edges)
        }
        edge_scale_alpha <- paste0("scale_edge_alpha(",
                                   "range = c(",input$edgeAlphaAttrL,",",input$edgeAlphaAttrH,"))")
        edge_scale_size <- paste0("scale_edge_width(",
                                  "range = c(",input$edgeSizeAttrL,",",input$edgeSizeAttrH,"))")

        edge_scale_col <- paste0("scale_edge_colour_gradient(low = \"",input$edgeColAttrL,"\",",
                                 "high = \"",input$edgeColAttrH,"\")")

        code_edges <- paste(code_edges,edge_scale_col,edge_scale_size,edge_scale_alpha,sep=" + ")
      }

      #################
      #theme ----
      #################
      code_theme <- paste0("theme_graph() + theme(legend.position = \"",input$legendPos,"\")")

#################
      #glue ----
################
      code <- paste(code_layout,code_edges,code_nodes,code_theme,sep=" + ")
      if(input$showLabs){
        code <- paste0(code,"+ geom_node_text(label = 1:vcount(rv$g),colour=\"white\")")
      }
      # p <- eval(parse(text = code))
      p <- code

      return(p)

    })
##################################
    DT_reactiveN <- reactive({
      create_attribute_df(rv$g,which = "nodes")
    })
    DT_reactiveE <- reactive({
      create_attribute_df(rv$g,which = "edges")
    })
##################################
    #render plot
    ggnet <- renderPlot( {
      eval(parse(text = gg_reactive()))
    })
    #render Attribute Manager
    dfattrN <- renderDataTable({
      DT_reactiveN()
    },options = list(
      lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),
      pageLength = 10,
      searching = FALSE))

    dfattrE <- renderDataTable({
      DT_reactiveE()
    },options = list(
      lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),
      pageLength = 10,
      searching = FALSE))

    output$Graph1 <- ggnet
    output$Graph2 <- ggnet
    output$Graph3 <- ggnet
    output$Graph4 <- ggnet
    output$attrManageN  <- dfattrN
    output$attrManageE  <- dfattrE

    #  DONE -----
    observeEvent(input$done, {
      result <- gg_reactive()
      result <- gsub("\\+","\\+ \n\t",result)
      result <- gsub("ggraph\\(rv\\$g,",paste0("ggraph\\(",text,","),result)
      V(rv$g)$x <- rv$xy[,1]
      V(rv$g)$y <- rv$xy[,2]
      result <- gsub("rv\\$xy\\[,1\\]",paste0("V(",text,")$x"),result)
      result <- gsub("rv\\$xy\\[,2\\]",paste0("V(",text,")$y"),result)
      # res_lay <- paste0("xy <- ",input$graph.layout,"(",text,")\n")
      # result <- paste0(res_lay,result)
      # eval(parse(text = paste0(text,"<<- rv$g")))
      eval(parse(text = paste0("assign(\"",text,"\",rv$g",",envir = .GlobalEnv)")))
      rstudioapi::insertText(result)
      invisible(stopApp())
    })

    observeEvent(input$cancel, {
      invisible(stopApp())
    })

  }

  viewer <- dialogViewer(dialogName = 'SNAhelper', width = 990, height = 900)
  # viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, stopOnCancel = FALSE, viewer = viewer)
}


#' @export
#' @rdname SNAhelper

SNAhelperAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()

  text <- context$selection[[1]]$text
  # text <- "gr"
  if (nchar(text) == 0) {
    stop('Please highlight an igraph object before using this addin.')
  }

  SNAhelper(text)
}
