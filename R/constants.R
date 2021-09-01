input.width <- '50%'
input.width2 <- '90%'
line.height <- '70px'
heading.height <- '30px'

layouts.available <- list("graphlayouts" = c("Stress"="graphlayouts::layout_with_stress",
                                      "Backbone"="graphlayouts::layout_as_backbone",
                                      "Radial focus" = "graphlayouts::layout_with_focus",
                                      "Radial centrality" = "graphlayouts::layout_with_centrality"),
                          "igraph" =   c("Circle"="layout_in_circle","Nicely"= "layout_nicely",
                                         "Random"="layout_randomly",
                                         "Fruchterman-Reingold"="layout_with_fr",
                                         "MDS"="layout_with_mds",
                                         "Sugiyama"="layout_with_sugiyama",
                                         "drl"="layout_with_drl",
                                         "gem"="layout_with_gem",
                                         "graphopt"="layout_with_graphopt",
                                         "kk"="layout_with_kk",
                                         "lgl"="layout_with_lgl"))

colours.available <- c(colors()[!grepl('grey', colors())])
fonts.available <- names(pdfFonts())

AttrNameImport <- paste0(
  "vnames <- get.vertex.attribute(g,'name')\n",
  "identCol <- which(apply(attrs,2,function(x) all(x%in%vnames)))[1]\n",
  "anames <- attrs[,identCol]\n",
  "attrs <- attrs[,-identCol,drop = FALSE]\n",
  "perm <- match(vnames,anames)\n",
  "for(attr in names(attrs)){\n",
    "   g <- set_vertex_attr(g,name = attr,value = attr[[attr]][perm])\n",
  "}\n"
)

AttrRowImport <- paste0(
  "for(attr in names(attrs)){\n",
  "   g <- set_vertex_attr(g,name = attr,value = attrs[[attr]])\n",
  "}\n"
)
