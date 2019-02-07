input.width <- '50%'
input.width2 <- '90%'
line.height <- '70px'
heading.height <- '30px'

layouts.available <- list("smglr" = c("Stress"="smglr::layout_with_stress",
                                      "Backbone"="smglr::layout_as_backbone"),
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
