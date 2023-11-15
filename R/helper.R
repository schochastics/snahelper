headingOutput <- function(heading, height = "20px", css = "color: #FF7F00; text-decoration: underline;") {
    fillCol(tags$div(style = css, strong(heading)), height = height)
}

is.validColour <- function(x) {
    if (is.null(x)) {
        return(TRUE)
    } else if (x %in% c(colours.available, "NA", "NULL")) {
        return(TRUE)
    } else if (grepl("#[0-9a-fA-F]{6}$", x)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

create_attribute_df <- function(g, which = "nodes") {
    if (which == "nodes") {
        vattrs <- igraph::vertex_attr_names(g)
        df_attrs <- data.frame(id = seq_len(igraph::vcount(g)))
        if (length(vattrs) > 0) {
            for (i in seq_along(vattrs)) {
                df_attrs[[vattrs[i]]] <- igraph::get.vertex.attribute(g, vattrs[i])
            }
        }
    } else if (which == "edges") {
        eattrs <- igraph::edge_attr_names(g)
        df_attrs <- data.frame(id = seq_len(igraph::ecount(g)))
        if (length(eattrs) > 0) {
            for (i in seq_along(eattrs)) {
                df_attrs[[eattrs[i]]] <- igraph::get.edge.attribute(g, eattrs[i])
            }
        }
    }
    df_attrs
}

# JS code ----
jscodeHeight <-
    '$(document).on("shiny:connected", function(e) {
    var jsHeight = document.documentElement.clientHeight;
    Shiny.onInputChange("ViewerHeight",jsHeight);
  });'

jscodeWidth <-
    '$(document).on("shiny:connected", function(e) {
    var jsWidth = document.documentElement.clientWidth;
    Shiny.onInputChange("ViewerWidth",jsWidth);
  });'

jsColourSelector <- I(
    '{
  option: function(item, escape) {
  return "<div><div style=\'width:25px; height:15px; background-color:" + item.rgb + "; float:left; vertical-align:bottom\'></div>&nbsp;" + escape(item.name) + "</div>";
  }
  }'
)


closest_node <- function(p, xy) {
    if (nrow(xy) == 0) {
        return(-1)
    }
    dists <- sqrt((p[1] - xy[, 1])^2 + (p[2] - xy[, 2])^2)
    mdist <- min(dists)
    if (mdist < 0.5) {
        return(which.min(dists))
    } else {
        return(-1)
    }
}
