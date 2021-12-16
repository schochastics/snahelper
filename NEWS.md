# snahelper 1.4.0

* `Snahelper` is now more reproducible by also returning the coordinates explicitly (if needed)
* fixed deprecated FA 5 sliders name (changed to sliders-h)

# snahelper 1.3.0

* added layout via stored node variables x and y 
* added `drop = FALSE` to `Netreader` for cases where only one attribute is read
* minor bug fixes

# snahelper 1.2.0

* added `componentlayouter`

# snahelper 1.1.0

* Fixed a bug that prevented the calculation of in/out degree in directed networks  
* Fixed a bug that produced warnings for directed networks
* Fixed label of variable name field for `netbuilder` and `netreader`
* Switched plot and control panel layout to remove scrolling issue. 
* Use {{DT}} for table rendering due to persistent issue with character attributes.

# snahelper 1.0.0

* **BREAKING** only works with the newest ggraph version (2.0.0) (error message added)
* added `Netbuilder` addin
* added `Netreader` addin
* edge geom is chosen automatically between `geom_edge_link0()` and `geom_edge_parallel0()`
* discrete edge colors are supported now
* added support for `layout_with_focus()` and `layout_with_centrality()`

# snahelper 0.3.0

* tweaking node position is now possible via clicking on desired location
* added `formatR` for nicer code output
* added backbone layout support
* centrality indices in node attribute manager automatically adapt to type of graph (directed/weighted/etc.)

# snahelper 0.2.0

* added option to repel node labels
* removed curvature for edges and added manual alpha tuning
* switched from `geom_edge_arc` to `geom_edge_fan`
* changed some default colors
* increased max node size
* added `colourpicker` support

# snahelper 0.1.0

* initial version
