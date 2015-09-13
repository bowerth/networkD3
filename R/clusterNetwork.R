#' Create cluster network diagrams (dynamic hierarchical edge bundling).
#'
#' @param List a hierarchical list object with a root node and children.
#' @param height height for the network graph's frame area in pixels (if
#'   \code{NULL} then height is automatically determined based on context)
#' @param width numeric width for the network graph's frame area in pixels (if
#'   \code{NULL} then width is automatically determined based on context)
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param linkColour character string specifying the colour you want the link
#' lines to be. Multiple formats supported (e.g. hexadecimal).
#' @param nodeColour character string specifying the colour you want the node
#' circles to be. Multiple formats supported (e.g. hexadecimal).
#' @param nodeStroke character string specifying the colour you want the node
#' perimeter to be. Multiple formats supported (e.g. hexadecimal).
#' @param textColour character string specifying the colour you want the text to
#' be before they are clicked. Multiple formats supported (e.g. hexadecimal).
#' @param opacity numeric value of the proportion opaque you would like the
#' graph elements to be.
#' @param margin integer value of the plot margin. Set the margin
#' appropriately to accomodate long text labels.
#'
#'
#' @examples
#' ## dontrun
#' ## Create cluster from JSON formatted data
#' ## Recreate Bostock example from http://bl.ocks.org/mbostock/7607999
#' Flare <- rjson::fromJSON(file = system.file("data/readme-flare-imports.json", package = "networkD3"), simplify = FALSE)
#' clusterNetwork(List = Flare, fontSize = 10, opacity = 0.9)
#'
#' Mike Bostock: \url{http://bl.ocks.org/mbostock/7607999}.
#'
#' @importFrom rjson toJSON
#' @export
#'
clusterNetwork <- function(
  List,
  height = NULL,
  width = NULL,
  fontSize = 10,
  linkColour = "#ccc",
  nodeColour = "#fff",
  nodeStroke = "steelblue",
  textColour = "#111",
  opacity = 0.9,
  margin = 0)
{
    # validate input
    if (!is.list(List))
      stop("List must be a list object.")
    ## root <- toJSON(List)
    classes <- toJSON(List)

    # create options
    options = list(
        height = height,
        width = width,
        fontSize = fontSize,
        linkColour = linkColour,
        nodeColour = nodeColour,
        nodeStroke = nodeStroke,
        textColour = textColour,
        margin = margin,
        opacity = opacity
    )

    # create widget
    htmlwidgets::createWidget(
        name = "clusterNetwork",
        x = list(classes = classes, options = options),
        width = width,
        height = height,
        htmlwidgets::sizingPolicy(viewer.suppress = TRUE,
                                  browser.fill = TRUE,
                                  ## browser.padding = 75,
                                  browser.padding = 0,
                                  knitr.figure = FALSE,
                                  knitr.defaultWidth = 800,
                                  knitr.defaultHeight = 500),
        package = "networkD3")

}

#' @rdname networkD3-shiny
#' @export
clusterNetworkOutput <- function(outputId, width = "100%", height = "800px") {
    shinyWidgetOutput(outputId, "clusterNetwork", width, height,
                        package = "networkD3")
}

#' @rdname networkD3-shiny
#' @export
renderClusterNetwork <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    shinyRenderWidget(expr, clusterNetworkOutput, env, quoted = TRUE)
}

#' Convert an R hclust or dendrogram object into a clusterNetwork list.
#'
#' \code{as.clusterNetwork} converts an R hclust or dendrogram object into a list suitable
#' for use by the \code{clusterNetwork} function.
#'
#' @param d An object of R class \code{hclust} or \code{dendrogram}.
#' @param classes An optional name for the classes node. If missing, use the first argument
#' variable name.
#'
#' @details \code{as.clusterNetwork} coverts R objects of class \code{hclust} or
#' \code{dendrogram} into a list suitable for use with the \code{clusterNetwork} function.
#'
#' @examples
#' # Create a hierarchical cluster object and display with clusterNetwork
#' ## dontrun
#' hc <- hclust(dist(USArrests), "ave")
#' clusterNetwork(as.clusterNetwork(hc))
#'
#' @export

as.clusterNetwork <- function(d, classes)
{
  if(missing(classes)) classes <- as.character(match.call()[[2]])
  if("hclust" %in% class(d)) d <- as.dendrogram(d)
  if(!("dendrogram" %in% class(d)))
    stop("d must be a object of class hclust or dendrogram")
  ul <- function(x, level=1)
  {
    if(is.list(x))
    {
      return(lapply(x, function(y)
      {
        name <- ""
        if(!is.list(y)) name <- attr(y,"label")
        list(name=name, children=ul(y,level+1))
      }))
    }
    list(name=attr(x,"label"))
  }
  list(name=classes,children=ul(d))
}


