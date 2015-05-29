#' Create Chord network diagram.
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
#' ## Create chord from JSON formatted data
#'
#' table <- read.csv(system.file(file.path("data", "cities.csv"), package = "networkD3"))
#' matrix <- JSONtoMatrix(file = system.file(file.path("data", "matrix.json"), package = "networkD3"))
#'
#' # Visualize the chord
#' chordNetwork(matrix = matrix, digits = 18, df = table, fontSize = 10)
#'
#' @source http://blog.uber.com/2012/01/09/uberdata-san-franciscomics/
#'
#' Mike Bostock: \url{http://bost.ocks.org/mike/uberdata/}.
#'
#' @importFrom RJSONIO toJSON
#' @export

chordNetwork <- function(
    matrix,
    digits=18,
    df,
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
    if (!is.matrix(matrix))
      stop("'matrix' must be a matrix object.")
    matrix <- RJSONIO::toJSON(matrix, digits = digits)

    if (!is.data.frame(df))
      stop("'df' must be a dataframe object.")
    tempfile <- tempfile()
    write.csv(df, file = tempfile, row.names = FALSE, quote = FALSE)
    tableLines <- readLines(tempfile)
    table <- toString(paste0(tableLines, collapse = '\n'))

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
        name = "chordNetwork",
        x = list(matrix = matrix, table = table, options = options),
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
chordNetworkOutput <- function(outputId, width = "100%", height = "800px") {
    shinyWidgetOutput(outputId, "chordNetwork", width, height,
                        package = "networkD3")
}

#' @rdname networkD3-shiny
#' @export
renderChordNetwork <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    shinyRenderWidget(expr, chordNetworkOutput, env, quoted = TRUE)
}

#' Convert an R hclust or dendrogram object into a chordNetwork list.
#'
#' \code{as.chordNetwork} converts an R hclust or dendrogram object into a list suitable
#' for use by the \code{chordNetwork} function.
#'
#' @param d An object of R class \code{hclust} or \code{dendrogram}.
#' @param classes An optional name for the classes node. If missing, use the first argument
#' variable name.
#'
#' @details \code{as.chordNetwork} coverts R objects of class \code{hclust} or
#' \code{dendrogram} into a list suitable for use with the \code{chordNetwork} function.
#'
#' @examples
#' # Create a hierarchical chord object and display with chordNetwork
#' ## dontrun
#' hc <- hclust(dist(USArrests), "ave")
#' chordNetwork(as.chordNetwork(hc))
#'
#' @export

as.chordNetwork <- function(d, classes)
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


