#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
bubbletree <-
  function(tree_data,
           width = NULL,
           height = NULL,
           elementId = NULL,
           settings = list(smoothness = 30,
                           padding = 10,
                           resizing_factor = 20,
                           max_textsize = .18,
                           min_textsize_scale = 2)) {

    settings <- modifyList(eval(formals()$settings), settings)

    # create widget
    htmlwidgets::createWidget(
      name = 'bubbletree',
      x = list(data = tree_data, settings = settings),
      width = width,
      height = height,
      sizingPolicy = htmlwidgets::sizingPolicy(
        browser.fill = TRUE,
        viewer.fill = TRUE,
        viewer.padding = 0
      )

    )
  }

#' Shiny bindings for bubbletree
#'
#' Output and render functions for using bubbletree within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a bubbletree
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name bubbletree-shiny
#'
#' @export
bubbletreeOutput <-
  function(outputId,
           width = '100%',
           height = '400px') {
    htmlwidgets::shinyWidgetOutput(outputId, 'bubbletree', width, height, package = 'bubbletree')
  }

#' @rdname bubbletree-shiny
#' @export
renderBubbletree <-
  function(expr,
           env = parent.frame(),
           quoted = FALSE) {
    if (!quoted) {
      expr <- substitute(expr)
    } # force quoted
    htmlwidgets::shinyRenderWidget(expr, bubbletreeOutput, env, quoted = TRUE)
  }


# bubbletree_html <- function(id, style, class, ...){
#   htmltools::tags$svg(id = "svgCircles", width="99%", height = "99%")
# }
