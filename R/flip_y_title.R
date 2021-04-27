#' Orients y-axis title horizontally
#'
#' Rotates and repositions the y-axis title where short titles allow for them to
#' be positioned horizontally at the top of the y-axis (without causing
#' excessive whitespace on left of plot).
#'
#' This function just sets the angle and justification of the y-axis title,
#' orienting text horizontally above the y-axis as per GSS best practise.
#' Depending on the placement of tick labels, you may need to expand the y-axis
#' to accomodate tick labels and title, e.g. also run
#' ggplot2::scale_y_continuous(expand = expansion(mult = c(0,.1)))
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Species, y = Sepal.Length)) +
#'     geom_boxplot(fill = wwfc_cols("light gold 20")) +
#'     wolves_theme(media = "web") +
#'     labs(title = "Virginica sepals are longest",
#'          subtitle = "Sepal length by species",
#'          y = "/mm") +
#'     scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
#'     flip_y_title()
#'
flip_y_title <- function() {
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(angle = 0,
                                         vjust = 1.01,
                                         hjust = 1,
                                         margin = ggplot2::margin(r = 0))
    )
}
