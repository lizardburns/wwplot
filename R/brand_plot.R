#' Add logo and source information in footer
#'
#' Uses \code{plot_grid()} from the {cowplot} package to brand plots with the
#' logo (right-justified) and has an option to add information about the
#' data source (left-justified) in the same plot footer element.
#'
#' @param plot A ggplot object
#' @param add_source logical. If TRUE, then you must supply text using the
#'   source_text arg (throws error otherwise), which is pasted after "Source:"
#'   in the output plot.
#' @param source_text Text to be combined with "Source:" and printed in plot
#'   footer.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
#'     geom_boxplot(fill = wwfc_cols("light gold 20")) +
#'     wolves_theme() +
#'     theme(axis.title.y = element_blank()) +
#'     labs(title = "Virginica sepals are the longest",
#'          subtitle = "Sepal length (mm) by species")
#'
#' # add logo to plot
#' brand_plot(p)
#'
#' # add logo and data source information to plot
#' brand_plot(p, add_source = TRUE, source_text = "Iris dataset")
brand_plot <- function(plot, add_source = FALSE, source_text = "Made with {wwplot}") {
  logo <- system.file("logos", "logo.png", package = "wwplot") %>%
    magick::image_read()

  x <- ggplot2::ggplot() +
    wolves_theme() +
    ggplot2::theme(axis.line.x = ggplot2::element_blank())

  q <- cowplot::ggdraw() +
    cowplot::draw_plot(x) +
    cowplot::draw_image(logo,
                        x = 1, y = 1,
                        hjust = 1, vjust = 1,
                        halign = 1, valign = 1)+
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#2a2a2b",
                                              color = "#2a2a2b")
      )

  if(add_source) {
    bottom_row <- cowplot::ggdraw(q) +
      cowplot::draw_label(
        source_text,
        colour = "#d9d9d9",
        x = 0.02, y = .6,
        hjust = 0, vjust = 1,
        size = 10
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#2a2a2b",
                                                color = "#2a2a2b")
        )
  } else {
    bottom_row <- q
  }

  cowplot::plot_grid(plot,
                     bottom_row,
                     nrow = 2, ncol = 1,
                     rel_heights = c(9, 1))
}
