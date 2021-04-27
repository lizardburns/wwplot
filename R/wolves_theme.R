#' Theme plots in Wolverhampton Wanderers style
#'
#' ggplot2 minimal theme which optimises font size (for web or print) and
#' the use/orientation of gridlines (major gridlines only).
#'
#' Uses the ggplot2 \code{theme} to set consistent plot theme elements.
#' Overwrites any preceding theme calls and can be overwritten by subsequent
#' ones.
#'
#' @param media A string, either "print" or "web". Should the font size
#'   be optimised for print or web outputs? Default: web.
#' @param grid A string, either "hgrid", "vgrid" or "fullgrid". Allows for
#'   setting optimal orientation of gridlines and easy adjustment after using
#'   \code{coord_flip()}. See examples. Default: hgrid.
#' @param panel_border logical, should a panel border be drawn? Intended for use
#'   with faceted plots where a panel border may be needed if there are many
#'   panels.
#' @param base_size integer. Base font size, given in pts. Default: 11.
#'
#' @export
#'
#' @examples
#' suppressMessages(library(dplyr))
#' library(ggplot2)
#'
#' # wolves theme with font size optimised for web outputs
#' ggplot(iris, aes(x = Species, y = Sepal.Length)) +
#'   geom_boxplot(fill = wwfc_cols("green")) +
#'   wolves_theme(media = "web") +
#'   labs(title = "Virginica sepals are longest",
#'        subtitle = "Sepal length (mm) by species",
#'        y = "")
#'
#' # wolves theme with font size optimised for web outputs
#' ggplot(iris, aes(x = Species, y = Sepal.Length)) +
#'   geom_boxplot(fill = wwfc_cols("green")) +
#'   wolves_theme(media = "print") +
#'   labs(title = "Virginica sepals are longest",
#'        subtitle = "Sepal length (mm) by species",
#'        y = "")
#'
#' # density plot in Wolves style
#' ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
#'   geom_density(alpha = .7) +
#'   wolves_theme() +
#'   scale_y_continuous(expand = expansion(mult = c(0, .05))) +
#'   labs(
#'     title = "Setosa sepals may be reliably distinguished by length",
#'     subtitle = "Distribution of sepal length (mm) by species",
#'     y = "",
#'     fill = ""
#'     ) +
#'   theme(plot.title.position = "plot") +
#'   scale_fill_wolves()
#'
#' # horizontal gridlines
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(mean = mean(Sepal.Length), .groups = "drop") %>%
#'   ggplot(aes(x = Species, y = mean)) +
#'   geom_col(fill = wwfc_cols("green")) +
#'   wolves_theme() +
#'   scale_y_continuous(
#'     limits = c(0,7),
#'     breaks = c(0:8),
#'     expand = expansion(mult = c(0, .05))
#'     ) +
#'   labs(title = "Virginica sepals are longest",
#'        subtitle = "Sepal length (mm) by species",
#'        y = "")
#'
#' # vertical gridlines
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(mean = mean(Sepal.Length), .groups = "drop") %>%
#'   ggplot(aes(x = Species, y = mean)) +
#'   geom_col(fill = wwfc_cols("green")) +
#'   coord_flip() +
#'   wolves_theme(grid = "vgrid") +
#'   scale_y_continuous(
#'     limits = c(0,7),
#'     breaks = c(0:8),
#'     expand = expansion(mult = c(0, .05))
#'     ) +
#'   labs(title = "Virginica sepals are longest",
#'        subtitle = "Sepal length (mm) by species",
#'        y = "")
#'
#' # full grid with horizontal and vertical gridlines
#' iris %>%
#'   ggplot(aes(x = Sepal.Width, y = Sepal.Length, colour = Species)) +
#'   geom_point(size = 3) +
#'   wolves_theme(grid = "fullgrid") +
#'   scale_colour_wolves() +
#'   scale_x_continuous(expand = expansion(mult = c(0.05, .05))) +
#'   labs(title = "Different strategies to achieve sepal area",
#'        subtitle = "Sepal dimensions (mm) by species",
#'        y = "Length",
#'        x = "Width",
#'        colour = "") +
#'   theme(legend.position = "right")
#'
wolves_theme <- function(
  media = "web",
  grid = "hgrid",
  panel_border = FALSE,
  base_size = 11
  ) {
  stopifnot(
    'media must be one of "web" or "print"!' =
      media %in% c("web", "print")
    )
  stopifnot(
    'grid must be one of "hgrid", "vgrid" or "fullgrid"!' =
      grid %in% c("hgrid", "vgrid", "fullgrid")
    )

  # colours ----
  font <- "sans"
  col_bg <- "#2a2a2b"
  col_txt <- "#A0A0A3"
  col_title <- "#FFFFFF"
  col_subtitle <- "#d9d9d9"

  col_axes <- "#016881"#"#505261"
  col_grid <- "#016881"#"#D8D8D8"
  col_strip <- "#bebfcc"

  # font size ----
  font_size <- list()
  if(media == "web") {
    font_size$title     <- 18 * base_size / 11
    font_size$subtitle  <- 16 * base_size / 11
    font_size$axistitle <- 13 * base_size / 11
    font_size$axistext  <- 13 * base_size / 11
    font_size$notes     <- 11 * base_size / 11
  } else if(media == "print") {
    font_size$title     <- 14 * base_size / 11
    font_size$subtitle  <- 13 * base_size / 11
    font_size$axistitle <- 12 * base_size / 11
    font_size$axistext  <- 12 * base_size / 11
    font_size$notes     <- 10 * base_size / 11
  }

  # grid size ----
  gridline_size <- .2

  # theme ----
  wolves <- ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = font,
      size = font_size$title,
      face = "bold",
      color = col_title
    ),
    plot.title.position = "plot",

    plot.subtitle = ggplot2::element_text(
      family = font,
      size = font_size$subtitle,
      margin = ggplot2::margin(0, 0, 9, 0),
      color = col_subtitle
    ),

    # plot.caption = ggplot2::element_text(size = font_size$notes, color = col_subtitle, hjust = 0),
    plot.caption = ggplot2::element_blank(),
    plot.caption.position =  "plot",

    text = ggplot2::element_text(colour = col_txt),
    title = ggplot2::element_text(colour = col_title),

    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(size = font_size$axistitle,
                                         color = col_txt),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font,
                                        size = font_size$axistext,
                                        color = col_subtitle),

    axis.title = ggplot2::element_text(size = font_size$axistitle,
                                       color = col_txt,
                                       hjust = 1),
    axis.title.y = ggplot2::element_text(size = font_size$axistitle,
                                         color = col_txt, hjust = .5,
                                         margin = ggplot2::margin(r = 15)),
    axis.title.x = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family = font,
                                      size = font_size$axistext,
                                      color = col_subtitle),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10),),
    axis.ticks.x = ggplot2::element_line(size = .5, colour = col_axes),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.length = grid::unit(.13, "cm"),
    axis.line.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(colour = col_axes,
                                        size = 1),

    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = col_grid,
                                               size = gridline_size),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(1, "lines"),

    plot.background = ggplot2::element_rect(colour = col_bg, fill = col_bg),
    rect = ggplot2::element_rect(fill = col_bg, colour = col_bg),

    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = font_size$axistitle)
  )

  # vgrid ----
  if(grid == "vgrid") {
    wolves <- (wolves +
                 ggplot2::theme(
                   axis.line.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_line(colour = col_axes,
                                                       size = 1),
                   panel.grid.major.x = ggplot2::element_line(colour = col_grid,
                                                              size = gridline_size),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_line(size = .5, colour = col_axes),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text()
                 ))
  }

  # full grid ----
  if(grid == "fullgrid") {
    wolves <- (wolves +
                 ggplot2::theme(
                   axis.line.y = ggplot2::element_line(colour = col_axes, size = 1),
                   panel.grid.major.x = ggplot2::element_line(colour = col_grid,
                                                              size = gridline_size),
                   axis.ticks.y = ggplot2::element_line(),
                   axis.title.x = ggplot2::element_text()
                 ))
  }

  # panel border ----
  if (panel_border) {
    wolves <- (wolves +
                 cowplot::panel_border() +
                 ggplot2::theme(
                   strip.background = ggplot2::element_rect(fill = col_strip))
               )
  }

  return(wolves)
}
