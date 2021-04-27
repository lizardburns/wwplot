
# corporate colours (from the brand guide 2021)
wwfc_colours <- c(
  `old gold` = "#ad6217",
  gold = "#9e6100",
  `steel grey` = "#6e848e",
  `bright gold 20` = "#d4681d",
  `light gold 20` = "#f09a3c",
  `portugal red 20` = "#6b0f19",
  `portugal green 20` = "#213c35",
  `mexico green 19` = "#02a089",
  `blue skies 20` = "#2eadce",
  `ikeme hot pink` = "#d33a78",
  `dicko lime` = "#c0f29d",
  `goodyear blue` = "#003853",
  `bully tourq` = "#016881"
)

#' Function to access wwfc colours
#'
#' Allows you to access wwfc brand colours in a robust and flexible way.
#' Supply colour names and the function ouputs hex codes. An empty function call
#' returns a named vector of all the colours used in available wwfc palettes.
#'
#' @param ... Character names of wwfc_colours
#'
#' @export
#'
#' @examples
#' wwfc_cols()
#'
#' wwfc_cols("bright gold 20")
#'
#' wwfc_cols("bully tourq", "goodyear blue")
#'
#' # manually setting colours in a plot
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(hp, mpg)) +
#'     geom_point(colour = wwfc_cols("bright gold 20""),
#'                size = 4,
#'                alpha = .8) +
#'     wolves_theme()
wwfc_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (wwfc_colours)

  wwfc_colours[cols]
}

# Palettes combine the wwfc colours
wwfc_palettes <- list(
  sir_jack = wwfc_cols("mexico green 19", "portugal green 20", "old gold", "portugal red 20", "ikeme hot pink"),
  wooly_bully = wwfc_cols("steel grey", "bully tourq", "blue skies 20", "light gold 20", "bright gold 20")
)

#' Return a function to interpolate an wwfc colour palette
#'
#' A function that is mainly intended for internal package use. Gets a pallete
#' by name from a list. Uses a boolean condition determining whether to reverse
#' the order or not, and additional arguments to pass on to colorRampPallete()
#' (such as an alpha value).
#'
#' Returns another function, which will interpolate the palette colors for a
#' certain number of levels, making it possible to create shades between our
#' original colours.
#'
#' The complete list of options is "wooly_bully" (the default) & "sir_jack". Use
#' e.g. view_palette("wooly_bully") to see the 5 core colours that make up a
#' palette.
#'
#' @param palette Character name of palette in wwfc_palettes
#' @param reverse logical indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @export
#'
#' @examples
#' wwfc_pal("wooly_bully")(8)
#' wwfc_pal("sir_jack")(12)
wwfc_pal <- function(palette = "wooly_bully", reverse = FALSE, ...) {

  pal <- wwfc_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' View a palette
#'
#' Displays a palette using geom_col() and theme_void().
#'
#' @param palette A character vector of colours to display.
#'
#' @export
#'
#' @examples
#' view_palette(wwfc_pal("wooly_bully")(8))
#'
#' # display wwfc palettes
#' library(ggplot2)
#'
#' wooly_bully <- view_palette(wwfc_pal("wooly_bully")(5)) +
#'     labs(title = '"wooly_bully" palette') +
#'     theme(plot.title = element_text(size = 18, hjust = .5))
#' sir_jack <- view_palette(wwfc_pal("sir_jack")(5)) +
#'   labs(title = '"sir_jack" palette') +
#'   theme(plot.title = element_text(size = 18, hjust = .5))
#' pal_plot <- cowplot::plot_grid(
#'   wooly_bully, sir_jack,
#'   nrow = 1
#'   )
#' pal_plot
view_palette <- function(palette) {
  # this just prevents a note being raised about the absence of global variables
  x <- y <- NULL

  plot_data <- tibble::tibble(x = palette, y = 1)

  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x, y, fill = palette)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}

#' Colour scale constructor for wwfc colours
#'
#' Custom colour scale functions for ggplot2. Handles discrete or continuous
#' scales.
#'
#' @param palette Character name of palette in wwfc_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or
#'   not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'   scale_color_gradientn() according to whether discrete arg is TRUE or FALSE
#'
#' @export
#'
#' @examples
#' # Color by discrete variable using default palette
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'   geom_point(size = 4) +
#'   wolves_theme() +
#'   scale_colour_wolves()
#'
#' # Color by numeric variable with likert palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'   geom_point(size = 4) +
#'   wolves_theme() +
#'   scale_colour_wolves(discrete = FALSE, palette = "sir_jack")
scale_colour_wolves <- function(palette = "wooly_bully",
                                discrete = TRUE,
                                reverse = FALSE,
                                ...) {
  pal <- wwfc_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("wwfc_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for wwfc colours
#'
#' Custom fill scale functions for ggplot2. Handles discrete or continuous
#' scales.
#'
#' @param palette Character name of palette in wwfc_palettes
#' @param discrete Boolean indicating whether fill aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'   scale_fill_gradientn() according to whether discrete arg is TRUE or FALSE
#'
#' @export
#'
#' @examples
#' # Fill by discrete variable with different palette + remove legend (guide)
#' # Note: you will probably want to avoid using colour in this way!
#' library(ggplot2)
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   wolves_theme(grid = "vgrid") +
#'   coord_flip() +
#'   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#'   scale_fill_wolves(palette = "wooly_bully", guide = "none") +
#'   labs(title = "scale_fill_wolves() can generate many colours from a palette",
#'        subtitle = "Using colour in this way (despite consistent data type) is not recommended!")
scale_fill_wolves <- function(palette = "wooly_bully", discrete = TRUE, reverse = FALSE, ...) {
  pal <- wwfc_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("wwfc_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
