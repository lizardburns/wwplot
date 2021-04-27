#' Automated generation of alt text
#'
#' This function relies on the inclusion of titles and subtitles in
#' plots to generate alt text in an automated way. It also incorporates caption
#' text (hidden by default in wolves_theme()) to add the section where further
#' details can be found so you must remember to specify this when creating plots
#' in order to be able to call alt_textify for complete alt text generation
#' later.
#'
#' Extracts the title, subtitle, caption and chart type (based on the first
#' geom_ layer specified) and stitches them together into 3 sentences of alt
#' text. Checks length and gives warning if over 255 characters.
#'
#' @param fig the plot to generate alt text for (with title, subtitle and
#'   caption labs set)
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
#'     geom_boxplot(fill = wwfc_cols("light gold 20")) +
#'     wolves_theme() +
#'     labs(title = "Virginica sepals are the longest",
#'          subtitle = "Sepal length (mm) by species",
#'          caption = "Comparing plant species",
#'          y = "")
#' alt_textify(p)
#'
#' # to output a text doc with alt text for all the figures within a document,
#' # you can, for example, add them to a named list and write them all out
#' # simultaneously
#' outlist <- purrr::map(list("fig1" = p), alt_textify)
#' outlist
#' # paste(names(outlist), outlist, sep = "\n") %>% write_lines("text.txt")
alt_textify <- function(fig) {
  # fetch the first geom to use as the name of the chart type
  geom_type <- class(fig$layers[[1]]$geom)[1] %>%
    stringr::str_remove(., "Geom") %>%
    stringr::str_to_lower()
  chart_type <- dplyr::case_when(
    geom_type == "col" ~ "bar",
    geom_type == "path" ~ "line",
    geom_type == "smooth" ~ "fitted line",
    TRUE ~ geom_type
  )

  # fjust the first word of subtitle retained
  desc_word1 <- stringr::str_split(fig$labels$subtitle, " ") %>%
    unlist() %>%
    .[1]

  # drop the case of the first character if not a proper noun/abbreviation since
  # we will insert the subtitle mid-sentence
  upper_words <- c(month.name)

  if(desc_word1 %in% upper_words) {
    desc_char1 <- stringr::str_sub(fig$labels$subtitle, 1, 1)
  } else {
    desc_char1 <- stringr::str_sub(fig$labels$subtitle, 1, 1) %>%
      stringr::str_to_lower()
  }

  alt_text <- paste0(
    "A ",
    chart_type,
    " chart showing the ",
    desc_char1,
    stringr::str_sub(fig$labels$subtitle, 2) %>% stringr::str_replace_all("\n", " "),
    ". ",
    fig$labels$title %>% stringr::str_replace_all("\n", " "),
    "\".\n"
  )
  # use 257 as the limit given the extra characters needed for writing out
  if(nchar(alt_text) > 257) {
    warning(paste("Alt text has too many characters and will need manual edits!"))
  }

  alt_text
}
