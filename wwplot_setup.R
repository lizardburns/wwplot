library(usethis)
library(devtools)

# [Good site](https://imagecolorpicker.com) for picking colours from photos

available::available("wwplot")
create_package("~/Documents/projects/wwplot")
use_git()
use_gpl3_license()
use_lifecycle_badge("experimental")

use_readme_rmd()
build_readme()

use_pipe()

use_r("wolves_theme")
use_r("palettes")
use_r("alt_textify")
use_r("flip_y_title")
use_r("brand_plot")

use_package("ggplot2", "Imports")
use_package("cowplot", "Imports")
use_package("purrr", "Imports")
use_package("dplyr", "Imports")
use_package("magick", "Imports")
use_package('stringr', "Imports")
use_package("tibble", "Suggests")
use_package('emo', "Suggests")

# logo -------------------------------------------------------------------------
use_logo("../wwfc.png")
