# importing and modifying the setuppath
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer

importsetup <- function(setuppath) {
  setupfile.df <- read_excel(setuppath)
  setupfile.df <- setupfile.df %>%
    pivot_longer(.data$PC : .data$GFF,
                 names_to = "Filter_type",
                 values_to = "Cal_const")

}
