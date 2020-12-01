# importing the Excel file containing detection limits, crystal drift, molar weights, and calibration constants

importsetup <- function(setuppath) {
  setupfile.df <- readxl::read_excel(setuppath)
  setupfile.df <- setupfile.df %>%
    tidyr::pivot_longer(.data$PC : .data$GFF,
                 names_to = "Filter_type",
                 values_to = "Cal_const")

}
