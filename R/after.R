#' Widening the dataframe
#'
#' @description This function will widen the dataframe and thus return the data to where each element is in a column.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf().
#'
#' @param projectpath The file created by convertxrf().
#'
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#'
#' @examples
#'
#' @export

widen <- function(projectpath) {

  project.df <- readr::read_csv(file = projectpath)

  projectwide.df <- project.df %>%
    dplyr::select(-c(1, Detection_limit)) %>%
    tidyr::pivot_wider(names_from = Element, values_from = Concentration)

  projectwide.df <- as.data.frame(projectwide.df)

  return(projectwide.df)

}

