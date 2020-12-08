#' Widening the dataframe
#'
#' @description This function will widen the dataframe and thus return the data to where each element is in a column.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf().
#'
#' @param projectpath The CSV file created with convertxrf().
#'
#' @importFrom dplyr select filter
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
    dplyr::filter(!Filter_blank %in% "blank") %>%
    dplyr::select(-c(1, Detection_limit)) %>%
    tidyr::pivot_wider(names_from = Element, values_from = Concentration)

  return(projectwide.df)

}


#' Widening the dataframe and removing concentrations below the detection limits
#'
#' @description This function will both widen the dataframe so each element has its own column, and remove concentrations that are below the detection limits.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf(), and where values below the detection limits are removed.
#'
#' @param projectpath The CSV file created with convertxrf().
#'
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' @export

widen_above <- function(projectpath) {

  project.df <- readr::read_csv(file = projectpath)

  projectabove.df <- project.df %>%
    dplyr::filter(Concentration > Detection_limit) %>%
    dplyr::select(-Detection_limit) %>%
    tidyr::pivot_wider(names_from = Element, values_from = Concentration, id_cols = Sample)

}

