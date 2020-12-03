#' Widening the dataframe
#'
#' @description This function will widen the dataframe and thus return the data to where each element is in a column.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf().
#'
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' @export

widen <- function() {

  project.df <- as.data.frame(project.df)

# DOESN'T WORK:
  projectwide.df <- project.df %>%
    dplyr::select(-(.data$Detection_limit)) %>%
    group_by(Treatment) %>%
    tidyr::pivot_wider(names_from = Element, values_from = Concentration, values_fn = list(Concentration = mean))

  return(projectwide.df)

}

