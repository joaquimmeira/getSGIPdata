#' Validate a date input (Internal)
#'
#' This function checks if the given date is in the correct format ("dd/mm/yyyy"),
#' ensures it falls within allowed date ranges, and formats it accordingly.
#'
#' @param date A string containing a date in "dd/mm/yyyy" format. If NULL, the `default` value is used.
#' @param default A date to be used when `date` is NULL.
#' @param is_start A logical value indicating whether the date is a start date (`TRUE`) or an end date (`FALSE`).
#' @param inicio_vigencia A date object representing the start date. Required when `is_start = FALSE` to ensure
#' the end date is not earlier than the start date.
#'
#' @return A formatted date string in "dd/mm/yyyy".
#' @keywords internal
#'
#' @examples
#' .validate_date("15/08/2020", Sys.Date(), TRUE)
#' .validate_date(NULL, Sys.Date(), FALSE, as.Date("2020-08-15"))
.validate_date <- function(date,
                           default,
                           is_start,
                           inicio_vigencia = NULL) {
  if (!is.null(date)) {
    parsed_date <- lubridate::dmy(date)
    if (is.na(parsed_date)) {
      stop(stringr::str_glue(
        "Invalid date format: {date}. Use 'dd/mm/yyyy'."))
    }
    if (is_start && difftime(parsed_date, as.Date("1900-01-01")) < 0) {
      stop("You inserted a date before 01/01/1900, only dates after 01/01/1900 are allowed")
    }
    if (!is_start && difftime(Sys.Date(), parsed_date) < 0) {
      stop("You inserted a date for fim_vigencia that is after today, only past dates are allowed")
    }
    if (!is_start && difftime(parsed_date, inicio_vigencia) < 0) {
      stop("inicio_vigencia must be before fim_vigencia")
    }
  } else {
    parsed_date <- default
  }
  return(format(parsed_date, "%d/%m/%Y"))
}



#' Validate state abbreviations (Internal)
#'
#' This function checks whether the provided state abbreviations are valid Brazilian states.
#' If no states are provided, it returns all valid states with a warning.
#'
#' @param states A character vector containing Brazilian state abbreviations.
#' If NULL, a warning is issued, and all valid states are returned.
#'
#' @return A character vector with validated state abbreviations.
#' @keywords internal
#'
#' @examples
#' .validate_states(c("SP", "RJ", "MG"))
#' .validate_states(NULL) # Returns all states with a warning
.validate_states <- function(states) {
  valid_states <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
                    "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
                    "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
                    "SP", "SE", "TO")
  if (!is.null(states)) {
    if (!is.character(states)) {
      stop("States must be a character vector")
    }
    states <- toupper(states)
    invalid_states <- states[!states %in% valid_states]
    if (length(invalid_states) > 0) {
      stop(glue::glue("Invalid state(s): {paste(invalid_states, collapse = ', ')}"))
    }
  } else {
    warning("You selected all states")
    states <- valid_states
  }
  return(states)
}

#' Validate party organization ID (Internal)
#'
#' Ensures that the input is a character vector and checks for validity.
#'
#' @param id_orgao_partidario Character vector representing the party organization ID.
#' @return Validated character vector of party organization IDs.
#' @keywords internal
#' @importFrom utils data

.validate_id_orgao_partidario <- function(id_orgao_partidario) {
  parties_id <- get("parties_id", envir = asNamespace("getSGIPdata"))

  # If the parameter is NULL or an empty vector, return all IDs
  if (is.null(id_orgao_partidario) || length(id_orgao_partidario) == 0) {
    return(parties_id$sqOrgaoPartidario)
  }

  # Check if the parameter is a character vector
  if (!is.character(id_orgao_partidario)) {
    stop("The parameter 'id_orgao_partidario' must be a character vector.")
  }

  # Find invalid IDs
  invalid_id <- setdiff(id_orgao_partidario, parties_id$sqOrgaoPartidario)

  # If there are invalid IDs, generate an error message
  if (length(invalid_id) > 0) {
    stop(paste("The following are not valid 'id_orgao_partidario':", paste(invalid_id, collapse = ", ")))
  }

  # Return the validated vector of IDs
  return(id_orgao_partidario)
}
