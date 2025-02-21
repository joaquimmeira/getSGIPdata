#' Get the ID of all the Parties of the Brazil or a specific state
#'
#' @param states A character vector containing the abbreviation of the states of
#' Brazil or the Federal District. If nothing is inserted, all states will be
#' selected.
#' @param ivigencia A string that contains a date, must be in "dd/mm/yyyy" format,
#' that marks the beginning of the search. If nothing is inserted, the system date
#' will be used.
#' @param fvigencia A string that contains a date, must be in "dd/mm/yyyy" format,
#' that marks the end of the search. If nothing is inserted, the system date will
#' be used.
#'
#' @returns A dataframe containing the ID and information of the Parties
#' @export
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom purrr map_df
#' @importFrom lubridate dmy
#' @importFrom stringr str_glue
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr everything
#'
#' @examples
#' \dontrun{
#'
#'  parties_id <- get_party_id(
#'   states = c("AL", "AC", "AM"),
#'   ivigencia = "20/05/2017",
#'   fvigencia = "20/02/2022"
#'  )
#'
#' }
get_parties_info <- function(states = NULL, ivigencia = NULL, fvigencia = NULL){

  if(!is.null(ivigencia)){
    inicio_vigencia <- lubridate::dmy(ivigencia)
    if(is.na(inicio_vigencia)){
      stop(stringr::str_glue("Invalid date format: {ivigencia}. Use 'dd/mm/yyyy'."))
    }
    if(difftime(inicio_vigencia, as.Date("1900-01-01")) < 0){
      stop("You inserted a date before 01/01/1900, only dates after 01/01/1900 are allowed")
    }
  } else{
    inicio_vigencia <- Sys.Date()
  }

  if(!is.null(fvigencia)){
    fim_vigencia <- lubridate::dmy(fvigencia)
    if(is.na(fim_vigencia)){
      stop(stringr::str_glue("Invalid date format: {fvigencia}. Use 'dd/mm/yyyy'."))
    }
    if(difftime(Sys.Date(), fim_vigencia) < 0){
      stop("You inserted a date for fim_vigencia that is after today, only past dates are allowed")
    }
    if(difftime(fim_vigencia, inicio_vigencia) < 0){
      stop("inicio_vigencia must be before fim_vigencia")
    }
  } else{
    fim_vigencia <- Sys.Date()
    if(difftime(fim_vigencia, inicio_vigencia) < 0){
      stop("inicio_vigencia must be before fim_vigencia")
    }
  }

  inicio_vigencia <- format(inicio_vigencia, "%d/%m/%Y")
  fim_vigencia <- format(fim_vigencia, "%d/%m/%Y")

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

  info_parties <- purrr::map_df(states, ~{
    Sys.sleep(runif(1, 1, 3))

    req <- httr2::request("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta") |>
      httr2::req_url_query(
        dataFimVigencia = fim_vigencia,
        dataInicioVigencia = inicio_vigencia,
        isComposicoesHistoricas = "false",
        nrZona = "0",
        sgUe = ifelse(.x == "DF", "", .x),
        sqPartido = "0",
        tpAbrangencia = ifelse(.x == "DF", "84", "83")
      )

    resp <- httr2::req_perform(req)
    info_parties <- httr2::resp_body_json(resp) |>
      tibble::tibble() |>
      tidyr::unnest_wider(dplyr::everything())

    return(info_parties)
  })

  return(info_parties)
}
