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

    # Check if date format is correct
    if(is.na(inicio_vigencia)){
      stop(stringr::str_glue("Invalid date format: {ivigencia}. Use 'dd/mm/yyyy'."))
    }


    # Check if inicio vigencia is a valid date
    if(difftime(inicio_vigencia, as.Date("1900-01-01")) < 0){
      stop("You insert a date before 01/01/1900, you can only insert dates after 01/01/1900")
    }

    year_inicio_vigencia <- lubridate::year(inicio_vigencia)
    day_inicio_vigencia <- lubridate::day(inicio_vigencia)
    month_inicio_vigencia <- lubridate::month(inicio_vigencia)

    # Correction for days below 10
    if(day_inicio_vigencia < 10){
      day_inicio_vigencia <- stringr::str_glue("0{day_inicio_vigencia}")
    }
    # Correction for months below 10
    if(month_inicio_vigencia < 10){
      month_inicio_vigencia <- stringr::str_glue("0{month_inicio_vigencia}")
    }
  } else{
    # In case inicio_vigencia is NULL, the date for inicio_vigencia is setted as
    # the date of the system
    inicio_vigencia <- Sys.Date()

    year_inicio_vigencia <- lubridate::year(inicio_vigencia)
    day_inicio_vigencia <- lubridate::day(inicio_vigencia)
    month_inicio_vigencia <- lubridate::month(inicio_vigencia)

    # Correction for days below 10
    if(day_inicio_vigencia < 10){
      day_inicio_vigencia <- stringr::str_glue("0{day_inicio_vigencia}")
    }
    # Correction for months below 10
    if(month_inicio_vigencia < 10){
      month_inicio_vigencia <- stringr::str_glue("0{month_inicio_vigencia}")
    }
  }

  if(!is.null(fvigencia)){

    fim_vigencia <- lubridate::dmy(fvigencia)

    if(is.na(fim_vigencia)){
      stop(stringr::str_glue("Invalid date format: {fvigencia}. Use 'dd/mm/yyyy'."))
    }

    # Check if fim_vigencia is a valid date
    if(difftime(Sys.Date(), fim_vigencia) < 0){
      stop("You insert a date for fim_vigencia that is after the today, you can only insert before today")
    }

    year_fim_vigencia <- lubridate::year(fim_vigencia)
    day_fim_vigencia <- lubridate::day(fim_vigencia)
    month_fim_vigencia <- lubridate::month(fim_vigencia)

    # Correction for days below 10
    if(day_fim_vigencia < 10){
      day_fim_vigencia <- stringr::str_glue("0{day_fim_vigencia}")
    }
    # Correction for months below 10
    if(month_fim_vigencia < 10){
      month_fim_vigencia <- stringr::str_glue("0{month_fim_vigencia}")
    }

    # Check if inicio_vigencia is after fim_vigencia
    if(difftime(fim_vigencia,inicio_vigencia) < 0){
      stop("You insert a date for 'inicio_vigencia' that is before 'fim_vigencia'")
    }

  } else{
    # In case inicio_vigencia is NULL, the date for inicio_vigencia is setted as
    # the date of the system
    fim_vigencia <- Sys.Date()

    year_fim_vigencia <- lubridate::year(fim_vigencia)
    day_fim_vigencia <- lubridate::day(fim_vigencia)
    month_fim_vigencia <- lubridate::month(fim_vigencia)

    # Correction for days below 10
    if(day_fim_vigencia < 10){
      day_fim_vigencia <- stringr::str_glue("0{day_fim_vigencia}")
    }
    # Correction for months below 10
    if(month_fim_vigencia < 10){
      month_fim_vigencia <- stringr::str_glue("0{month_fim_vigencia}")
    }

    # Check if inicio_vigencia is after fim_vigencia
    if(difftime(fim_vigencia,inicio_vigencia) < 0){
      stop("You insert a date for 'inicio_vigencia' that is before 'fim_vigencia'")
    }
  }


  # Define the valid state abbreviations
  valid_states <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
                    "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
                    "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
                    "SP", "SE", "TO")

  # Validate input
  if (!is.null(states)) {
    if (!is.character(states)) {
      stop("States must be a character vector")
    }
    states <- toupper(states)  # Convert to uppercase
    invalid_states <- states[!states %in% valid_states]
    if (length(invalid_states) > 0) {
      stop(glue::glue("Invalid state(s): {paste(invalid_states, collapse = ', ')}"))
    }
  } else {
    warning("You selected all states")
    states <- valid_states
  }

  # Generate API URLs
  state_parties <- stringr::str_glue(
    'https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta?dataFimVigencia={day_fim_vigencia}%2F{month_fim_vigencia}%2F{year_fim_vigencia}&dataInicioVigencia={day_inicio_vigencia}%2F{month_inicio_vigencia}%2F{year_inicio_vigencia}&isComposicoesHistoricas=true&nrZona=0&sgUe=&sgUeSuperior={states}&sqPartido=0&tpAbrangencia=83'
  )

  if ("DF" %in% states) {
    state_parties <- stringr::str_subset(state_parties, "DF", negate = TRUE) |>
      append(
        stringr::str_glue("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta?dataFimVigencia={day_fim_vigencia}%2F{month_fim_vigencia}%2F{year_fim_vigencia}&dataInicioVigencia={day_inicio_vigencia}%2F{month_inicio_vigencia}%2F{year_inicio_vigencia}&isComposicoesHistoricas=false&nrZona=0&sgUe=&sqPartido=0&tpAbrangencia=84")
      )
  }

  # Extracting the id of the parties
  info_parties <- state_parties |>
    purrr::map_df(
      ~{
        # Run the iteration in aleatory timing
        Sys.sleep(runif(1, 1, 3))

        .x |>
          jsonlite::read_json() |>
          dplyr::tibble(data = _) |>
          tidyr::unnest_wider(data)
      }
    )

  return(info_parties)
}
