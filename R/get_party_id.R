#' Get the ID of all the Parties of the Brazil or a specific state
#'
#' @param states A character vector containing the abbreviation of the states of
#' Brazil. If nothing is inputed then will select all states
#'
#' @returns A dataframe containing the ID and information of the Parites
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  parties_id <- get_party_id(states = c("AL", "AC", "AM"))
#'
#' }
get_party_id <- function(states = NULL){

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
    'https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta?dataFimVigencia=18%2F02%2F2025&dataInicioVigencia=01%2F06%2F2017&isComposicoesHistoricas=true&nrZona=0&sgUe=&sgUeSuperior={states}&sqPartido=0&tpAbrangencia=83'
  )

  if ("DF" %in% states) {
    state_parties <- stringr::str_subset(state_parties, "DF", negate = TRUE) |>
     append(
      "https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta?dataFimVigencia=19%2F02%2F2025&dataInicioVigencia=19%2F02%2F2025&isComposicoesHistoricas=false&nrZona=0&sgUe=&sqPartido=0&tpAbrangencia=84"
    )
  }

  # Extracting the id of the parties
  id_parties <- state_parties |>
    purrr::map_df(
      ~{
        # Run the iteration in aleatory timing
        Sys.sleep(runif(1, 1, 3))

        .x |>
          jsonlite::read_json() |>
          dplyr::tibble(data = _) |>
          tidyr::unnest_wider(data) |>
          dplyr::rename(
            "id_orgao_partidario" = sqOrgaoPartidario,
            "tipo_orgao" = tipoOrgao,
            "situacao_vigencia" = situacaoiVigencia
          ) |>
          dplyr::select(
            id_orgao_partidario,
            sigla,
            numero,
            municipio,
            uf,
            tipo_orgao,
            situacao_vigencia,
            abrangencia,
            sgUeSede
          ) |>
          dplyr::mutate(
            id_orgao_partidario = as.character(id_orgao_partidario),
            numero = as.character(numero)
          )
      }
    )

  return(id_parties)
}
