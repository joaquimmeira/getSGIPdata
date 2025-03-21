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
get_parties_info <- function(level = NULL,
                             states = NULL,
                             ivigencia = NULL,
                             fvigencia = NULL) {

  inicio_vigencia <- .validate_date(ivigencia,
                                   default = Sys.Date(),
                                   is_start = TRUE)

  fim_vigencia <- .validate_date(fvigencia,
                                default = Sys.Date(),
                                is_start = FALSE,
                                inicio_vigencia)

  level <- ifelse(!(level %in% c("F", "E", "M")), "F", level)

  states <- .validate_states(states)
  parties_info <- .fetch_parties_info(level,
                                      states,
                                      inicio_vigencia,
                                      fim_vigencia)
  return(parties_info)
}

#' Fetch party information from the API (Internal)
#'
#' This function retrieves party information from the TSE API
#' based on the given states and validity dates.
#'
#' @param states A character vector with state abbreviations.
#' @param inicio_vigencia A string in "dd/mm/yyyy" format representing the start date.
#' @param fim_vigencia A string in "dd/mm/yyyy" format representing the end date.
#'
#' @return A dataframe with party information.
#' @keywords internal
.fetch_parties_info <- function(level, states, inicio_vigencia, fim_vigencia) {
  if("F" %in% level) {
  req <- request("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta") |>
         req_url_query(
           dataFimVigencia = fim_vigencia,
           dataInicioVigencia = inicio_vigencia,
           isComposicoesHistoricas = "true",
           nrZona = "0",
           sgUe = "",
           sqPartido = "0",
           tpAbrangencia = "81"
         )

    resp <- httr2::req_perform(req)
    info_parties <- httr2::resp_body_json(resp) |>
      tibble::tibble() |>
      tidyr::unnest_wider(dplyr::everything()) |>
      dplyr::rename("id_orgao_partidario" = sqOrgaoPartidario) |>
      dplyr::mutate(
        id_orgao_partidario = as.character(id_orgao_partidario),
        numero = as.character(numero)
        )

    return(info_parties)

  }
  if("E" %in% level) {
  purrr::map_df(states, ~{
    Sys.sleep(runif(1, 1, 3))  # Delay to avoid rate limits

    ifelse(.x == "DF",
       # Request for DF parties
       req <- request("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta") |>
         req_url_query(
           dataFimVigencia = fim_vigencia,
           dataInicioVigencia = inicio_vigencia,
           isComposicoesHistoricas = "true",
           nrZona = "0",
           sgUe = "",
           sqPartido = "0",
           tpAbrangencia = "84"
         ),
       # Resques for all states of Brasil
       req <- request("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta") |>
         req_url_query(
           dataFimVigencia = fim_vigencia,
           dataInicioVigencia = inicio_vigencia,
           isComposicoesHistoricas = "true",
           nrZona = "0",
           sgUe = .x,
        #  sgUeSuperior = .x,
           sqPartido = "0",
           tpAbrangencia = "82"
         )
)
    

    resp <- httr2::req_perform(req)
    info_parties <- httr2::resp_body_json(resp) |>
      tibble::tibble() |>
      tidyr::unnest_wider(dplyr::everything()) |>
      dplyr::rename("id_orgao_partidario" = sqOrgaoPartidario) |>
      dplyr::mutate(
        id_orgao_partidario = as.character(id_orgao_partidario),
        numero = as.character(numero)
        )
  
    return(info_parties)
  })
  }
  if("M" %in% level) {
  purrr::map_df(states, ~{
    Sys.sleep(runif(1, 1, 3))  # Delay to avoid rate limits

       # Resques for all states of Brasil
       req <- request("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/consulta") |>
         req_url_query(
           dataFimVigencia = fim_vigencia,
           dataInicioVigencia = inicio_vigencia,
           isComposicoesHistoricas = "true",
           nrZona = "0",
           sgUe = "",
           sgUeSuperior = .x,
           sqPartido = "0",
           tpAbrangencia = "83"
         )
    
    sqOrgaoPartidario <- NA_character_
    
    resp <- httr2::req_perform(req)
    info_parties <- httr2::resp_body_json(resp) |>
      tibble::tibble() |>
      tidyr::unnest_wider(dplyr::everything()) |>
      dplyr::mutate(
        id_orgao_partidario = as.character(sqOrgaoPartidario),
        numero = as.character(numero)
        )

    return(info_parties)
  })
    }
    
}
