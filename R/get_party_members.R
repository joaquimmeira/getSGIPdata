#' Get party members from a specific party organization
#'
#' This function makes a request to the SGIP API to retrieve members of a party organization,
#' identified by `id_orgao_partidario`, and returns the data as a tibble.
#'
#' @param id_orgao_partidario Character representing the ID of the party organization.
#' If nothing is provided, it will run for all parties in Brazil at municipality level.
#'
#' @return A tibble containing the members of the party organization.
#' @export
#'
#' @examples
#' \dontrun{
#'   members <- get_party_members("447881")
#'   print(members)
#' }
get_party_members <- function(id_orgao_partidario = NULL) {
  id_orgao_partidario <- .validate_id_orgao_partidario(id_orgao_partidario)
  members_list <- purrr::map_df(id_orgao_partidario, .fetch_party_members)
  return(members_list)
}


#' Fetch party members from API (Internal)
#'
#' Retrieves party members based on the given party ID.
#'
#' @param party_id Character representing the party ID.
#' @return A tibble with party members.
#' @keywords internal
.fetch_party_members <- function(party_id) {
  Sys.sleep(runif(1, 1, 3))

  response <- .request_party_data(party_id)

  if (!"membros" %in% names(response)) {
    stop("The API response does not contain the 'membros' key.")
  }

  members <- .parse_members(response, party_id)
  members |> dplyr::glimpse()

  return(members)
}

#' Request party data from API (Internal)
#'
#' Sends a request to the API to retrieve party data.
#'
#' @param party_id Character representing the party ID.
#' @return A list containing the API response.
#' @keywords internal
.request_party_data <- function(party_id) {
  req <- request("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/comAnotacoesEMembros") |>
    req_url_query(idOrgaoPartidario = party_id)

  response <- httr2::req_perform(req) |> httr2::resp_body_json()
  return(response)
}

#' Parse API response into tibble (Internal)
#'
#' Converts the API response into a structured tibble.
#'
#' @param response List containing the API response.
#' @param party_id Character representing the party ID.
#' @return A tibble containing the parsed party members.
#' @keywords internal
.parse_members <- function(response, party_id) {
  members <- response$membros |>
    tibble::tibble() |>
    tidyr::unnest_wider(dplyr::everything()) |>
    dplyr::mutate(
      party_id = party_id,
      sigla = response$anotacaoOrgaoPartidarioInfo[[1]]$sigla,
      uf = response$anotacaoOrgaoPartidarioInfo[[1]]$uf,
      municipio = response$anotacaoOrgaoPartidarioInfo[[1]]$municipio
    )

  return(members)
}
