#' Get party members from a specific party organization
#'
#' This function makes a request to the SGIP API to retrieve members of a party organization,
#' identified by `id_orgao_partidario`, and returns the data as a tibble.
#'
#' @param id_orgao_partidario An integer representing the ID of the party organization.
#'
#' @return A tibble containing the members of the party organization.
#' @export
#'
#' @examples
#' \dontrun{
#'   members <- get_party_members(12345)
#'   print(members)
#' }
get_party_members <- function(id_orgao_partidario) {
  # Input validation
  if (!is.numeric(id_orgao_partidario) || length(id_orgao_partidario) != 1) {
    stop("The parameter 'id_orgao_partidario' must be a single integer.")
  }

  # Build the URL
  request_url <- stringr::str_glue(
    "https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/comAnotacoesEMembros?idOrgaoPartidario={id_orgao_partidario}"
  )

  # Error handling for the API request
  tryCatch({
    response <- jsonlite::read_json(request_url)

    # Check if the response contains the 'membros' key
    if (!"membros" %in% names(response)) {
      stop("The API response does not contain the 'membros' key.")
    }

    # Convert the list of members into a tibble and unnest the columns
    members <- response$membros |>
      dplyr::tibble(data = _) |>
      tidyr::unnest_wider(data)

    return(members)

  }, error = function(e) {
    stop("Error accessing the API: ", e$message)
  })
}
