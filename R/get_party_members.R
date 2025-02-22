#' Get party members from a specific party organization
#'
#' This function makes a request to the SGIP API to retrieve members of a party organization,
#' identified by `id_orgao_partidario`, and returns the data as a tibble.
#'
#' @param id_orgao_partidario An integer representing the ID of the party organization.
#' If nothing is provided, it will run for all parties in Brazil at municipality level.
#'
#' @return A tibble containing the members of the party organization.
#' @export
#'
#' @examples
#' \dontrun{
#'   members <- get_party_members(12345)
#'   print(members)
#' }
get_party_members <- function(id_orgao_partidario = NULL) {

  if(is.null(id_orgao_partidario)){
    id_orgao_partidario <- parties_id$sqOrgaoPartidario
  } else{

    # Input validation
    if (!is.character(id_orgao_partidario)) {
      stop("The parameter 'id_orgao_partidario' must be a character vector.")
    } else{
      # Check if id_orgao_partidario is valid
      if(!all(id_orgao_partidario %in% parties_id$sqOrgaoPartidario)){
        invalid_id <- setdiff(id_orgao_partidario, parties_id$sqOrgaoPartidario)
        stop(stringr::str_glue("{paste(invalid_id, collapse = ', ')} isn't a valid id_orgao_partidario"))
      }

    }

  }
  members_list <- purrr::map_df(
    id_orgao_partidario,
    ~ {

      Sys.sleep(runif(1, 1, 3))

      # Build the URL
      req <- request("https://sgip3.tse.jus.br/sgip3-consulta/api/v1/orgaoPartidario/comAnotacoesEMembros") |>
        req_url_query(idOrgaoPartidario = .x)

      response <- httr2::req_perform(req) |> httr2::resp_body_json()

      # Check if the response contains the 'membros' key
      if (!"membros" %in% names(response)) {
        stop("The API response does not contain the 'membros' key.")
      }

      # Convert the list of members into a tibble and unnest the columns
      members <- response$membros |>
        tibble::tibble() |>
        tidyr::unnest_wider(dplyr::everything()) |>
        dplyr::mutate(
          party_id = .x,
          sigla = resp$anotacaoOrgaoPartidarioInfo[[1]]$sigla,
          uf = resp$anotacaoOrgaoPartidarioInfo[[1]]$uf,
          municipio = resp$anotacaoOrgaoPartidarioInfo[[1]]$municipio
        )


      members |> dplyr::glimpse()

      return(members)
    }
  )
}
