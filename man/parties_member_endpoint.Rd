% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{parties_member_endpoint}
\alias{parties_member_endpoint}
\title{parties_member_endpoint Dataset}
\format{
A tibble with 44,962 rows and 7 columns:
\describe{
\item{sqOrgaoPartidario}{Unique identification code for the party organization.}
\item{numero}{Party number used for identification.}
\item{sigla}{Abbreviation of the political party.}
\item{uf}{Federative unit (state) where the party is registered.}
\item{municipio}{Municipality where the party is registered.}
\item{situacaoiVigencia}{Status of the party (e.g., "Active").}
\item{members_endpoint}{API endpoint to access the party members' data, containing the specific URL.}
}
}
\source{
Data obtained from the SGIP API.
}
\usage{
data(parties_member_endpoint)
}
\description{
This dataset contains information about the SGIP API endpoints for accessing
data on political party members in Brazil. It includes details such as the party number,
the abbreviation, the federative unit (state), the municipality where the party is registered,
the party's status, and the specific endpoint to query the members of each party.
}
\details{
The dataset was extracted from the SGIP API, which provides access to the database of parties
and their members across the country. This dataset can be used to explore the parties and their
respective member information as a source for political analysis and electoral statistics.
}
