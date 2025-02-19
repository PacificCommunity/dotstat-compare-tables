library(dplyr)
library(fs)
library(httr2)
library(jsonlite)

# retrieve keycloac url for prod, stag, test environments
kc_url <- function(evt = c("prod", "stag", "test")) {
  kc_endpoint <- "/auth/realms/siscc/protocol/openid-connect/token"

  kc_url <- paste0(
    case_when(
        evt == "prod" ~ "https://stats-keycloak.pacificdata.org",
        evt == "stag" ~ "https://stats-keycloak-staging.pacificdata.org",
        evt == "test" ~ "https://stats-test-keycloak.pacificdata.org"
    ), kc_endpoint)

    return(kc_url)
}

get_user_credentials <- function(cred_path = path("~", "STDCREDS", ext = "json") |> path_expand() |> path_norm()) {
    if(cred_path |> file_exists()) {
        user_creds <- read_json(cred_path)
    } else {
        user_creds <- list()
        println("What is your Keycloak username?")
        println("What is your Keycloak password?")
    }
}

# Returns the keycloac token for the current user and specified environment
get_kc_token <- function(
    evt = c("prod", "stag", "test"),
    client_id = "client_id",
    grant_type = "password",
    ui_locales = "en",
    scope = "openid",
#    credentials_custom_path = FALSE,
    verbose = FALSE,
    return_full_response = FALSE) {

  user_credentials <- get_user_credentials()

  this_req <- request(
    kc_url(evt)
  ) |>
    req_method("POST") |>
    req_body_form(
      client_id = client_id,
      grant_type = grant_type,
      ui_locales = ui_locales,
      scope = scope,
      username = user_credentials$DSS_UID,
      password = user_credentials$DSS_PWD 
    )

  if(verbose) this_req |> req_dry_run()

  if(return_full_response) return(this_response)
  return(bearer)
}

# Request an access token
response <- POST(
  url = keycloak_url,
  body = list(
    grant_type = "client_credentials",
    client_id = client_id,
    client_secret = client_secret
  ),
  encode = "form"
)