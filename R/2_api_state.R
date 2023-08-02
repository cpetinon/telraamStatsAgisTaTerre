#'  Return the state of he Telraam API. Determine if updates can be made.
#'
#' @param key file containing an API key ("key.txt")
#'
#' @return TRUE if the API responds well, FALSE otherwise
#' @export
#'
#' @importFrom httr VERB
#'
#'
api_state <- function(key){
  VERB("GET", url = "https://telraam-api.net/v1", add_headers(key))$status_code == 200  # the request suceeded if equal to 200
}
