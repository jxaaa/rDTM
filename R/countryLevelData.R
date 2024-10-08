#' Global IDP Data Country Level
#' @description
#' This level will return information aggregated at country level to provide a broad snapshot of displacement dynamics.(https://dtm.iom.int/data-and-analysis/dtm-api)
#'
#' @param operation String, specifying the operation to perform.
#' @param countryName String, specifying the name of country.
#' @param admin0Pcode String, specifying the Country code ( ISO 3166-1 alpha-3). Must provide either the countryName or the admin0Pcode, not both.
#' @param fromDate String, specifying the start date of the reporting period (format: "yyyy-mm-dd"), optional.
#' @param toDate String, specifying the start date of the reporting period (format: "yyyy-mm-dd"), optional.
#' @param monthFrom_month String, specifying the start month of the reporting period. Between 1 and 12.
#' @param monthFrom_year Integer, specifying the start year of the reporting period. Year must be 4 digit.
#' @param monthTo_month Integer, specifying the end month of the reporting period. Between 1 and 12.
#' @param monthTo_year Integer, specifying the end year of the reporting period. Year must be 4 digit.
#' @param roundFrom Integer, specifying the start Data collection round number, optional. Not less than 0.
#' @param roundTo Integer, specifying the end Data collection round number, optional. Not less than 0.
#' @param to_dataframe Boolean, Convert the response to a DataFrame, default to False, optional.
#'
#' @return A json file or dataframe.
#'
#' @import dplyr httr jsonlite
#' @export
#'
#' @examples
#' #Retrieve the Global IDP Data Country Level for Afghanistan from Jan 2000 to May 2024, and convert output as dataframe.
#' result1 <- countryLevelData(countryName="Afghanistan", monthFrom_month= "1", monthFrom_year=2000, monthTo_month= "5", monthTo_year=2024,to_dataframe =TRUE)
#'
#' #Retrieve the Global IDP Data Country Level for Afghanistan from Jan 2000 to Dec 2023, and output as JSON.
#' result2 <- countryLevelData(countryName="Afghanistan", monthFrom_month= "1", monthFrom_year=2000, monthTo_month= "12", monthTo_year=2023,to_dataframe =FALSE)


countryLevelData <- function(operation = "", countryName = "", admin0Pcode = "", fromDate = NULL, toDate = NULL,
                             monthFrom_month = NULL, monthFrom_year = NULL, monthTo_month = NULL,
                             monthTo_year = NULL, roundFrom = NULL, roundTo = NULL, to_dataframe = FALSE) {
  # Check if all required parameters are provided
  if (countryName == "" && admin0Pcode == "") {
    stop("Please provide either the countryName or the admin0Pcode")
  }
  if (countryName != "" && admin0Pcode != "") {
    stop("Please provide either the countryName or the admin0Pcode, not both")
  }
  if (is.null(monthFrom_month) | !is.character(monthFrom_month)) {
    stop("Please provide the start month of the reporting period (str)")
  }
  if (is.null(monthFrom_year) | !is.numeric(monthFrom_year)) {
    stop("Please provide the start year of the reporting period (int)")
  }
  if (is.null(monthTo_month) | !is.character(monthTo_month)) {
    stop("Please provide the end month of the reporting period (str)")
  }
  if (is.null(monthTo_year) | !is.numeric(monthTo_year)) {
    stop("Please provide the end year of the reporting period (int)")
  }
  # The URL for getting admin data
  url <- "https://dtmapi.iom.int/api/IdpAdmin0Data/GetAdmin0Data"
  # The request body
  payload <- list(
    operation = operation,
    countryName = countryName,
    admin0Pcode = admin0Pcode,
    reportingDate = list(
      fromDate = fromDate,
      toDate = toDate
    ),
    reportMonthRange = list(
      monthFrom = list(
        year = monthFrom_year,
        month = monthFrom_month
      ),
      monthTo = list(
        year = monthTo_year,
        month = monthTo_month
      )
    ),
    roundNumber = list(
      from = roundFrom,
      to = roundTo
    )
  )

  payload_json <- toJSON(payload, auto_unbox = TRUE, null = "null")

  response <- POST(url,
                   body = payload_json,
                   encode = "json",
                   content_type_json())
  response_content <- content(response, as = "text", encoding = "UTF-8")
  response_json <- fromJSON(response_content)


  if (to_dataframe) {
    if (response_json$statusCode == 200) {
      return(as.data.frame(response_json$result))
    } else if (response_json$statusCode == 204) {
      stop("No data found in the DTM API for the given parameters")
    } else {
      stop(response_json$errorMessages)
    }
  } else {
    return(prettify(response_content))
  }
}
