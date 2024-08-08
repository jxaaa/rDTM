#' Global IDP Data at Admin 1 Level
#' @description
#' This level will return information aggregated at the administrative boundaries level 1, states, provinces and equivalent, still providing high-level information, but that drills down past country-level.(https://dtm.iom.int/data-and-analysis/dtm-api)
#'
#' @inheritParams countryLevelData
#' @return A json file or dataframe.
#'
#' @import dplyr httr jsonlite
#' @export
#'
#' @examples
#' #Retrieve the Global IDP Data at Admin 1 Level for Afghanistan from Jan 2000 to May 2024, and convert output as dataframe.
#' result1 <- admin1LevelData(admin0Pcode="AFG", monthFrom_month= "1", monthFrom_year=2000, monthTo_month= "5", monthTo_year=2024,to_dataframe =TRUE)
#'
#' #Retrieve the Global IDP Data at Admin 1 Level for Afghanistan from Jan 2000 to Dec 2023, and output as JSON.
#' result2 <- admin1LevelData(countryName="Afghanistan", monthFrom_month= "1", monthFrom_year=2000, monthTo_month= "12", monthTo_year=2023,to_dataframe =FALSE)

admin1LevelData <- function(operation = "", countryName = "", admin0Pcode = "", fromDate = NULL, toDate = NULL,
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
  if (is.null(monthFrom_year) | !is.integer(monthFrom_year)) {
    stop("Please provide the start year of the reporting period (int)")
  }
  if (is.null(monthTo_month) | !is.character(monthTo_month)) {
    stop("Please provide the end month of the reporting period (str)")
  }
  if (is.null(monthTo_year) | !is.integer(monthTo_year)) {
    stop("Please provide the end year of the reporting period (int)")
  }
  # The URL for getting admin0 data
  url <- "https://dtmapi.iom.int/api/IdpAdmin1Data/GetAdmin1Data"
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
  response_content <- content(response, as = "text")
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
