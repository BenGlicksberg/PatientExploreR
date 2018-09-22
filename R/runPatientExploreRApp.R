#' Run PatientExploreR flagship application example.
#'
#' Launch demo application in `inst/examples/PatientExploreRApp`
#'
#' @export
#' @examples
#' if (interactive()) {
#'    patientExploreR::runPatientExploreRApp()
#' }
runPatientExploreRApp <- function() {
    appDir <- system.file("examples", "PatientExploreRApp", package="PatientExploreR")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `PatientExploreR`.", call.=FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
