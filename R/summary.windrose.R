#' @name summary
#' @title Prints a summary of a windrose object
#' @description Creates and prints a summary of a windrose object.
#' @usage summary(data_rose)
#' @param data (required) a windrose object
#' @return returns a summary string for a windrose object.
#' @export
#' @seealso \code{brewer.pal}.
summary.windrose <-
function(object, ...) {
  s_windrose <- cat(
                    "Wind speed:", "\n",
                    names(summary(object$data$Wind_Speed_meter_per_second)), "\n",
                    summary(object$data$Wind_Speed_meter_per_second), "\n",
                    "Wind direction:", "\n",
                    names(summary(object$data$Wind_Direction_deg)), "\n",
                    summary(object$data$Wind_Direction_deg), "\n",
                    "Number of records = ", length(object$data$DateTime), "\n",
                    "Speed bins = ", length(levels(object$data$spd_binned)), "\n",
                    "Direction bins = ", length(levels(object$data$dir_binned)), "\n")
  return(s_windrose)
}
