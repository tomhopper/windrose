summary.windrose <-
function(object, ...) {
  s_windrose <- cat("Number of records = ", length(object$data$DateTime), "\n",
                    "Wind speed:", "\n",
                    names(summary(object$data$Wind_Speed_meter_per_second)), "\n",
                    summary(object$data$Wind_Speed_meter_per_second), "\n",
                    "Wind direction:", "\n",
                    names(summary(object$data$Wind_Direction_deg)), "\n",
                    summary(object$data$Wind_Direction_deg), "\n",
                    "Speed bins = ", length(levels(object$data$spd_binned)), "\n",
                    "Direction bins = ", length(levels(object$data$dir_binned)), "\n")
  return(s_windrose)
}
