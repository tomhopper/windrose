windrose <-
function(data = NULL,
                     spd,
                     dir,
                     spdres = NULL,
                     dirres = 45,
                     spdmin = NULL,
                     spdmax = NULL,
                     spdseq = NULL,
                     palette = "YlGnBu",
                     countmax = NA,
                     plot_rose = FALSE){
  
  
  # Look to see what data was passed in to the function
  if (is.null(data)) {
    if (is.numeric(spd) & is.numeric(dir)){
      # assume that we've been given vectors of the speed and direction vectors
      data <- data.frame(spd = spd,
                         dir = dir)
      spd <- "spd"
      dir <- "dir"
    }
  } else {
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
    if (class(data) == "data.frame"){
      spd <- deparse(substitute(spd))
      dir <- deparse(substitute(dir))
    } else {
      stop("Please pass a data frame to parameter 'data'.")
    }
  }  
  
  if (is.null(spdmin)) spdmin <- min(data[[spd]], na.rm = TRUE)
  if (is.null(spdmax)) spdmax <- max(data[[spd]], na.rm = TRUE)
  
  # Tidy up input data ----
  n_in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (is.null(spdseq)){
    if (is.null(spdres)) spdres <- (spdmax - spdmin)/10
    spdseq <- round(seq(spdmin, spdmax, spdres), digits = 1)
  } else {
    #     if (debug >0){
    #       cat("Using custom speed bins \n")
    #     }
  }
  # get some information about the number of bins, etc.
  n_spd_seq <- length(spdseq)
  n_colors_in_range <- n_spd_seq - 1
  
  # create the color map
  spd_colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n_colors_in_range),
                                                min(9,
                                                    n_colors_in_range)),                                               
                                            palette))(n_colors_in_range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd_breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd_labels <- c(paste(c(spdseq[1:n_spd_seq-1]),
                          '-',
                          c(spdseq[2:n_spd_seq])),
                    paste(spdmax,
                          "-",
                          max(data[spd],na.rm = TRUE)))
    spd_colors <- c(spd_colors, "grey50")
  } else{
    spd_breaks <- c(seq(spdseq))
    spd_labels <- paste(c(spdseq[1:n_spd_seq-1]),
                        '-',
                        c(spdseq[2:n_spd_seq]))    
  }
  data$spd_binned  <-  cut(x = data[[spd]],
                           breaks = spd_breaks,
                           labels = spd_labels,
                           ordered_result = TRUE)
  
  # figure out the wind direction bins
  dir_breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)
  dir_labels <- c(paste(round(360-dirres/2, 0),"-",round(dirres/2, 0)),
                  paste(round(seq(dirres/2, 360-3*dirres/2, by = dirres), 0),
                        "-",
                        round(seq(3*dirres/2, 360-dirres/2, by = dirres), 0)),
                  paste(round(360-dirres/2, 0),"-",round(dirres/2, 0)))
  # assign each wind direction to a bin
  dir_binned <- cut(data[[dir]],
                    breaks = dir_breaks,
                    ordered_result = TRUE)
  levels(dir_binned) <- dir_labels
  data$dir_binned  <-  dir_binned
  
  wind_data <- structure(list(data = data, dirres = dirres, countmax = countmax, spd_colors = spd_colors), 
                         class = "windrose")
  attr(wind_data, "hidden") <- c("dirres", "countmax", "spd_colors")
  
  if (plot_rose == TRUE) plot(data)
  
  return(wind_data)
}
