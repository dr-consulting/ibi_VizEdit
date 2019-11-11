#' Finding user home directory.
#'
#' \code{get_user_folder} is designed to identify the home directory of the user who initiated the R session.
#'

get_user_folder <- function(){
  if(Sys.getenv('USERPROFILE')=="")
    user_folder<-"~"
  else if(Sys.getenv("HOMEPATH")!="")
    user_folder<-Sys.getenv('USERPROFILE')
  else
    user_folder<-'C:/'

  return(user_folder)
}


#' Setting maximum file size
#'
#' \code{set_file_size_max} is an internal utility that sets the maximum file size for an input data set in megabytes.
#' The default is set to 150 MB. If this is too low in general or for a specific use case, it can be adjusted here.
#'

set_file_size_max <- function(size = 150){
  options(shiny.maxRequestSize=size*1024^2)
}


#' Initialize global reactive values.
#'
#' \code{ini_global_reactive_values} is an internal utility that initializes a set of global reactive values that affect
#' the behavior of the GUI, including colors, displays, and which actions are available to a user.
#'
#' @param tot_edits is a \code{dataframe} that is used to maintain a running track record unique edits made by a user.
#' @param base_on is used to toggle access to the "base" editing functions on and off. Default is off.
#' @param adv_on is used to toggle access to the "advanced" editing functions on and off. Default is off.
#' @param ppg_on is used to toggle the display of the underlying PPG waveform on and off. Only works when a user is
#' zoomed in on a section of the file. Default is off.
#' @param select_on is used to toggle on access to the select brush on the "base" editing panel. This particular select
#' brush supports the use of the "combine", "divide", and "average" functions. Default is off.
#' @param add_delete_on is used to toggle on access to mouse-click-based "add" and "delete" functionality.
#' @param select_on2 is used to toggle on access to the select brush on the "advanced" editing panel. This particular
#' select brush allows the user to specify the section of corrupted data targeted for imputation. Default is off.
#' @param add_delete_on2 is used to toggle on access to mouse-click-based "add" and "delete" functionality. Default is
#' off.
#' @param start.time is used to store the time and date the user began processing a target file.
#' @param GP_impute_tab is used to store ongoing information about Gaussian imputation attempts. The table is included
#' in the final output.
#'

ini_global_reactive_values <- function(){
  rv_start <- reactiveValues(tot.edits=data.frame(), base.on=0, adv.on=0, ppg.on=0, select.on=0, add.delete.on=0,
                             select.on2=0, add.delete.on2=0, start.time=NULL, GP.impute.tab=NULL, IBI.temp=NULL)
  return(rv_start)
}


#' Set min/max to 0/1.
#'
#' \code{range01} is an internal utility that takes a vector of values and returns a new vector with the same
#' distribution, re-scaled to a range of 0 to 1.
#'

range01 <- function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}
