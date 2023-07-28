#' Filter Group by Name
#'
#' Filter a clusterised object that has been assigned groups and remove groups that are not a specified label.
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param group_number A number that identifies the group to be extracted.
#'
#' @return A new clusterised object that only contains sites from the specified group.
#' @export
#'
#' @examples # extract groups 6, 5, and 1 from a clusterised object
#' \dontrun{Colombia_Meta_6 <- filter_groups_by_name(clusterised_Meta_20km_groups, 6)}
#' \dontrun{Colombia_Meta_5 <- filter_groups_by_name(clusterised_Meta_20km_groups, 5)}
#' \dontrun{Colombia_Meta_1 <- filter_groups_by_name(clusterised_Meta_20km_groups, 1)}
filter_groups_by_number <- function(clusterised_object, group_number){

  split_centred_coordinates <- split(clusterised_object[[2]],
                                     clusterised_object[[2]]$site_group)

  NSites <- numeric()
  for(i in 1:length(split_centred_coordinates)){
    NSites[i] <- length(split_centred_coordinates[[i]]$site_group)
  }

  index_filter <- clusterised_object[[2]]$site_group %in% group_number

  new_dataframe_list <- clusterised_object[[1]][index_filter]
  new_centred_coordinates <- clusterised_object[[2]][index_filter,]

  return(
    list(new_dataframe_list, new_centred_coordinates)
  )

}

