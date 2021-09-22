
patch_legend <- function(gp){
  df <- data.frame(id = seq_along(gp$x$data),
                   legend_entries = unlist(lapply(gp$x$data, `[[`, "name")))
  # Extract the group identifier
  df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
  # Add an indicator for the first entry per group
  df$is_first <- !duplicated(df$legend_group)

  for (i in df$id) {
    # Is the layer the first entry of the group?
    is_first <- df$is_first[[i]]
    # Assign the group identifier to the name and legendgroup arguments
    gp$x$data[[i]]$name <- df$legend_group[[i]]
    gp$x$data[[i]]$legendgroup <- gp$x$data[[i]]$name
    # Show the legend only for the first layer of the group
    if (!is_first) gp$x$data[[i]]$showlegend <- FALSE
  }
  gp
}
