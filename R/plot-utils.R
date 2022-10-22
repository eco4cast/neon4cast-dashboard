


forecast_plots <- function(df, ncol = NULL) {
  ggobj <-
    ggplot(df) +
    geom_point(aes(datetime, observation)) +
    geom_ribbon_interactive(aes(x = datetime, ymin = quantile02.5, ymax = quantile97.5,
                                fill = model_id, data_id = model_id, tooltip = model_id),
                            alpha = 0.2, show.legend=FALSE) +
    geom_line_interactive(aes(datetime, mean, col = model_id,
                              tooltip = model_id, data_id = model_id), show.legend=FALSE) +
    facet_wrap(~site_id, scales = "free", ncol=ncol) +
    theme(axis.text.x = element_text( angle = 90, hjust = 0.5, vjust = 0.5)) +
    theme_bw()


  girafe(ggobj = ggobj,
         width_svg = 8, height_svg = 4,
         options = list(
           opts_hover_inv(css = "opacity:0.20;"),
           opts_hover(css = "stroke-width:2;"),
           opts_zoom(max = 4)
         ))

}



leaderboard_plots <- function(leaderboard, by_start, var) {
  board1 <-
    leaderboard |>
    filter(variable == var) |>
    mutate(model_id = fct_rev(fct_reorder(model_id, crps))) |>   # sort by score
    pivot_longer(
      cols = c(crps, logs),
      names_to = "metric",
      values_to = "score"
    ) |>
    ggplot(aes(model_id, score, fill = model_id, col = model_id)) +
    geom_col_interactive(aes(tooltip = model_id, data_id = model_id),
                         show.legend = FALSE) +
    coord_flip() +
    facet_wrap( ~ metric, scales = "free_y") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    theme_bw()

  if (!is.null(by_start)) {
  board2 <-
    by_start |>
    filter(percent_na < .01) |>
    ggplot(aes(reference_datetime, crps, col = model_id)) +
    geom_point_interactive(
      aes(tooltip = model_id, data_id = model_id),
      size = 2,
      show.legend = FALSE
    ) +
    geom_line_interactive(
      aes(tooltip = model_id, data_id = model_id),
      size = 1,
      show.legend = FALSE
    ) +
    facet_wrap( ~ site_id) +
    theme_bw()
  }

  ggob <- board1
  if(!is.null(by_start))
    ggob <- board1 / board2 # patchwork stack

  girafe(
    ggobj = ggob,
    width_svg = 8,
    height_svg = 4,
    options = list(
      opts_hover_inv(css = "opacity:0.20;"),
      opts_hover(css = "stroke-width:2;"),
      opts_zoom(max = 4)
    )
  )

}
