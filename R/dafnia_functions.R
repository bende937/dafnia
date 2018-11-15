# Functions to aggregate and plot the dafnia dataset
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' aggregates a column in a dataframe by factors in other columns
#'
#' @param df a dataframe with columns \code{out} and elements of \code{by}
#' @param out a column name (string) in dataframe \code{df} that you
#' want to aggregate/summarize
#' @param by a vector of column names (strings) that you want to aggregate by
#' @return a dataframe with the statistics of the aggregate
#' @example aggregate_result(dafnia, "Feeding", c("Lake", "Temp"))
#' @export
aggregate_result <- function(df, out, by) {
  aggr <- aggregate(df[[out]],
                    by = lapply(by, function(arg){df[[arg]]}),
                    FUN = function(x) c(mean = mean(x),
                                        sd = sd(x),
                                        n = length(x)))
  aggr <- do.call(data.frame, aggr) %>%
    setNames(c(by, "mean", "sd", "n")) %>%
    mutate(se = sd / sqrt(n))
  return(aggr)
}



# we create a function to plot an aggregated dataframe
plot_aggregate <- function(aggr, x, fill, y="mean", e="se",
                           ylabel=y, xlabel=x, fill_label=fill,
                           fill_colors=c("royalblue1", "peachpuff1")) {

  ggplot(data = aggr, aes(x = factor(get(x, aggr)),
                          y = get(y, aggr),
                          fill = factor(get(fill, aggr)))) +
    geom_bar(stat = "identity",
             position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymax = aggr[[y]] + aggr[[e]],
                      ymin = aggr[[y]] - aggr[[e]]),
                  position = position_dodge(width = 0.9),
                  width = 0.25) +
    labs(x = xlabel, y = ylabel) +
    scale_fill_manual(values=fill_colors, name=fill_label)
}
