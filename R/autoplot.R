# SpCV Block -------------------------------------------------------------------

#' @title Visualization Functions for SpCV Block Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @import ggplot2
#'
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVBlock] or
#'   [ResamplingRepeatedSpCVBlock].
#' @param task `[TaskClassifST]/[TaskRegrST]`\cr
#'   mlr3 task object.
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVBlock] or
#'   [ResamplingRepeatedSpCVBlock].
#'   mlr3 task object.
#' @param fold_id `[numeric]`\cr
#'   Fold IDs to plot.
#' @param repeats_id `[numeric]`\cr
#'   Repetition ID to plot.
#' @param plot_as_grid `[logical(1)]`\cr
#'   Should a gridded plot using via \CRANpkg{patchwork} be created? If `FALSE`
#'   a list with of \CRANpkg{ggplot2} objects is returned.
#'   Only applies if a numeric vector is passed to argument `fold_id`.
#' @param train_color `[character(1)]`\cr
#'   The color to use for the training set observations.
#' @param test_color `[character(1)]`\cr
#'   The color to use for the test set observations.
#' @param show_blocks `[logical(1)]`\cr
#'   Whether to show an overlay of the spatial blocks polygons.
#' @param show_labels `[logical(1)]`\cr
#'   Whether to show an overlay of the spatial block IDs.
#' @param ... Passed to `geom_sf()`. Helpful for adjusting point sizes and
#'   shapes.
#' @details
#' By default a plot is returned; if `fold_id` is set, a gridded plot is
#' created. If `plot_as_grid = FALSE`, a list of plot objects is returned.
#' This can be used to align the plots individually.
#'
#' When no single fold is selected, the [ggsci::scale_color_ucscgb()] palette
#' is used to display all partitions.
#' If you want to change the colors, call `<plot> + <color-palette>()`.
#' @return [ggplot()] or list of ggplot2 objects.
#' @name autoplot.ResamplingSpCVBlock
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @export
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_block", range = 1000L)
#'   resampling$instantiate(task)
#'
#'   ## list of ggplot2 resamplings
#'   plot_list = autoplot(resampling, task,
#'     crs = 4326,
#'     fold_id = c(1, 2), plot_as_grid = FALSE)
#'
#'   ## Visualize all partitions
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'
#'   ## Visualize the train/test split of a single fold
#'   autoplot(resampling, task, fold_id = 1) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'
#'   ## Visualize train/test splits of multiple folds
#'   autoplot(resampling, task,
#'     fold_id = c(1, 2),
#'     show_blocks = TRUE) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_blocks = FALSE,
  show_labels = FALSE,
  ...) {

  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_blocks = show_blocks,
    show_labels = show_labels,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingSpCVBlock
#' @export
autoplot.ResamplingRepeatedSpCVBlock = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_blocks = FALSE,
  show_labels = FALSE,
  ...) {

  autoplot.ResamplingSpCVBlock(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_blocks = show_blocks,
    show_labels = show_labels,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object. One of class [ResamplingSpCVBuffer],
#'   [ResamplingSpCVBlock], [ResamplingSpCVCoords], [ResamplingSpCVEnv].
#' @rdname autoplot.ResamplingSpCVBlock
#' @export
plot.ResamplingSpCVBlock = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVBlock
#' @export
plot.ResamplingRepeatedSpCVBlock = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCV Env ---------------------------------------------------------------------

#' @title Visualization Functions for SpCV Env Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVEnv] or
#'   [ResamplingRepeatedSpCVEnv].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVEnv] or
#'   [ResamplingRepeatedSpCVEnv].
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_env", folds = 4, features = "dem")
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingSpCVEnv
#' @export
autoplot.ResamplingRepeatedSpCVEnv = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {

  autoplot.ResamplingSpCVEnv(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVEnv
#' @export
plot.ResamplingSpCVEnv = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVEnv
#' @export
plot.ResamplingRepeatedSpCVEnv = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCV Coords ------------------------------------------------------------------

#' @title Visualization Functions for SpCV Coords Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @name autoplot.ResamplingSpCVCoords
#'
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVCoords] or
#'   [ResamplingRepeatedSpCVCoords].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSpCVCoords] or
#'   [ResamplingRepeatedSpCVCoords].
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_coords")
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingSpCVCoords
#' @export
autoplot.ResamplingRepeatedSpCVCoords = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {

  autoplot.ResamplingSpCVCoords(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVCoords
#' @export
plot.ResamplingSpCVCoords = function(x, ...) {
  print(autoplot(x, ...))
}

#' @rdname autoplot.ResamplingSpCVCoords
#' @export
plot.ResamplingRepeatedSpCVCoords = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SptCV Cluto ------------------------------------------------------------------

#' @title Visualization Functions for SptCV Cluto Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSptCVCluto] or
#'   [ResamplingRepeatedSptCVCluto].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingSptCVCluto] or
#'   [ResamplingRepeatedSptCVCluto].
#' @param tickformat_date `[character]`\cr
#'   Date format for z-axis.
#' @param nticks_y `[integer]`\cr
#'   Number of y axis breaks. Only applies to SptCVCluto.
#' @param nticks_x `[integer]`\cr
#'   Number of x axis breaks. Only applies to SptCVCluto.
#' @param point_size `[numeric]`\cr
#'   Point size of markers.
#' @param axis_label_fontsize `[integer]`\cr
#'   Font size of axis labels.
#' @name autoplot.ResamplingSptCVCluto
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - Vignette [Spatiotemporal Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#' @examples
#' \dontrun{
#' if (mlr3misc::require_namespaces(c("sf", "skmeans", "plotly"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task_st = tsk("cookfarm")
#'   resampling = rsmp("sptcv_cluto", folds = 5, time_var = "Date")
#'   resampling$instantiate(task_st)
#'
#'   # plot
#'   autoplot(resampling, task_st)
#'   autoplot(resampling, task_st, fold_id = 1)
#'   autoplot(resampling, task_st, fold_id = c(1, 2))
#' }
#' }
autoplot.ResamplingSptCVCluto = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  tickformat_date = "%Y-%m",
  nticks_x = 3,
  nticks_y = 3,
  point_size = 3,
  axis_label_fontsize = 11,
  ...) {

  autoplot_spatiotemp(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    tickformat_date = tickformat_date,
    nticks_y = nticks_y,
    nticks_x = nticks_y,
    point_size = point_size,
    axis_label_fontsize = axis_label_fontsize,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingSptCVCluto
#' @export
autoplot.ResamplingRepeatedSptCVCluto = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {

  autoplot.ResamplingSptCVCluto(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSptCVCluto
#' @export
plot.ResamplingSptCVCluto = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSptCVCluto
#' @export
plot.ResamplingRepeatedSptCVCluto = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# SpCVDisc ---------------------------------------------------------------------

#' @title Visualization Functions for SpCV Disc Method.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @importFrom stats na.omit
#'
#' @details
#' This method requires to set argument `fold_id` and no plot containing all
#' partitions can be created. This is because the method does not make use of
#' all observations but only a subset of them (many observations are left out).
#' Hence, train and test sets of one fold are not re-used in other folds as in
#' other methods and plotting these without a train/test indicator would not
#' make sense.
#'
#' @section 2D vs 3D plotting:
#' This method has both a 2D and a 3D plotting method.
#' The 2D method returns a \pkg{ggplot} with x and y axes representing the spatial
#' coordinates.
#' The 3D method uses \pkg{plotly} to create an interactive 3D plot.
#' Set `plot3D = TRUE` to use the 3D method.
#'
#' Note that spatiotemporal datasets usually suffer from overplotting in 2D
#' mode.
#'
#' @name autoplot.ResamplingSpCVDisc
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @param show_omitted `[logical]`\cr
#'   Whether to show points not used in train or test set for the current fold.
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - Vignette [Spatiotemporal Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces("sf", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_disc",
#'     folds = 5, radius = 200L, buffer = 200L)
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task,
#'     fold_id = 1,
#'     show_omitted = TRUE, size = 0.7) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVDisc = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  repeats_id = NULL,
  show_omitted = FALSE,
  ...) {

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(repeats_id)) {
    repeats_id = 1
  } else {
    repeats_id = repeats_id
    # otherwise it gets passed to geom_sf() down below
    # dots$repeats_id = NULL
  }

  resampling_sub = resampling$clone()

  if (grepl("Repeated", class(resampling)[1])) {
    resampling_sub$instance = resampling_sub$instance[[repeats_id]]
  }

  if (!is.null(fold_id)) {
    if (length(fold_id) == 1) {
      ### only one fold

      data_coords = prepare_autoplot_cstf(task, resampling_sub)

      # suppress undefined global variables note
      data_coords$indicator = ""

      row_id_test = resampling_sub$instance$test[[fold_id]]
      row_id_train = resampling_sub$instance$train[[fold_id]]

      data_coords[row_id %in% row_id_test, indicator := "Test"]
      data_coords[row_id %in% row_id_train, indicator := "Train"]

      # should omitted points be shown?
      if (show_omitted && nrow(data_coords[indicator == ""]) > 0) {
        data_coords[indicator == "", indicator := "Omitted"]

        sf_df = sf::st_as_sf(data_coords,
          coords = task$extra_args$coordinate_names,
          crs = task$extra_args$crs)
        sf_df = reorder_levels(sf_df)

        ggplot() +
          geom_sf(data = sf_df, aes(color = indicator), ...) +
          scale_color_manual(values = c(
            "Omitted" = "grey",
            "Train" = "#0072B5",
            "Test" = "#E18727"
          )) +
          labs(color = "Set", title = sprintf(
            "Fold %s, Repetition %s", fold_id,
            repeats_id)) +
          theme(
            plot.title = ggtext::element_textbox(
              size = 10,
              color = "black", fill = "#ebebeb", box.color = "black",
              height = unit(0.33, "inch"), width = unit(1, "npc"),
              linetype = 1, r = unit(5, "pt"),
              valign = 0.5, halign = 0.5,
              padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
          )
      } else {
        data_coords = data_coords[indicator != ""]

        sf_df = sf::st_as_sf(data_coords,
          coords = task$extra_args$coordinate_names,
          crs = task$extra_args$crs)
        sf_df = reorder_levels(sf_df)

        ggplot() +
          geom_sf(data = sf_df, aes(color = indicator), ...) +
          scale_color_manual(values = c(
            "Train" = "#0072B5",
            "Test" = "#E18727"
          )) +
          labs(color = "Set", title = sprintf(
            "Fold %s, Repetition %s", fold_id,
            repeats_id)) +
          theme(
            plot.title = ggtext::element_textbox(
              size = 10,
              color = "black", fill = "#ebebeb", box.color = "black",
              height = unit(0.33, "inch"), width = unit(1, "npc"),
              linetype = 1, r = unit(5, "pt"),
              valign = 0.5, halign = 0.5,
              padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
          )
      }
    }
    else {
      ### Multiplot of multiple partitions with train and test set

      # FIXME: redundant code - call function from single plots?
      plot_list = mlr3misc::map(fold_id, function(.x) {

        data_coords = prepare_autoplot_cstf(task, resampling_sub)

        # suppress undefined global variables note
        data_coords$indicator = ""

        row_id_test = resampling_sub$instance$test[[.x]]
        row_id_train = resampling_sub$instance$train[[.x]]

        data_coords[row_id %in% row_id_test, indicator := "Test"]
        data_coords[row_id %in% row_id_train, indicator := "Train"]

        # should omitted points be shown?
        if (show_omitted && nrow(data_coords[indicator == ""]) > 0) {
          data_coords[indicator == "", indicator := "Omitted"]

          sf_df = sf::st_as_sf(data_coords,
            coords = task$extra_args$coordinate_names,
            crs = task$extra_args$crs)
          sf_df = reorder_levels(sf_df)

          ggplot() +
            geom_sf(data = sf_df, aes(color = indicator), ...) +
            scale_color_manual(values = c(
              "Omitted" = "grey",
              "Train" = "#0072B5",
              "Test" = "#E18727"
            )) +
            labs(color = "Set", title = sprintf(
              "Fold %s, Repetition %s", .x,
              repeats_id)) +
            theme(
              plot.title = ggtext::element_textbox(
                size = 10,
                color = "black", fill = "#ebebeb", box.color = "black",
                height = unit(0.33, "inch"), width = unit(1, "npc"),
                linetype = 1, r = unit(5, "pt"),
                valign = 0.5, halign = 0.5,
                padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
            )
        } else {
          data_coords = data_coords[indicator != ""]

          sf_df = sf::st_as_sf(data_coords,
            coords = task$extra_args$coordinate_names,
            crs = task$extra_args$crs)
          sf_df = reorder_levels(sf_df)

          ggplot() +
            geom_sf(data = sf_df, aes(color = indicator), ...) +
            scale_color_manual(values = c(
              "Train" = "#0072B5",
              "Test" = "#E18727"
            )) +
            labs(color = "Set", title = sprintf(
              "Fold %s, Repetition %s", .x,
              repeats_id)) +
            theme(
              plot.title = ggtext::element_textbox(
                size = 10,
                color = "black", fill = "#ebebeb", box.color = "black",
                height = unit(0.33, "inch"), width = unit(1, "npc"),
                linetype = 1, r = unit(5, "pt"),
                valign = 0.5, halign = 0.5,
                padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
            )
        }
      })

      # Return a plot grid via patchwork?

      if (!plot_as_grid) {
        return(invisible(plot_list))
      } else {
        # for repeated cv we also print out the rep number
        if (is.null(repeats_id)) {
          repeats_id = 1 # nocov
        }

        plot_list_pw = patchwork::wrap_plots(plot_list) +
          patchwork::plot_layout(guides = "collect")
        return(plot_list_pw)
      }
    }
  } else {

    ### Create one plot colored by all test folds

    data_coords = prepare_autoplot_cstf(task, resampling_sub)

    # extract test ids from lists
    row_ids_test = data.table::rbindlist(
      lapply(resampling_sub$instance$test, as.data.table),
      idcol = "fold")
    setnames(row_ids_test, c("fold", "row_id"))

    test_folds = merge(data_coords, row_ids_test, by = "row_id", all = TRUE)

    sf_df = sf::st_as_sf(test_folds,
      coords = task$extra_args$coordinate_names,
      crs = task$extra_args$crs)

    # only keep test ids
    sf_df = stats::na.omit(sf_df, cols = "fold")

    # order fold ids
    sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
    sf_df$fold = as.factor(as.character(sf_df$fold))
    sf_df$fold = factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

    # for all non-repeated rsmp cases
    if (is.null(repeats_id)) {
      repeats_id = 1 # nocov
    }

    plot = ggplot() +
      geom_sf(
        data = sf_df["fold"], show.legend = "point",
        aes(color = fold)
      ) +
      ggsci::scale_color_ucscgb() +
      labs(color = sprintf("Partition #, Rep %s", repeats_id))
    return(plot)
  }
}

#' @rdname autoplot.ResamplingSpCVDisc
#' @export
autoplot.ResamplingRepeatedSpCVDisc = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_omitted = FALSE,
  ...) {

  autoplot.ResamplingSpCVDisc(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_omitted = show_omitted,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVDisc
#' @export
plot.ResamplingSpCVDisc = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVDisc
#' @export
plot.ResamplingRepeatedSpCVDisc = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}


# SpCVTiles --------------------------------------------------------------------

#' @title Visualization Functions for SpCV Tiles Method.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods to
#'   visualize mlr3 spatiotemporal resampling objects.
#'
#' @importFrom stats na.omit
#'
#' @details
#' Specific combinations of arguments of `"spcv_tiles"` remove some
#' observations, hence `show_omitted` has an effect in some cases.
#'
#' @name autoplot.ResamplingSpCVTiles
#' @inheritParams autoplot.ResamplingSpCVBlock
#'
#' @param show_omitted `[logical]`\cr
#'   Whether to show points not used in train or test set for the current fold.
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - Vignette [Spatiotemporal Visualization](https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz.html).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @examples
#' \donttest{
#' if (mlr3misc::require_namespaces("sf", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("spcv_tiles",
#'     nsplit = c(4L, 3L), reassign = FALSE)
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task,
#'     fold_id = 1,
#'     show_omitted = TRUE, size = 0.7) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
#' }
autoplot.ResamplingSpCVTiles = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  repeats_id = NULL,
  show_omitted = FALSE,
  ...) {

  resampling = object
  coords = task$coordinates()
  coords$row_id = task$row_ids
  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  resampling = assert_autoplot(resampling, fold_id, task)

  if (is.null(repeats_id)) {
    repeats_id = 1
  } else {
    repeats_id = repeats_id
    # otherwise it gets passed to geom_sf() down below
    # dots$repeats_id = NULL
  }

  resampling_sub = resampling$clone(deep = TRUE)

  if (grepl("Repeated", class(resampling)[1])) {
    resampling_sub$instance = resampling_sub$instance[[repeats_id]]
  }

  if (!is.null(fold_id)) {
    if (length(fold_id) == 1) {
      ### only one fold

      data_coords = prepare_autoplot_cstf(task, resampling_sub)

      # suppress undefined global variables note
      data_coords$indicator = ""

      row_id_test = resampling_sub$instance$test[[fold_id]]
      row_id_train = resampling_sub$instance$train[[fold_id]]

      data_coords[row_id %in% row_id_test, indicator := "Test"]
      data_coords[row_id %in% row_id_train, indicator := "Train"]

      # should omitted points be shown?
      if (show_omitted && nrow(data_coords[indicator == ""]) > 0) {
        data_coords[indicator == "", indicator := "Omitted"]

        sf_df = sf::st_as_sf(data_coords,
          coords = task$extra_args$coordinate_names,
          crs = task$extra_args$crs)
        sf_df = reorder_levels(sf_df)

        ggplot() +
          geom_sf(data = sf_df, aes(color = indicator), ...) +
          scale_color_manual(values = c(
            "Omitted" = "grey",
            "Train" = "#0072B5",
            "Test" = "#E18727"
          )) +
          labs(color = "Set", title = sprintf(
            "Fold %s, Repetition %s", fold_id,
            repeats_id)) +
          theme(
            plot.title = ggtext::element_textbox(
              size = 10,
              color = "black", fill = "#ebebeb", box.color = "black",
              height = unit(0.33, "inch"), width = unit(1, "npc"),
              linetype = 1, r = unit(5, "pt"),
              valign = 0.5, halign = 0.5,
              padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
          )
      } else {
        data_coords = data_coords[indicator != ""]

        sf_df = sf::st_as_sf(data_coords,
          coords = task$extra_args$coordinate_names,
          crs = task$extra_args$crs)
        sf_df = reorder_levels(sf_df)

        ggplot() +
          geom_sf(data = sf_df, aes(color = indicator), ...) +
          scale_color_manual(values = c(
            "Train" = "#0072B5",
            "Test" = "#E18727"
          )) +
          labs(color = "Set", title = sprintf(
            "Fold %s, Repetition %s", fold_id,
            repeats_id)) +
          theme(
            plot.title = ggtext::element_textbox(
              size = 10,
              color = "black", fill = "#ebebeb", box.color = "black",
              height = unit(0.33, "inch"), width = unit(1, "npc"),
              linetype = 1, r = unit(5, "pt"),
              valign = 0.5, halign = 0.5,
              padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
          )
      }
    }
    else {
      ### Multiplot of multiple partitions with train and test set

      # FIXME: redundant code - call function from single plots?
      plot_list = mlr3misc::map(fold_id, function(.x) {

        data_coords = prepare_autoplot_cstf(task, resampling_sub)

        # suppress undefined global variables note
        data_coords$indicator = ""

        row_id_test = resampling_sub$instance$test[[.x]]
        row_id_train = resampling_sub$instance$train[[.x]]

        data_coords[row_id %in% row_id_test, indicator := "Test"]
        data_coords[row_id %in% row_id_train, indicator := "Train"]

        # should omitted points be shown?
        if (show_omitted && nrow(data_coords[indicator == ""]) > 0) {
          data_coords[indicator == "", indicator := "Omitted"]

          sf_df = sf::st_as_sf(data_coords,
            coords = task$extra_args$coordinate_names,
            crs = task$extra_args$crs)
          sf_df = reorder_levels(sf_df)

          ggplot() +
            geom_sf(data = sf_df, aes(color = indicator), ...) +
            scale_color_manual(values = c(
              "Omitted" = "grey",
              "Train" = "#0072B5",
              "Test" = "#E18727"
            )) +
            labs(color = "Set", title = sprintf(
              "Fold %s, Repetition %s", .x,
              repeats_id)) +
            theme(
              plot.title = ggtext::element_textbox(
                size = 10,
                color = "black", fill = "#ebebeb", box.color = "black",
                height = unit(0.33, "inch"), width = unit(1, "npc"),
                linetype = 1, r = unit(5, "pt"),
                valign = 0.5, halign = 0.5,
                padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
            )
        } else {
          data_coords = data_coords[indicator != ""]

          sf_df = sf::st_as_sf(data_coords,
            coords = task$extra_args$coordinate_names,
            crs = task$extra_args$crs)
          sf_df = reorder_levels(sf_df)

          ggplot() +
            geom_sf(data = sf_df, aes(color = indicator), ...) +
            scale_color_manual(values = c(
              "Train" = "#0072B5",
              "Test" = "#E18727"
            )) +
            labs(color = "Set", title = sprintf(
              "Fold %s, Repetition %s", .x,
              repeats_id)) +
            theme(
              plot.title = ggtext::element_textbox(
                size = 10,
                color = "black", fill = "#ebebeb", box.color = "black",
                height = unit(0.33, "inch"), width = unit(1, "npc"),
                linetype = 1, r = unit(5, "pt"),
                valign = 0.5, halign = 0.5,
                padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
            )
        }
      })

      # Return a plot grid via patchwork?

      if (!plot_as_grid) {
        return(invisible(plot_list))
      } else {
        # for repeated cv we also print out the rep number
        if (is.null(repeats_id)) {
          repeats_id = 1 # nocov
        }

        plot_list_pw = patchwork::wrap_plots(plot_list) +
          patchwork::plot_layout(guides = "collect")
        return(plot_list_pw)
      }
    }
  } else {

    ### Create one plot colored by all test folds

    data_coords = prepare_autoplot_cstf(task, resampling_sub)

    names(resampling_sub$instance$test) = seq_len(length(resampling_sub$instance$test))

    # extract test ids from lists
    row_ids_test = data.table::rbindlist(
      lapply(resampling_sub$instance$test, as.data.table),
      idcol = "fold")
    setnames(row_ids_test, c("fold", "row_id"))

    test_folds = merge(data_coords, row_ids_test, by = "row_id", all = TRUE)

    test_folds$fold = as.integer(test_folds$fold)

    sf_df = sf::st_as_sf(test_folds,
      coords = task$extra_args$coordinate_names,
      crs = task$extra_args$crs)

    # only keep test ids
    sf_df = stats::na.omit(sf_df, cols = "fold")

    # order fold ids
    sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
    sf_df$fold = as.factor(as.character(sf_df$fold))
    sf_df$fold = factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

    # for all non-repeated rsmp cases
    if (is.null(repeats_id)) {
      repeats_id = 1 # nocov
    }

    plot = ggplot() +
      geom_sf(
        data = sf_df["fold"], show.legend = "point",
        aes(color = fold)
      ) +
      ggsci::scale_color_ucscgb() +
      labs(color = sprintf("Partition #, Rep %s", repeats_id))
    return(plot)
  }
}

#' @rdname autoplot.ResamplingSpCVTiles
#' @export
autoplot.ResamplingRepeatedSpCVTiles = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  show_omitted = FALSE,
  ...) {

  autoplot.ResamplingSpCVTiles(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    show_omitted = show_omitted,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingSpCVTiles
#' @export
plot.ResamplingSpCVTiles = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingSpCVTiles
#' @export
plot.ResamplingRepeatedSpCVTiles = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}


# CV ---------------------------------------------------------------------------

#' @title Visualization Functions for Non-Spatial CV Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @name autoplot.ResamplingCV
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCV] or
#'   [ResamplingRepeatedCV].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCV] or
#'   [ResamplingRepeatedCV].
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingSptCVCstf()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   resampling = rsmp("cv")
#'   resampling$instantiate(task)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_spatial(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    ... = ...
  )
}

#' @rdname autoplot.ResamplingCV
#' @export
autoplot.ResamplingRepeatedCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  repeats_id = 1,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {

  autoplot.ResamplingCV(
    object = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    train_color = train_color,
    test_color = test_color,
    ... = ...,
    # ellipsis
    repeats_id = repeats_id
  )
}

# Custom CV --------------------------------------------------------------------

#' @title Visualization Functions for Non-Spatial CV Methods.
#'
#' @description Generic S3 `plot()` and `autoplot()` (ggplot2) methods.
#'
#' @name autoplot.ResamplingCustomCV
#' @param object `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCustomCV].
#' @param x `[Resampling]`\cr
#'   mlr3 spatial resampling object of class [ResamplingCustomCV].
#' @inheritParams autoplot.ResamplingSpCVBlock
#' @export
#' @seealso
#'   - mlr3book chapter on on ["Spatiotemporal Visualization"](https://mlr3book.mlr-org.com/spatiotemporal.html#vis-spt-partitions).
#'   - [autoplot.ResamplingSpCVBlock()]
#'   - [autoplot.ResamplingSpCVBuffer()]
#'   - [autoplot.ResamplingSpCVCoords()]
#'   - [autoplot.ResamplingSpCVEnv()]
#'   - [autoplot.ResamplingSpCVDisc()]
#'   - [autoplot.ResamplingSpCVTiles()]
#'   - [autoplot.ResamplingCV()]
#'   - [autoplot.ResamplingSptCVCstf()]
#'   - [autoplot.ResamplingSptCVCluto()]
#' @examples
#' if (mlr3misc::require_namespaces(c("sf", "patchwork"), quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3spatiotempcv)
#'   task = tsk("ecuador")
#'   breaks = quantile(task$data()$dem, seq(0, 1, length = 6))
#'   zclass = cut(task$data()$dem, breaks, include.lowest = TRUE)
#'
#'   resampling = rsmp("custom_cv")
#'   resampling$instantiate(task, f = zclass)
#'
#'   autoplot(resampling, task) +
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#'   autoplot(resampling, task, fold_id = 1)
#'   autoplot(resampling, task, fold_id = c(1, 2)) *
#'     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
#' }
autoplot.ResamplingCustomCV = function( # nolint
  object,
  task,
  fold_id = NULL,
  plot_as_grid = TRUE,
  train_color = "#0072B5",
  test_color = "#E18727",
  ...) {
  autoplot_custom_cv(
    resampling = object,
    task = task,
    fold_id = fold_id,
    plot_as_grid = plot_as_grid,
    ... = ...
  )
}

#' @importFrom graphics plot
#' @rdname autoplot.ResamplingCV
#' @export
plot.ResamplingCV = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingCV
#' @export
plot.ResamplingRepeatedCV = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

#' @rdname autoplot.ResamplingCustomCV
#' @export
plot.ResamplingCustomCV = function(x, ...) {
  print(autoplot(x, ...)) # nocov
}

# autoplot_spatial -------------------------------------------------------------

autoplot_spatial = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = NULL,
  train_color = NULL,
  test_color = NULL,
  show_blocks = FALSE,
  show_labels = FALSE,
  ...) {

  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  # we need to work with a clone, otherwise the object modifications
  # will create issues in future calls usings the same object
  rsmp_autopl = resampling$clone()

  rsmp_autopl = assert_autoplot(rsmp_autopl, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  if (is.null(repeats_id)) {
    repeats_id = 1
  }

  coords_resamp = merge(coords, rsmp_autopl$instance, by = "row_id")

  if (grepl("Repeated", class(rsmp_autopl)[1])) {
    coords_resamp = coords_resamp[rep == repeats_id, ]
  }

  if (!is.null(fold_id)) {

    # Multiplot with train and test set for each fold --------------------------

    plot_list = mlr3misc::map(fold_id, function(.x) {

      dt = coords_resamp
      dt$indicator = rep("foo", nrow(dt))
      dt[, indicator := ifelse(fold == .x, "Test", "Train")]

      sf_df = sf::st_as_sf(dt,
        coords = task$extra_args$coordinate_names,
        crs = task$extra_args$crs)

      sf_df = reorder_levels(sf_df)

      if (show_blocks) {
        if (grepl("Repeated", class(rsmp_autopl)[1])) {
          coords_resamp = coords_resamp[rep == repeats_id, ]
          blocks = rsmp_autopl$blocks[[repeats_id]]
        } else {
          blocks = rsmp_autopl$blocks
        }
        blocks = sf::st_set_crs(blocks, sf::st_crs(sf_df))

        p1 = ggplot() +
          geom_sf(data = sf_df, aes(color = indicator), ...) +
          geom_sf(
            data = blocks, color = "black", alpha = 0,
            size = 0.7) +
          scale_color_manual(values = c(
            "Train" = "#0072B5",
            "Test" = "#E18727"
          )) +
          labs(color = "Set", title = sprintf(
            "Fold %s, Repetition %s", .x,
            repeats_id)) +
          theme(
            plot.title = ggtext::element_textbox(
              size = 10,
              color = "black", fill = "#ebebeb", box.color = "black",
              height = unit(0.33, "inch"), width = unit(1, "npc"),
              linetype = 1, r = unit(5, "pt"),
              valign = 0.5, halign = 0.5,
              padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
          )

        if (show_labels) {
          p1 = p1 +
            geom_sf_label(
              data = blocks, color = "black",
              label = blocks$fold,
              size = 2, label.padding = unit(0.1, "lines"),
              fun.geometry = function(x) {
                # Warning: In st_point_on_surface.sfc(sf::st_zm(x)) :
                # st_point_on_surface may not give correct results for
                # longitude/latitude data
                suppressWarnings(sf::st_point_on_surface(sf::st_zm(x)))
              }
            )
        }
        return(p1)
      } else {

        ggplot() +
          geom_sf(data = sf_df, aes(color = indicator), ...) +
          scale_color_manual(values = c(
            "Train" = "#0072B5",
            "Test" = "#E18727"
          )) +
          labs(color = "Set", title = sprintf(
            "Fold %s, Repetition %s", .x,
            repeats_id)) +
          theme(
            plot.title = ggtext::element_textbox(
              size = 10,
              color = "black", fill = "#ebebeb", box.color = "black",
              height = unit(0.33, "inch"), width = unit(1, "npc"),
              linetype = 1, r = unit(5, "pt"),
              valign = 0.5, halign = 0.5,
              padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
          )
      }
    })

    # Return a plot grid via patchwork? ----------------------------------------

    if (!plot_as_grid) {
      return(invisible(plot_list))
    } else {
      # for repeated cv we also print out the rep number
      if (is.null(repeats_id)) {
        repeats_id = 1 # nocov
      }

      plot_list_pw = patchwork::wrap_plots(plot_list) +
        patchwork::plot_layout(guides = "collect")
      return(plot_list_pw)
    }
  } else {

    # Create one plot colored by all test folds --------------------------------

    if (show_blocks) {
      blocks = sf::st_as_sf(coords_resamp)
    }
    if (!is.null(coords_resamp$blocks)) {
      coords_resamp$blocks = NULL
    }

    sf_df =
      sf::st_as_sf(coords_resamp,
        coords = task$extra_args$coordinate_names,
        crs = task$extra_args$crs)

    # order fold ids
    sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
    sf_df$fold = as.factor(as.character(sf_df$fold))
    sf_df$fold = factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

    # for all non-repeated rsmp cases
    if (is.null(repeats_id)) {
      repeats_id = 1 # nocov
    }

    plot = ggplot() +
      geom_sf(
        data = sf_df["fold"], show.legend = "point",
        aes(color = fold)
      ) +
      ggsci::scale_color_ucscgb() +
      labs(color = sprintf("Partition #, Rep %s", repeats_id))
    return(plot)
  }
}

# autoplot_spatiotemp ----------------------------------------------------------

autoplot_spatiotemp = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = NULL,
  train_color = NULL,
  test_color = NULL,
  tickformat_date = NULL,
  nticks_x = NULL,
  nticks_y = NULL,
  point_size = NULL,
  axis_label_fontsize = NULL,
  ...) {

  mlr3misc::require_namespaces("plotly")
  resampling = assert_autoplot(resampling, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  data = task$data()
  data$row_id = task$row_ids

  coords_resamp = merge(coords, resampling$instance, by = "row_id")
  task_resamp_ids = merge(data, coords_resamp, by = "row_id")

  if (is.null(repeats_id)) {
    repeats_id = 1
  }

  # set some required defaults
  if (grepl("Repeated", class(resampling)[1])) {
    task_resamp_ids = task_resamp_ids[rep == repeats_id, ]
  }

  coords = sf::st_coordinates(sf::st_as_sf(task$coordinates(),
    coords = task$extra_args$coordinate_names,
    crs = task$extra_args$crs))
  task_resamp_ids$x = coords[, 1]
  task_resamp_ids$y = coords[, 2]

  # Create one plot with all (test)-folds
  task_resamp_ids = task_resamp_ids[order(task_resamp_ids$fold, decreasing = FALSE), ]
  task_resamp_ids$fold = as.factor(as.character(task_resamp_ids$fold))
  task_resamp_ids$fold = factor(task_resamp_ids$fold,
    levels = unique(as.character(task_resamp_ids$fold))
  )

  # plot train and test of a specific fold?
  if (!is.null(fold_id)) {

    # suppress undefined global variables note
    indicator = NULL
    fold = NULL

    task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))

    # reorder factor levels so that "train" comes first
    task_resamp_ids$indicator = ordered(task_resamp_ids$indicator,
      levels = c("Train", "Test")
    )

    if (length(fold_id) == 1) {
      # suppress undefined global variables note
      indicator = NULL
      fold = NULL

      task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))
      task_resamp_ids[, indicator := ifelse(fold == fold_id, "Test", "Train")]
      plot_single_plotly = plotly::plot_ly(task_resamp_ids,
        x = ~x, y = ~y, z = ~Date,
        color = ~indicator, colors = c(
          "#0072B5", "#E18727"
        ),
        sizes = c(20, 100)
      )

      plot_single_plotly = plotly::add_markers(plot_single_plotly,
        marker = list(size = point_size))
      plot_single_plotly = plotly::layout(plot_single_plotly,
        title = sprintf("Partition #, Rep %s", repeats_id),
        autosize = TRUE,
        scene = list(
          xaxis = list(title = "Lat", nticks = nticks_x),
          yaxis = list(title = "Lon", nticks = nticks_y),
          zaxis = list(
            title = "Time",
            type = "date",
            tickformat = tickformat_date,
            tickfont = list(size = axis_label_fontsize)
          ),
          camera = list(eye = list(z = 1.50))
        )
      )
      print(plot_single_plotly)
      return(invisible(plot_single_plotly))
    } else {
      # Multiplot across multiple folds with train and test set

      task_resamp_ids$indicator = as.factor(as.character(task_resamp_ids$indicator))
      plot_list = mlr3misc::map(fold_id, function(.x) {

        dt = task_resamp_ids
        dt[, indicator := ifelse(fold == .x, "Test", "Train")]

        dt$Date = as.Date(dt$Date)

        pl = plotly::plot_ly(task_resamp_ids,
          x = ~x, y = ~y, z = ~Date,
          color = ~indicator, colors = c(
            "#0072B5", "#E18727"
          ),
          # this is needed for later when doing 3D subplots
          scene = paste0("scene", .x),
          showlegend = ifelse(.x == 1, TRUE, FALSE)
        )

        # create plots for each fold
        pl = plotly::plot_ly(task_resamp_ids,
          x = ~x, y = ~y, z = ~Date,
          color = ~indicator, colors = c(
            "#0072B5", "#E18727"
          ),
          # this is needed for later when doing 3D subplots
          scene = paste0("scene", .x),
          showlegend = ifelse(.x == 1, TRUE, FALSE)
        )
        pl = plotly::add_markers(pl, marker = list(size = point_size))
        layout_args = list(pl,
          "title" = sprintf("Fold #%s", .x),
          list(
            xaxis = list(
              title = "Lat",
              nticks = nticks_x,
              tickfont = list(size = axis_label_fontsize)),
            yaxis = list(
              title = "Lon",
              nticks = nticks_y,
              tickfont = list(size = axis_label_fontsize)),
            zaxis = list(
              title = "Time",
              type = "date",
              tickformat = tickformat_date,
              tickfont = list(size = axis_label_fontsize)
            ),
            camera = list(eye = list(z = 1.50))
          )
        )
        # -`p` is the name of the plotly object.
        # - title sets the title of the plot
        # - the "scene" name is dynamically generated and refers to the scene
        #   name in the `plot_ly()` call
        names(layout_args) = c(
          "p",
          "title",
          paste0("scene", .x)
        )

        pl = mlr3misc::invoke(plotly::layout, .args = layout_args)
      })
    }

    # is a grid requested?
    if (!plot_as_grid) {
      return(invisible(plot_list))
    } else {
      messagef("Unfortunately plotly does not support a dynamic
       arrangement of multiple subplots.
       See article 'Visualization of spatiotemporal clusters'
       (https://mlr3spatiotempcv.mlr-org.com/articles/spatiotemp-viz) for a
       manual workaround.
       Use the objects in the returned list to arrange a custom grid.",
        wrap = TRUE)

      return(invisible(plot_list))
    }
  } else {
    pl = plotly::plot_ly(task_resamp_ids,
      x = ~x, y = ~y, z = ~Date,
      color = ~fold, colors = ggsci::pal_ucscgb("default")(26),
      sizes = c(20, 100)
    )
    pl = plotly::add_markers(pl, marker = list(size = point_size))
    plotly::layout(pl,
      title = sprintf("Partition #, Rep %s", repeats_id),
      margin = list(
        l = 10,
        r = 10,
        t = 40,
        b = 10
      ),
      scene = list(
        xaxis = list(
          title = "Lat",
          nticks = nticks_x),
        yaxis = list(
          title = "Lon",
          nticks = nticks_y),
        zaxis = list(
          title = "Time",
          type = "date",
          tickformat = tickformat_date,
          tickfont = list(size = axis_label_fontsize)
        ),
        legend = list(
          margin = list(
            l = 5
          )
        ),
        camera = list(eye = list(z = 1.50))
      )
    )
  }
  return(pl)
}

# autoplot_custom_cv -----------------------------------------------------------

autoplot_custom_cv = function(
  resampling = NULL,
  task = NULL,
  fold_id = NULL,
  repeats_id = NULL,
  plot_as_grid = NULL,
  train_color = NULL,
  test_color = NULL,
  ...) {

  mlr3misc::require_namespaces(c("sf", "patchwork", "ggtext"))

  # we need to work with a clone, otherwise the object modifications
  # will create issues in future calls usings the same object
  rsmp_autopl = resampling$clone()

  rsmp_autopl = assert_autoplot(rsmp_autopl, fold_id, task)

  # add the row_ids of the task to the coordinates
  coords = task$coordinates()
  coords$row_id = task$row_ids

  # bring into correct format (complicated alternative to reshape::melt)
  rsmp_autopl$instance = data.table::rbindlist(
    lapply(rsmp_autopl$instance, as.data.table),
    idcol = "fold")
  setnames(rsmp_autopl$instance, c("fold", "row_id"))

  coords_resamp = merge(coords, rsmp_autopl$instance, by = "row_id")
  # we need integers and not characters for the upcoming map() calls
  coords_resamp$fold = as.integer(as.factor(coords_resamp$fold))

  if (!is.null(fold_id)) {

    # Multiplot with train and test set for each fold --------------------------

    plot_list = mlr3misc::map(fold_id, function(.x) {

      dt = coords_resamp
      dt$indicator = rep("foo", nrow(dt))
      dt[, indicator := ifelse(fold == .x, "Test", "Train")]

      sf_df = sf::st_as_sf(dt,
          coords = task$extra_args$coordinate_names,
          crs = task$extra_args$crs)

      sf_df = reorder_levels(sf_df)

      ggplot() +
        geom_sf(data = sf_df, aes(color = indicator), ...) +
        scale_color_manual(values = c(
          "Train" = "#0072B5",
          "Test" = "#E18727"
        )) +
        labs(color = "Set", title = sprintf(
          "Fold %s", .x)) +
        theme(
          plot.title = ggtext::element_textbox(
            size = 10,
            color = "black", fill = "#ebebeb", box.color = "black",
            height = unit(0.33, "inch"), width = unit(1, "npc"),
            linetype = 1, r = unit(5, "pt"),
            valign = 0.5, halign = 0.5,
            padding = margin(2, 2, 2, 2), margin = margin(3, 3, 3, 3))
        )
    })

    # Return a plot grid via patchwork? ----------------------------------------

    if (!plot_as_grid) {
      return(invisible(plot_list))
    } else {

      plot_list_pw = patchwork::wrap_plots(plot_list) +
        patchwork::plot_layout(guides = "collect")
      return(plot_list_pw)
    }
  } else {

    # Create one plot colored by all test folds --------------------------------

    sf_df = sf::st_as_sf(coords_resamp,
        coords = task$extra_args$coordinate_names,
        crs = task$extra_args$crs)

    # order fold ids
    sf_df = sf_df[order(sf_df$fold, decreasing = FALSE), ]
    sf_df$fold = as.factor(as.character(sf_df$fold))
    sf_df$fold = factor(sf_df$fold, levels = unique(as.character(sf_df$fold)))

    plot = ggplot() +
      geom_sf(
        data = sf_df["fold"], show.legend = "point",
        aes(color = fold)
      ) +
      ggsci::scale_color_ucscgb() +
      labs(color = sprintf("Partition #, Rep %s", repeats_id))
    return(plot)
  }
}
