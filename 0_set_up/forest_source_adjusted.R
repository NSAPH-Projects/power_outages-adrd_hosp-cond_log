
#' Create confidence interval grob
#'
#' @inheritParams forest
#' @param pch Numeric or character vector indicating what sort of plotting
#' symbol to use. See \code{\link[grid]{pointsGrob}}.
#' @param gp Graphical parameters of \code{\link[grid]{gpar}}. Please refer
#'  to \code{\link{forest_theme}} for more details.
#' @param t_height The height confidence interval line end vertices. If
#' value is `NULL` (default), no vertices will be drawn.
#' @param name name of the grob.
#' 
#' @return A gTree object
#'
#' @export
makeci <- function(est, lower, upper, pch, sizes = 1, gp = gpar(),
                   t_height = NULL, xlim = c(0, 1), nudge_y = 0,
                   name = NULL){
  
  gTree(
    est = est, lower = lower, upper = upper, pch = pch,
    size = sizes, gp = gp, t_height = t_height,
    xlim = xlim, nudge_y = nudge_y, name = name,
    # vp = viewport(xscale = xlim),
    cl = "makeci"
  )
  
}

#' @export
makeContext.makeci <- function(x) {
  tbvp <- viewport(xscale = x$xlim)
  if (is.null(x$vp))
    x$vp <- tbvp
  else
    x$vp <- vpStack(x$vp, tbvp)
  x
}

#' @export
makeContent.makeci <- function(x) {
  
  kids <- makeci_static(est = x$est, lower = x$lower, upper = x$upper,
                        pch = x$pch, size = x$size, gp =  x$gp,
                        t_height = x$t_height, xlim = x$xlim,
                        nudge_y = x$nudge_y)
  
  setChildren(x, kids)
}

# Main function for confidence interval
#' @keywords internal
makeci_static <- function(est, lower, upper, pch, size = 1, gp = gpar(),
                          t_height = NULL, xlim = c(0, 1), nudge_y = 0){
  
  # Return NULL if the CI is outside
  if(upper < min(xlim) | lower > max(xlim))
    return(gList(nullGrob()))
  
  # Point estimation
  rec_gp <- gp
  rec_gp$col <- gp$fill
  rec <- pointsGrob(x = unit(est, "native"),
                    y = 0.5 + nudge_y,
                    pch = pch,
                    size = unit(size, "char"),
                    gp = rec_gp,
                    name = "point")
  
  # Center indication if alpha is not 1
  if(gp$alpha != 1){
    gp$alpha <- NULL
    cent_gp <- segmentsGrob(x0 = unit(est, "native"), x1 = unit(est, "native"),
                            y0 = unit(0.5 + nudge_y, "npc") - unit(size*.2, "char"),
                            y1 = unit(0.5 + nudge_y, "npc") + unit(size*.2, "char"),
                            gp = gp, name = "center")
    
  }else {
    cent_gp <- nullGrob()
  }    
  
  if(upper > max(xlim) | lower < min(xlim)){
    # Both side arrow
    if(upper > max(xlim) & lower < min(xlim)){
      x_pos <- unit(c(0, 1), c("npc", "npc"))
      arrow_side <- "both"
      x_vert <- NULL
    }
    
    # Left side arrow
    else if(lower < min(xlim) & upper < max(xlim)){
      x_pos <- unit(c(0, upper), c("npc", "native"))
      arrow_side <- "first"
      x_vert <- unit(upper, "native")
    }
    
    # Right side arrow
    else{
      x_pos <- unit(c(lower, 1), c("native", "npc"))
      arrow_side <- "last"
      x_vert <- unit(lower, "native")
    }
    
    lng <- linesGrob(x=x_pos, y = 0.5 + nudge_y,
                     arrow=arrow(length=unit(0.05, "inches"),
                                 ends = arrow_side),
                     gp=gp)
  } else {
    lng <- linesGrob(x=unit(c(lower, upper), "native"), y=0.5 + nudge_y,
                     gp=gp)
    
    x_vert <- unit(c(lower, upper),  "native")
  }
  
  # Draw T end to the CI
  if(!is.null(t_height) & !is.null(x_vert)){
    if(!is.unit(t_height))
      t_height <- unit(t_height, "npc")
    
    vert <- segmentsGrob(x0 = x_vert, y0 = unit(0.5 + nudge_y, "npc") - t_height/2,
                         x1 = x_vert, y1 = unit(0.5 + nudge_y, "npc") + t_height/2,
                         gp = gp,
                         name = "T.end")
  }else {
    vert <- nullGrob()
  }
  
  # No dots if outside
  if(est > max(xlim) | est < min(xlim))
    rec <- nullGrob()
  
  gList(cent_gp, lng, vert, rec)
  
}


#' Create pooled summary diamond shape
#'
#' @inheritParams forest
#' @param gp Graphical parameters of \code{\link[grid]{gpar}}.
#'  Please refer to \code{\link{forest_theme}} for more details.
#' 
#' @return A gTree object
#'
#' @export
make_summary <- function(est, lower, upper, sizes = 1, gp, xlim, nudge_y = NULL){
  
  # Return NULL if the CI is outside
  if(upper < min(xlim) | lower > max(xlim))
    return(NULL)
  
  polygonGrob(x = unit(c(lower, est, upper, est), "native"),
              y = unit(0.5 + c(0, 0.5 * sizes, 0, -0.5*sizes) + nudge_y*2, "npc"),
              gp = gp,
              vp = viewport(xscale = xlim),
              name = "pooled.diamond")
}



# Forest ------------------------------------------------------------------



forest_source_adjusted <- function(data,
                   est,
                   lower,
                   upper,
                   sizes,
                   ref_line = ifelse(x_trans %in% c("log", "log2", "log10"), 1, 0),
                   vert_line = NULL,
                   ci_column,
                   is_summary = NULL,
                   xlim = NULL,
                   ticks_at = NULL,
                   ticks_digits = NULL,
                   ticks_minor = NULL,
                   arrow_lab = NULL,
                   x_trans = "none",
                   xlab = NULL,
                   footnote = NULL,
                   title = NULL,
                   nudge_y = 0,
                   fn_ci = makeci,
                   fn_summary = make_summary,
                   index_args = NULL,
                   theme = NULL,
                   ...){
  
  dot_args <- list(...)
  
  # Check arguments
  args_ci <- names(formals(fn_ci))
  if(!all(c("est", "lower", "upper", "sizes", "xlim", "pch", "gp", "t_height", "nudge_y") %in% args_ci))
    stop("arguments \"est\", \"lower\", \"upper\", \"sizes\", \"xlim\", \"pch\", \"gp\", \"t_height\" and \"nudge_y\" must be provided in the function `fn_ci`.")
  
  args_summary <- names(formals(fn_summary))
  if(any(unlist(is_summary))){
    if(!all(c("est", "lower", "upper", "sizes", "xlim", "gp", "nudge_y") %in% args_summary))
      stop("arguments \"est\", \"lower\", \"upper\", \"sizes\", \"nudge_y\", \"xlim\",and \"gp\" must be provided in the function `fn_summary`.")
  }
  
  check_errors(data = data, est = est, lower = lower, upper = upper, sizes = sizes,
               ref_line = ref_line, vert_line = vert_line, ci_column = ci_column,
               is_summary = is_summary, xlim = xlim, ticks_at = ticks_at,
               ticks_digits = ticks_digits, arrow_lab = arrow_lab, xlab = xlab,
               title = title, x_trans = x_trans, ticks_minor = ticks_minor)
  
  # Set theme
  if(is.null(theme)){
    theme <- forest_theme()
  }
  
  # For multiple ci_column
  if(length(ref_line) == 1)
    ref_line <- rep(ref_line, length(ci_column))
  
  if(!is.null(vert_line) && !inherits(vert_line, "list"))
    vert_line <- rep(list(vert_line), length(ci_column))
  
  if(length(x_trans) == 1)
    x_trans <- rep(x_trans, length(ci_column))
  
  if(!is.null(xlim) && !inherits(xlim, "list"))
    xlim <- rep(list(xlim), length(ci_column))
  
  if(is.null(ticks_at)){
    ticks_at <- ticks_minor
  }else{
    if(!inherits(ticks_at, "list"))
      ticks_at <- rep(list(ticks_at), length(ci_column))
  }
  
  if(is.null(ticks_minor)){
    ticks_minor <- ticks_at
  }else{
    if(!inherits(ticks_minor, "list"))
      ticks_minor <- rep(list(ticks_minor), length(ci_column))
  }
  
  if(!is.null(arrow_lab) && !inherits(arrow_lab, "list"))
    arrow_lab <- rep(list(arrow_lab), length(ci_column))
  
  if(length(xlab) == 1)
    xlab <- rep(xlab, length(ci_column))
  
  # Replicate sizes
  if(inherits(est, "list") & length(sizes) == 1)
    sizes <- rapply(est, function(x) ifelse(is.na(x), NA, sizes), how = "replace")
  
  if(is.atomic(est)){
    est <- list(est)
    lower <- list(lower)
    upper <- list(upper)
    if(length(sizes) == 1)
      sizes <- rep(sizes, nrow(data))
    sizes <- list(sizes)
  }
  
  # Check index_var
  if(!is.null(index_args)){
    for(ind_v in index_args){
      if(!is.list(dot_args[[ind_v]]))
        dot_args[[ind_v]] <- list(dot_args[[ind_v]])
      
      est_len <- vapply(est, length, FUN.VALUE = 1L)
      arg_len <- vapply(dot_args[[ind_v]], length, FUN.VALUE = 1L)
      if(length(dot_args[[ind_v]]) != length(est) || length(unique(c(est_len, arg_len))) != 1)
        stop("index_args should have the same length as est.")
    }
  }
  
  # Calculate group number
  group_num <- length(est)/length(ci_column)
  ci_col_list <- rep(ci_column, group_num)
  
  theme <- make_group_theme(theme = theme, group_num = group_num)
  
  # Get color and pch
  color_list <- rep(theme$ci$col, each = length(ci_column))
  fill_list <- rep(theme$ci$fill, each = length(ci_column))
  alpha_list <- rep(theme$ci$alpha, each = length(ci_column))
  pch_list <- rep(theme$ci$pch, each = length(ci_column))
  lty_list <- rep(theme$ci$lty, each = length(ci_column))
  lwd_list <- rep(theme$ci$lwd, each = length(ci_column))
  
  # Positions of values in ci_column
  gp_list <- rep_len(1:(length(lower)/group_num), length(lower))
  
  # Check nudge_y
  if(nudge_y >= 1 || nudge_y < 0)
    stop("`nudge_y` must be within 0 to 1.")
  
  # Check nudge_y
  if(group_num > 1 & nudge_y == 0)
    nudge_y <- 0.1
  
  # Create nudge_y vector
  if(group_num > 1){
    if((group_num %% 2) == 0){
      rep_tm <- cumsum(c(nudge_y/2, rep(nudge_y, group_num)))
      nudge_y <- c(rep_tm[1:(group_num/2)], -rep_tm[1:(group_num/2)])
    }else{
      rep_tm <- cumsum(c(0, rep(nudge_y, group_num %/% 2)))
      nudge_y <- unique(c(rep_tm, - rep_tm))
    }
    
    nudge_y <- sort(nudge_y, decreasing = TRUE)
    
  }
  
  nudge_y <- rep(nudge_y, each = length(ci_column))
  
  
  if(is.null(is_summary)){
    is_summary <- rep(FALSE, nrow(data))
  }
  
  # Check exponential
  if(any(x_trans %in% c("log", "log2", "log10"))){
    for(i in seq_along(x_trans)){
      if(x_trans[i] %in% c("log", "log2", "log10")){
        sel_num <- gp_list == i
        checks_ill <- c(any(unlist(est[sel_num]) <= 0, na.rm = TRUE),
                        any(unlist(lower[sel_num]) <= 0, na.rm = TRUE),
                        any(unlist(upper[sel_num]) <= 0, na.rm = TRUE),
                        (any(ref_line[i] <= 0)),
                        (!is.null(vert_line) && any(unlist(vert_line[[i]]) <= 0, na.rm = TRUE)),
                        (!is.null(xlim) && any(unlist(xlim[[i]]) < 0)))
        zeros <- c("est", "lower", "upper", "ref_line", "vert_line", "xlim")
        if (any(checks_ill)) {
          message("found values equal or less than 0 in ", zeros[checks_ill])
          stop("est, lower, upper, ref_line, vert_line and xlim should be larger than 0, if `x_trans` in \"log\", \"log2\", \"log10\".")
        }
      }
    }
  }
  
  # ticks digits auto calculation if missing
  if(is.null(ticks_digits) & !is.null(ticks_at)){
    if(is.list(ticks_at))
      ticks_digits <- sapply(ticks_at, function(x){
        max(count_decimal(x))
      })
    else
      ticks_digits <- max(count_decimal(ticks_digits))
    
    ticks_digits <- as.integer(ticks_digits)
  }
  
  # Set xlim to minimum and maximum value of the CI
  xlim <- lapply(seq_along(ci_column), function(i){
    sel_num <- gp_list == i
    make_xlim(xlim = xlim[[i]],
              lower = lower[sel_num],
              upper = upper[sel_num],
              ref_line = ref_line[i],
              ticks_at = c(ticks_at[[i]], ticks_minor[[i]]),
              x_trans = x_trans[i])
  })
  
  # Set X-axis breaks if missing
  ticks_at <- lapply(seq_along(xlim), function(i){
    make_ticks(at = ticks_at[[i]],
               xlim = xlim[[i]],
               refline = ref_line[i],
               x_trans = x_trans[i])
  })
  
  ticks_minor <- lapply(seq_along(xlim), function(i){
    make_ticks(at = ticks_minor[[i]],
               xlim = xlim[[i]],
               refline = ref_line[i],
               x_trans = x_trans[i])
  })
  
  # ticks digits auto calculation if missing
  if(is.null(ticks_digits)){
    if(is.list(ticks_at))
      ticks_digits <- sapply(ticks_at, function(x){
        count_zeros(x)
      }, USE.NAMES = FALSE)
    else{
      ticks_digits <- count_zeros(ticks_at)
    }
    ticks_digits <- as.integer(ticks_digits)
  }
  
  if(length(ci_column) != length(ticks_digits))
    ticks_digits <- rep(ticks_digits, length(ci_column))
  
  gt <- tableGrob(data, theme = theme$tab_theme, rows = NULL)
  
  if(group_num > 1 & any(is_summary)){
    gt$heights[c(FALSE, is_summary)] <- gt$heights[c(FALSE, is_summary)]*2
  }
  
  
  # Do not clip text
  gt$layout$clip <- "off"
  
  # Column index
  col_indx <- rep_len(1:length(ci_column), length(ci_col_list))
  
  # Draw CI
  for(col_num in seq_along(ci_col_list)){
    
    # Get current CI column and group number
    current_col <- ci_col_list[col_num]
    current_gp <- sum(col_indx[1:col_num] == col_indx[col_num])
    
    # Convert value is exponentiated
    col_trans <- x_trans[col_indx[col_num]]
    if(col_trans != "none"){
      est[[col_num]] <- xscale(est[[col_num]], col_trans)
      lower[[col_num]] <- xscale(lower[[col_num]], col_trans)
      upper[[col_num]] <- xscale(upper[[col_num]], col_trans)
      
      # Transform other indexing arguments
      if(!is.null(index_args)){
        for(ind_v in index_args){
          if(any(unlist(dot_args[[ind_v]][[col_num]]) <= 0, na.rm = TRUE) & col_trans %in% c("log", "log2", "log10"))
            stop(ind_v, " should be larger than 0, if `x_trans` in \"log\", \"log2\", \"log10\".")
          dot_args[[ind_v]][[col_num]] <- xscale(dot_args[[ind_v]][[col_num]], col_trans)
        }
      }
    }
    
    for(i in 1:nrow(data)){
      if(is.na(est[[col_num]][i]))
        next
      
      if(is.na(lower[[col_num]][i]) || is.na(upper[[col_num]][i])){
        warning("Missing lower and/or upper limit on column", current_col, " row ", i)
        next
      }
      
      dot_pass <- dot_args
      if(!is.null(index_args)){
        for(ind_v in index_args){
          dot_pass[[ind_v]] <- dot_pass[[ind_v]][[col_num]][i]
        }
      }
      
      if(is_summary[i]){
        # Update graphical parameters
        g_par <- gpar(col = theme$summary$col[current_gp],
                      fill = theme$summary$fill[current_gp])
        if ("gp" %in% names(dot_pass)) {
          g_par <- modifyList(list(dot_pass$gp), g_par)
          dot_pass$gp <- NULL
        }
        
        dot_pass <- dot_pass[names(dot_pass) %in% args_summary]
        
        draw_ci <- do.call(fn_summary, c(
          list(est = est[[col_num]][i],
               lower = lower[[col_num]][i],
               upper = upper[[col_num]][i],
               sizes = sizes[[col_num]][i],
               xlim = xlim[[col_indx[col_num]]],
               gp = g_par,
               nudge_y = nudge_y[col_num]),
          dot_pass
        ))
        
      }else {
        # Update graphical parameters
        g_par <- gpar(lty = lty_list[col_num],
                      lwd = lwd_list[col_num],
                      col = color_list[col_num],
                      fill = fill_list[col_num],
                      alpha = alpha_list[col_num])
        
        if ("gp" %in% names(dot_pass)) {
          g_par <- modifyList(dot_pass$gp, g_par)
          dot_pass$gp <- NULL
        }
        
        dot_pass <- dot_pass[names(dot_pass) %in% args_ci]
        
        draw_ci <- do.call(fn_ci, c(
          list(est = est[[col_num]][i],
               lower = lower[[col_num]][i],
               upper = upper[[col_num]][i],
               sizes = sizes[[col_num]][i],
               xlim = xlim[[col_indx[col_num]]],
               pch = pch_list[col_num],
               gp = g_par,
               t_height = theme$ci$t_height,
               nudge_y = nudge_y[col_num]),
          dot_pass
        ))
      }
      
      # Skip if CI is outside xlim
      if(upper[[col_num]][i] < min(xlim[[col_indx[col_num]]]) | lower[[col_num]][i] > max(xlim[[col_indx[col_num]]])){
        message("The confidence interval of row ", i, ", column ", current_col, ", group ", current_gp,
                " is outside of the xlim.")
        next
      }
      
      
      gt <- gtable_add_grob(gt, draw_ci,
                            t = i + 1,
                            l = current_col,
                            b = i + 1,
                            r = current_col,
                            clip = "off",
                            name = paste0("ci-", i, "-", current_col, "-", current_gp))
    }
  }
  
  tot_row <- nrow(gt)
  
  # Prepare X axis
  x_axis <- lapply(seq_along(xlim), function(i){
    make_xaxis(at = ticks_at[[i]],
               at_minor = ticks_minor[[i]],
               gp = theme$xaxis,
               xlab_gp = theme$xlab,
               ticks_digits = ticks_digits[[i]],
               x0 = ref_line[i],
               xlim = xlim[[i]],
               xlab = xlab[i],
               x_trans = x_trans[i])
  })
  
  x_axht <- sapply(x_axis, function(x){
    ht <- Reduce(`+`, lapply(x$children, grobHeight))
    convertHeight(ht, unitTo = "mm", valueOnly = TRUE)
  })
  
  gt <- gtable_add_rows(gt, heights = unit(max(x_axht), "mm") + unit(.8, "lines"))
  
  # Prepare arrow object and row to put it
  if(!is.null(arrow_lab)){
    arrow_grob <- lapply(seq_along(xlim), function(i){
      make_arrow(x0 = ref_line[i],
                 arrow_lab = arrow_lab[[i]],
                 arrow_gp = theme$arrow,
                 x_trans = x_trans[i],
                 col_width = convertWidth(gt$widths[ci_column[i]], "char", valueOnly = TRUE),
                 xlim = xlim[[i]])
    })
    
    lb_ht <- sapply(arrow_grob, function(x){
      ht <- Reduce(`+`, lapply(x$children, heightDetails))
      convertHeight(ht, unitTo = "mm", valueOnly = TRUE)
    })
    
    gt <- gtable_add_rows(gt, heights = unit(max(lb_ht), "mm"))
    
  }
  
  # Add footnote
  if(!is.null(footnote)){
    footnote_grob <- textGrob(label = footnote,
                              gp = theme$footnote,
                              x = 0,
                              y = .8,
                              just = "left",
                              check.overlap = TRUE,
                              name = "footnote")
    
    gt <- gtable_add_grob(gt,
                          footnote_grob,
                          t = tot_row + 1,
                          l = 1,
                          b = nrow(gt), r = min(ci_column),
                          clip = "off",
                          name = "footnote")
  }
  
  for(j in ci_column){
    idx <- which(ci_column == j)
    # Add reference line
    gt <- gtable_add_grob(gt,
                          vert_line(x = ref_line[idx],
                                    gp = theme$refline,
                                    xlim = xlim[[idx]],
                                    x_trans = x_trans[idx]),
                          t = 2,
                          l = j,
                          b = tot_row, r = j,
                          # Make sure reference line is below the whisker
                          z = max(gt$layout$z[grepl("core-", gt$layout$name)]),
                          clip = "off",
                          name = paste0("ref.line-", j))
    
    # Add the X-axis
    gt <- gtable_add_grob(gt, x_axis[[idx]],
                          t = tot_row + 1,
                          l = j,
                          b = tot_row + 1, r = j,
                          clip = "off",
                          name = paste0("xaxis-", j))
    
    # Add vertical line
    if(!is.null(vert_line))
      gt <- gtable_add_grob(gt,
                            vert_line(x = vert_line[[idx]],
                                      gp = theme$vertline,
                                      xlim = xlim[[idx]],
                                      x_trans = x_trans[idx]),
                            t = 2,
                            l = j,
                            b = tot_row, r = j,
                            z = max(gt$layout$z[grepl("core-", gt$layout$name)]),
                            clip = "off",
                            name = paste0("vert.line-", j))
    
    # Add arrow
    if(!is.null(arrow_lab))
      gt <- gtable_add_grob(gt, arrow_grob[[idx]],
                            t = nrow(gt), l = j,
                            b = nrow(gt), r = j,
                            clip = "off",
                            name = paste0("arrow-", j))
    
  }
  
  # Add legend
  if(group_num > 1 & theme$legend$position != "none"){
    
    by_row <- !theme$legend$position %in% c("top", "bottom")
    
    legend <- theme$legend
    legend$pch <- theme$ci$pch
    legend$gp$col <- theme$ci$col
    legend$gp$lty <- theme$ci$lty
    legend$gp$fill <- theme$ci$fill
    
    
    leg_grob <- do.call(legend_grob, legend)
    
    if(by_row){
      gt <- gtable_add_cols(gt, widths = max(grobWidth(leg_grob$children)) + unit(.5, "lines"))
      gt <- gtable_add_grob(gt, leg_grob,
                            t = 2, l = ncol(gt),
                            b = nrow(gt)-1, r = ncol(gt),
                            clip = "off",
                            name = "legend")
    }else{
      add_pos <- ifelse(legend$position == "top", 0, -1)
      gt <- gtable_add_rows(gt, heights = max(grobHeight(leg_grob$children)) + unit(.5, "lines"), pos = add_pos)
      gt <- gtable_add_grob(gt, leg_grob,
                            t = if(add_pos == 0) 1 else nrow(gt), l = 1,
                            b = if(add_pos == 0) 1 else nrow(gt), r = ncol(gt),
                            clip = "off",
                            name = "legend")
    }
  }
  
  if(!is.null(title)){
    max_height <- max(convertHeight(stringHeight(title), "mm", valueOnly = TRUE))
    gt <- gtable_add_rows(gt, unit(max_height, "mm") + unit(2, "mm"), pos = 0)
    title_x <- switch(theme$title$just,
                      right = unit(1, "npc"),
                      left  = unit(0, "npc"),
                      center = unit(.5, "npc"))
    title_gb <- textGrob(label = title,
                         gp = theme$title$gp,
                         x = title_x,
                         just = theme$title$just,
                         check.overlap = TRUE,
                         name = "plot.title")
    
    gt <- gtable_add_grob(gt, title_gb,
                          t = 1,
                          b = 1,
                          l = 1,
                          r = ncol(gt),
                          clip = "off",
                          name = "plot.title")
  }
  
  # Add padding
  gt <- gtable_add_padding(gt, unit(5, "mm"))
  
  # Auto fit the page
  # gt$widths <- unit(rep(1/ncol(gt), ncol(gt)), "npc")
  # gt$heights <- unit(rep(1/nrow(gt), nrow(gt)), "npc")
  
  class(gt) <- union("forestplot", class(gt))
  
  return(gt)
  
}


#' Draw plot
#'
#' Print or draw forestplot.
#'
#' @param x forestplot to display
#' @param autofit If true, the plot will be autofit.
#' @param ... other arguments not used by this method
#' @return Invisibly returns the original forestplot.
#' @rdname print.forestplot
#' @method print forestplot
#' @export
print.forestplot <- function(x, autofit = FALSE, ...){
  
  if(autofit){
    # Auto fit the page
    x$widths <- unit(rep(1/ncol(x), ncol(x)), "npc")
    x$heights <- unit(rep(1/nrow(x), nrow(x)), "npc")
  }
  
  grid.newpage()
  grid.draw(x)
  
  invisible(x)
}

#' @method plot forestplot
#' @rdname print.forestplot
#' @export
plot.forestplot <- print.forestplot


