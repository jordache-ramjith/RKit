# All emitted code is assembled from fixed templates and quoted values.
# No user-supplied expression is evaluated.
r_string <- function(x) paste(capture.output(dput(as.character(x))), collapse = "\n")
r_num <- function(x) {
  if (length(x) != 1 || !is.finite(x)) stop("Enter a finite number.")
  format(x, scientific = FALSE, digits = 16, trim = TRUE)
}
col_code <- function(x) paste0('.data[[', r_string(x), ']]')
nonempty <- function(x) !is.null(x) && length(x) > 0 && nzchar(x[[1]])

read_source <- function(path, meta) {
  if (meta$kind == "xlsx") {
    d <- readxl::read_excel(path, sheet = meta$sheet, na = meta$na, .name_repair = "unique")
  } else {
    d <- readr::read_delim(path, delim = meta$delim, na = meta$na,
      locale = readr::locale(decimal_mark = meta$decimal, encoding = meta$encoding),
      show_col_types = FALSE, name_repair = "unique", trim_ws = TRUE)
    if (nrow(readr::problems(d))) stop("Some rows could not be read consistently. Check the separator, decimal mark and file contents.")
  }
  d <- as.data.frame(d)
  if (!nrow(d) || !ncol(d)) stop("This file has no data rows or columns.")
  if (nrow(d) > 100000 || ncol(d) > 200) stop("For this teaching version, use at most 100,000 rows and 200 columns.")
  if (any(vapply(d, function(x) is.numeric(x) && any(!is.finite(x) & !is.na(x)), logical(1))))
    stop("The file contains infinite numeric values. Replace or correct them in the source file before importing.")
  d
}
import_code <- function(meta) {
  path <- paste0("data/", meta$name)
  if (meta$kind == "xlsx") {
    paste0("raw_data <- readxl::read_excel(", r_string(path), ", sheet = ", r_string(meta$sheet),
      ", na = ", r_string(meta$na), ", .name_repair = \"unique\")")
  } else {
    paste0("raw_data <- readr::read_delim(\n  ", r_string(path), ", delim = ", r_string(meta$delim),
      ", na = ", r_string(meta$na), ",\n  locale = readr::locale(decimal_mark = ", r_string(meta$decimal),
      ", encoding = ", r_string(meta$encoding), "),\n  trim_ws = TRUE, name_repair = \"unique\", show_col_types = FALSE\n)")
  }
}
filter_expr <- function(rule) {
  c <- col_code(rule$column)
  if (rule$operator == "missing") return(paste0("is.na(", c, ")"))
  if (rule$operator == "present") return(paste0("!is.na(", c, ")"))
  rhs <- if (rule$numeric) r_num(as.numeric(rule$value)) else r_string(rule$value)
  op <- switch(rule$operator, eq = "==", ne = "!=", gt = ">", ge = ">=", lt = "<", le = "<=", stop("Choose a filter comparison."))
  paste0("(!is.na(", c, ") & ", c, " ", op, " ", rhs, ")")
}
step_code <- function(step) {
  c <- col_code(step$column)
  if (step$kind == "filter") {
    join <- if (step$join == "all") " & " else " | "
    return(paste0("study <- study |>\n  dplyr::filter(", paste(vapply(step$rules, filter_expr, character(1)), collapse = join), ")"))
  }
  if (step$kind == "type") {
    rhs <- switch(step$type, categorical = paste0("factor(", c, ")"), numerical = paste0("as.numeric(as.character(", c, "))"),
      text = paste0("as.character(", c, ")"))
    return(paste0("study <- study |>\n  dplyr::mutate(!!", r_string(step$column), " := ", rhs, ")"))
  }
  rhs <- switch(step$operation,
    log = paste0("log(", c, ")"),
    scale = paste0(c, " * ", r_num(step$number)),
    add = paste0(c, " + ", r_num(step$number)),
    subtract = paste0(c, " - ", col_code(step$other)),
    ratio = paste0(c, " / ", col_code(step$other)),
    threshold = paste0("factor(dplyr::if_else(is.na(", c, "), NA_character_,\n    dplyr::if_else(", c, " >= ", r_num(step$number), ", ", r_string(step$above), ", ", r_string(step$below), ")))") ,
    recode = paste0("dplyr::if_else(as.character(", c, ") == ", r_string(step$old), ", ", r_string(step$new), ", as.character(", c, "))"),
    stop("Choose a supported operation."))
  paste0("study <- study |>\n  dplyr::mutate(!!", r_string(step$name), " := ", rhs, ")")
}
apply_steps <- function(raw, steps) {
  env <- new.env(parent = globalenv())
  env$study <- raw
  for (s in steps) eval(parse(text = step_code(s)), env)
  as.data.frame(env$study)
}
check_step <- function(d, step) {
  if (step$kind == "type" && step$type == "numerical") {
    v <- d[[step$column]]
    z <- suppressWarnings(as.numeric(as.character(v)))
    if (any(!is.na(v) & (is.na(z) | !is.finite(z)))) stop("Some values are not numbers. Recode them first; this change would lose information.")
  }
  if (step$kind == "derive") {
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", step$name)) stop("Start the new name with a letter and use only letters, numbers or underscores.")
    if (step$name %in% names(d)) stop("That name already exists. Choose a new name so the original column is preserved.")
    if (step$operation != "recode" && !is.numeric(d[[step$column]])) stop("Choose a numerical starting variable.")
    if (step$operation == "log" && any(d[[step$column]] <= 0, na.rm = TRUE)) stop("Log needs positive values. This column contains zero or negative values; none have been removed.")
    if (step$operation %in% c("ratio", "subtract") && !is.numeric(d[[step$other]])) stop("The second variable must be numerical.")
    if (step$operation == "ratio" && any(d[[step$other]] == 0, na.rm = TRUE)) stop("The denominator contains zero. Division by zero is not allowed.")
    if (step$operation == "threshold" && (!nzchar(step$above) || !nzchar(step$below) || step$above == step$below)) stop("Use two distinct, non-empty group labels.")
    if (step$operation == "recode" && !step$old %in% as.character(d[[step$column]])) stop("The value to replace is not present in this column.")
  }
  candidate <- apply_steps(d, list(step))
  if (!nrow(candidate)) stop("This filter would leave no rows. Adjust the condition before applying it.")
  if (step$kind == "derive" && is.numeric(candidate[[step$name]]) && any(!is.finite(candidate[[step$name]]) & !is.na(candidate[[step$name]]))) stop("This calculation produces infinite values. Check your inputs.")
  invisible(TRUE)
}

summary_code <- function(group_cols = character()) {
  groups <- if (length(group_cols)) paste0("\n  dplyr::group_by(dplyr::across(dplyr::all_of(", r_string(group_cols), "))) |>") else ""
  paste0("summary_table <- analysis_data |>", groups, "\n  dplyr::summarise(\n",
    "    rows = dplyr::n(), n = sum(!is.na(.data$value)), missing = sum(is.na(.data$value)),\n",
    "    mean = if (n > 0) mean(.data$value, na.rm = TRUE) else NA_real_,\n",
    "    sd = if (n > 1) sd(.data$value, na.rm = TRUE) else NA_real_,\n",
    "    variance = if (n > 1) var(.data$value, na.rm = TRUE) else NA_real_,\n",
    "    median = if (n > 0) median(.data$value, na.rm = TRUE) else NA_real_,\n",
    "    q1 = if (n > 0) as.numeric(quantile(.data$value, .25, na.rm = TRUE)) else NA_real_,\n",
    "    q3 = if (n > 0) as.numeric(quantile(.data$value, .75, na.rm = TRUE)) else NA_real_,\n",
    "    IQR = q3 - q1,\n",
    "    min = if (n > 0) min(.data$value, na.rm = TRUE) else NA_real_,\n",
    "    max = if (n > 0) max(.data$value, na.rm = TRUE) else NA_real_, .groups = \"drop\"\n  )")
}
plot_theme_code <- paste0('plot_theme <- ggplot2::theme_minimal(base_size = 12) +\n',
 '  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),\n',
 '    plot.title = ggplot2::element_text(face = "bold", size = 13),\n    strip.text = ggplot2::element_text(size = 11), legend.position = "bottom")')

analysis_code <- function(s) {
  x <- col_code(s$x); y <- col_code(s$y); z <- col_code(s$z)
  plots <- character()
  columns <- if (is.null(s$facet_columns)) 2L else max(1L, min(2L, as.integer(s$facet_columns)))
  facet <- paste0('  facet_wrap(~stratum, ncol = ', columns, ', scales = "fixed", labeller = labeller(stratum = function(x) wrap_labels(x, 22))) +\n')
  if (s$route == "num") {
    prep <- paste0('analysis_data <- study |> dplyr::transmute(value = ', x, ')')
    summ <- summary_code()
    plots <- c(paste0('plot_main <- ggplot(analysis_data, aes(x = .data$value)) +\n  geom_histogram(bins = ', s$bins, ', fill = "#087F8C", colour = "white", na.rm = TRUE) +\n  labs(x = ', r_string(s$x), ', y = "Observations", title = "How are the values distributed?") + plot_theme'),
      paste0('plot_extra <- ggplot(analysis_data, aes(x = "", y = .data$value)) +\n  geom_boxplot(width = .3, fill = "#B8DFD9", na.rm = TRUE) +\n  labs(x = NULL, y = ', r_string(s$x), ', title = "Centre, spread and unusual values") + plot_theme'))
  } else if (s$route == "cat") {
    prep <- paste0('analysis_data <- study |> dplyr::transmute(category = as.character(', x, '))\n',
      'missing_count <- sum(is.na(analysis_data$category))\n',
      'analysis_data <- analysis_data |> dplyr::filter(!is.na(.data$category))')
    summ <- 'summary_table <- analysis_data |>\n  dplyr::count(.data$category, name = "n") |>\n  dplyr::mutate(percent = 100 * .data$n / sum(.data$n))'
    plots <- paste0('plot_main <- ggplot(summary_table, aes(x = .data$category, y = .data$', if (s$measure == 'percent') 'percent' else 'n', ')) +\n  geom_col(fill = "#087F8C", width = .65) +\n  labs(x = ', r_string(s$x), ', y = ', r_string(if(s$measure == 'percent') 'Percent of non-missing observations' else 'Count'), ', title = "How common is each category?") + plot_theme')
  } else if (s$route == "group") {
    extra <- if (nonempty(s$z)) paste0(', stratum = factor(', z, ')') else ''
    prep <- paste0('analysis_data <- study |> dplyr::transmute(value = ', x, ', group = factor(', y, ')', extra, ')\n',
      'analysis_data <- analysis_data |> dplyr::filter(!is.na(.data$group)', if (nonempty(s$z)) ', !is.na(.data$stratum)' else '', ')')
    summ <- summary_code(c('group', if (nonempty(s$z)) 'stratum'))
    plots <- paste0('plot_main <- ggplot(analysis_data, aes(x = .data$group, y = .data$value)) +\n  geom_boxplot(fill = "#B8DFD9", width = .5, outlier.shape = NA, na.rm = TRUE) +\n  geom_point(position = position_jitter(width = .12, height = 0, seed = 42), alpha = .4, colour = "#087F8C", na.rm = TRUE) +\n',
      if (nonempty(s$z)) facet else '',
      '  labs(x = ', r_string(s$y), ', y = ', r_string(s$x), ', title = "Compare distributions, not just averages") + plot_theme')
  } else if (s$route == "cat2") {
    prep <- paste0('analysis_data <- study |> dplyr::transmute(row_group = factor(', x, '), column_group = factor(', y, ')) |>\n  dplyr::filter(!is.na(.data$row_group), !is.na(.data$column_group))')
    denom <- switch(s$denom,
      row = 'dplyr::group_by(.data$row_group) |>\n  dplyr::mutate(percent = 100 * .data$n / sum(.data$n)) |> dplyr::ungroup()',
      column = 'dplyr::group_by(.data$column_group) |>\n  dplyr::mutate(percent = 100 * .data$n / sum(.data$n)) |> dplyr::ungroup()',
      'dplyr::mutate(percent = 100 * .data$n / sum(.data$n))')
    summ <- paste0('summary_table <- analysis_data |>\n  dplyr::count(.data$row_group, .data$column_group, .drop = FALSE, name = "n") |>\n  ', denom)
    plots <- paste0('plot_main <- ggplot(summary_table, aes(x = .data$row_group, y = .data$percent, fill = .data$column_group)) +\n  geom_col(position = "dodge", width = .7) +\n  labs(x = ', r_string(s$x), ', fill = ', r_string(s$y), ', y = ', r_string(switch(s$denom,row='Percent within each row group',column='Percent within each column group',all='Percent of all complete observations')), ', title = "Which categories occur together?") + plot_theme')
  } else {
    extra <- if (nonempty(s$z)) paste0(', stratum = factor(', z, ')') else ''
    prep <- paste0('analysis_data <- study |> dplyr::transmute(x = ', x, ', y = ', y, extra, ') |>\n  dplyr::filter(!is.na(.data$x), !is.na(.data$y)', if(nonempty(s$z)) ', !is.na(.data$stratum)' else '', ')')
    summ <- paste0('summary_table <- analysis_data |>', if(nonempty(s$z)) '\n  dplyr::group_by(.data$stratum) |>' else '',
      '\n  dplyr::summarise(n = dplyr::n(),\n    correlation = if (dplyr::n() >= 3 && sd(.data$x) > 0 && sd(.data$y) > 0)\n      cor(.data$x, .data$y, method = ', r_string(s$cor), ') else NA_real_, .groups = "drop")')
    plots <- paste0('plot_main <- ggplot(analysis_data, aes(x = .data$x, y = .data$y)) +\n  geom_point(colour = "#087F8C", alpha = .7, size = 2) +\n',
      if(nonempty(s$z)) facet else '',
      '  labs(x = ', r_string(s$x), ', y = ', r_string(s$y), ', title = "Look at the pattern before the coefficient") + plot_theme')
  }
  if (s$route == 'cat2') summ <- paste(summ,
    paste0('count_table <- xtabs(n ~ row_group + column_group, data = summary_table)\n',
           'names(dimnames(count_table)) <- c(',r_string(s$x),', ',r_string(s$y),')\n',
           'count_table_with_totals <- addmargins(count_table)\n',
           'percentage_table <- 100 * prop.table(count_table',
           switch(s$denom,row=', margin = 1',column=', margin = 2',all=''),')\n',
           'percentage_table[!is.finite(percentage_table)] <- NA_real_'),sep='\n\n')
  # Keep layout in the emitted script so downloaded figures follow the app.
  wrap_code <- 'wrap_labels <- function(x, width = 14) {\n  vapply(as.character(x), function(label) {\n    pieces <- strwrap(label, width = width)\n    pieces <- unlist(lapply(pieces, function(part) {\n      starts <- seq.int(1L, max(1L, nchar(part)), by = width)\n      substring(part, starts, starts + width - 1L)\n    }))\n    paste(pieces, collapse = "\\n")\n  }, character(1))\n}'
  category <- switch(s$route, cat = 'category', group = 'group', cat2 = 'row_group', '')
  layout <- c(paste0('facet_columns <- ', columns, 'L'),
    if (nonempty(s$z)) 'panel_count <- length(unique(stats::na.omit(analysis_data$stratum)))' else 'panel_count <- 1L',
    'panel_rows <- ceiling(panel_count / facet_columns)',
    'plot_height_px <- if (panel_count > 1) 120 + panel_rows * 260 else 360',
    wrap_code,
    'plot_main <- plot_main + labs(\n  title = wrap_labels(plot_main$labels$title, 38),\n  x = wrap_labels(plot_main$labels$x, 34), y = wrap_labels(plot_main$labels$y, 34))')
  if (nzchar(category)) layout <- c(layout,
    paste0('category_labels <- levels(factor(analysis_data[[', r_string(category), ']]))'),
    'plot_main <- plot_main + scale_x_discrete(labels = function(x) wrap_labels(x, 12))',
    'if (length(category_labels) > 4 || max(nchar(category_labels)) > 35) {\n  plot_main <- plot_main + coord_flip()\n  plot_height_px <- max(plot_height_px, 120 + panel_rows * (130 + 30 * length(category_labels)))\n}')
  if (s$route == 'cat2') layout <- c(layout,
    'plot_main <- plot_main + guides(fill = guide_legend(ncol = 1)) +\n  scale_fill_manual(values = grDevices::hcl.colors(length(levels(analysis_data$column_group)), "Dark 3"),\n    labels = function(x) wrap_labels(x, 30))',
    'plot_height_px <- plot_height_px + 24 * length(levels(analysis_data$column_group))')
  if (s$route == 'num') layout <- c(layout,
    'plot_extra <- plot_extra + labs(title = wrap_labels(plot_extra$labels$title, 38), y = wrap_labels(plot_extra$labels$y, 34))')
  if (s$route == 'cat2') layout <- c(layout,
    'plot_main <- plot_main + labs(fill = wrap_labels(plot_main$labels$fill, 30))')
  layout <- c(layout, 'plot_export_width <- 8', 'plot_export_height <- max(4.5, plot_height_px / 96)',
    '# To save the complete figure without squeezing its panels:\n# ggsave("figure.png", plot_main, width = plot_export_width, height = plot_export_height, dpi = 300, limitsize = FALSE)')
  paste(c(prep, summ, plot_theme_code, plots, layout), collapse = '\n\n')
}
validate_analysis <- function(d, s) {
  selected <- c(s$x, s$y, s$z); selected <- selected[nzchar(selected)]
  if (!length(selected) || any(!selected %in% names(d))) stop("Choose the variables for this question.")
  if (anyDuplicated(selected)) stop("Choose different variables for each role.")
  nums <- switch(s$route, num = s$x, group = s$x, num2 = c(s$x,s$y), character())
  cats <- setdiff(selected, nums)
  for(v in nums) {
    if(!is.numeric(d[[v]])) stop(paste(v, "needs to be numerical. Check its type in Prepare data."))
    if(!any(!is.na(d[[v]]))) stop(paste(v, "has no observed values."))
  }
  for(v in cats) {
    n <- length(unique(stats::na.omit(d[[v]])))
    if(n == 0) stop(paste(v,"has no observed categories."))
    if(n > 30) stop(paste(v,"has more than 30 categories. Check whether it is an ID or numerical measurement, or combine categories first."))
  }
  if (s$route %in% c('num2','cat2','group') && !any(stats::complete.cases(d[selected]))) stop("There are no complete observations for these selected variables.")
  invisible(TRUE)
}
run_analysis <- function(d,s) {
  validate_analysis(d,s)
  e <- new.env(parent = globalenv()); e$study <- d
  eval(parse(text = analysis_code(s)),e)
  list(table=e$summary_table,plot=e$plot_main,extra=if(exists('plot_extra',e,inherits=FALSE))e$plot_extra else NULL,
       counts=if(s$route=='cat2')e$count_table_with_totals else NULL,percentages=if(s$route=='cat2')e$percentage_table else NULL,
       analysed=nrow(e$analysis_data),height=e$plot_height_px,export_width=e$plot_export_width,export_height=e$plot_export_height,code=analysis_code(s))
}
full_script <- function(meta, steps, spec = NULL) {
  lines <- c('# Learn statistics with R | Complete reproducible script',
    '# Open an RStudio project containing this script and the data/ folder.',
    '# The bundled ZIP has the original file in data/. For a script-only download, copy it there.',
    '# Run this script from top to bottom. install.packages() is needed only once.',
    '# install.packages(c("dplyr", "readr", "readxl", "ggplot2"))',
    'library(dplyr)','library(ggplot2)', '', '# 1. Import the original data',import_code(meta),'study <- raw_data')
  for(i in seq_along(steps)) lines <- c(lines,'',paste0('# Preparation step ',i),step_code(steps[[i]]))
  if (!is.null(spec)) lines <- c(lines,'','# 2. Describe the selected variables',analysis_code(spec),'',if(spec$route=='cat2')c('print(count_table_with_totals)','print(percentage_table)')else 'print(summary_table)','print(plot_main)',if(spec$route=='num')'print(plot_extra)')
  else lines <- c(lines,'','# Inspect the prepared data','print(head(study))','str(study)')
  paste(c(lines,'','# Package and R versions used in your session','sessionInfo()'),collapse='\n')
}
