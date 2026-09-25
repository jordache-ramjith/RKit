# Student scripts use ordinary column names and familiar R commands.
# The app's exact internal scripts remain separate, optional exports.
learner_name <- function(x) paste(deparse(as.name(x),backtick=TRUE),collapse='')
learner_col <- function(x,data='study') paste0(data,'$',learner_name(x))
learner_import <- function(meta) {
 path<-r_string(paste0('data/',meta$name))
 if(meta$kind=='xlsx')return(paste0('study <- readxl::read_excel(',path,', sheet = ',r_string(meta$sheet),',\n  na = ',r_string(meta$na),')'))
 common<-meta$delim==','&&meta$decimal=='.'&&meta$encoding=='UTF-8'
 if(common)return(paste0('study <- readr::read_csv(',path,', na = ',r_string(meta$na),',\n  show_col_types = FALSE)'))
 sub('raw_data <-','study <-',import_code(meta),fixed=TRUE)
}
learner_step <- function(s) {
 if(s$kind=='filter'){
  rules<-vapply(s$rules,function(r){e<-filter_expr(r);gsub(col_code(r$column),learner_col(r$column),e,fixed=TRUE)},character(1))
  return(c('# Keep rows that meet these conditions. & means AND; | means OR.',paste0('keep <- ',paste(rules,collapse=if(s$join=='all')' & 'else' | ')),
   '# A blank after the comma keeps every column.', 'study <- study[keep, ]'))
 }
 x<-learner_col(s$column)
 if(s$kind=='type')return(paste0(x,' <- ',switch(s$type,categorical=paste0('factor(',x,')'),numerical=paste0('as.numeric(as.character(',x,'))'),text=paste0('as.character(',x,')'))))
 rhs<-switch(s$operation,log=paste0('log(',x,')'),scale=paste0(x,' * ',r_num(s$number)),add=paste0(x,' + ',r_num(s$number)),
  subtract=paste0(x,' - ',learner_col(s$other)),ratio=paste0(x,' / ',learner_col(s$other)),
  threshold=paste0('factor(ifelse(',x,' >= ',r_num(s$number),', ',r_string(s$above),', ',r_string(s$below),'))'),
  recode=paste0('ifelse(as.character(',x,') == ',r_string(s$old),', ',r_string(s$new),', as.character(',x,'))'))
 c('# Save the new variable in its own column.',paste0(learner_col(s$name),' <- ',rhs))
}
learner_preamble <- function(meta,steps,packages=c('ggplot2')) {
 lines<-c('# R Kit | A script to learn from',
 '# Open an RStudio project in the folder containing this script and data/.',
 '# Run in order. The ZIP includes the original file; a script-only download needs it in data/.',
 paste0('# Install once if needed: install.packages(',r_string(unique(c(if(meta$kind=='xlsx')'readxl'else'readr',packages))),')'),
 paste0('library(',packages,')'),'', '# 1. Import the data',learner_import(meta),'',
 '# study is our dataset. $ selects one of its columns.')
 for(i in seq_along(steps))lines<-c(lines,'',paste0('# Preparation step ',i),learner_step(steps[[i]]))
 lines
}
learner_clean <- function(cols) {
 if(length(cols)==1)return(c('# Keep people with this variable recorded; is.na() identifies missing values.',paste0('analysis <- subset(study, !is.na(',learner_name(cols),'))')))
 c('# complete.cases() keeps people with every selected variable recorded.',paste0('analysis <- study[complete.cases(study[',r_string(cols),']), ]'))
}
learner_gg <- function(data,mapping,geometry,labels=NULL,facet=NULL) c(paste0('plot_main <- ggplot(data = ',data,', aes(',mapping,')) +'),paste0('  ',geometry,' +'),
 if(!is.null(facet))paste0('  facet_wrap(~ ',facet,', ncol = 2) +'),if(!is.null(labels))paste0('  labs(',labels,') +'),'  theme_minimal()','print(plot_main)')
learner_descriptive <- function(s,d=NULL) {
 x<-learner_col(s$x,'analysis');xn<-learner_name(s$x);yn<-if(nonempty(s$y))learner_name(s$y)else'';y<-if(nonempty(s$y))learner_col(s$y,'analysis')else''
 facet<-if(nonempty(s$z))learner_name(s$z)else NULL
 lines<-c('# 2. Describe the selected variables')
 if(s$route=='cat'){
  lines<-c(lines,paste0('sum(is.na(',learner_col(s$x),'))  # Number missing'),learner_clean(s$x))
  if(!is.null(d)&&is.factor(d[[s$x]]))lines<-c(lines,paste0(x,' <- droplevels(',x,')'))
  lines<-c(lines,'# table() counts people in each category.',paste0('t1 <- table(',x,')'),'t1',
   '# prop.table() divides each count by the total. Multiply by 100 for percentages.','prop.table(t1)','100 * prop.table(t1)')
  if(s$measure=='count')lines<-c(lines,learner_gg('analysis',paste0('x = ',xn),'geom_bar(fill = "#087F8C")',paste0('x = ',r_string(s$x),', y = "Count"')))else lines<-c(lines,
   '# Put the counts and percentages into a small dataset for the bar chart.','bar_data <- as.data.frame(t1)','names(bar_data) <- c("category", "count")','bar_data$percent <- 100 * bar_data$count / sum(bar_data$count)',
   learner_gg('bar_data','x = category, y = percent','geom_col(fill = "#087F8C")',paste0('x = ',r_string(s$x),', y = "Percent"')))
 }else if(s$route=='num'){
  lines<-c(lines,'analysis <- study',paste0('sum(is.na(',x,'))  # Number missing'),paste0('sum(!is.na(',x,'))  # Number recorded'),
   '# na.rm = TRUE leaves out missing measurements.',paste0('summary(',x,')'),paste0('mean(',x,', na.rm = TRUE)'),paste0('sd(',x,', na.rm = TRUE)'),paste0('var(',x,', na.rm = TRUE)'),paste0('IQR(',x,', na.rm = TRUE)'),
   learner_gg('analysis',paste0('x = ',xn),paste0('geom_histogram(bins = ',s$bins,', fill = "#087F8C", colour = "white", na.rm = TRUE)')),
   paste0('plot_extra <- ggplot(data = analysis, aes(x = "", y = ',xn,')) +\n  geom_boxplot(na.rm = TRUE) +\n  labs(x = NULL) +\n  theme_minimal()'),'print(plot_extra)')
 }else if(s$route=='cat2'){
  lines<-c(lines,learner_clean(c(s$x,s$y)),'# Rows are the first variable; columns are the second.',paste0('t1 <- table(',x,', ',y,')'),'t1','addmargins(t1)  # Add row and column totals',
   switch(s$denom,row='# margin = 1 divides by each row total.',column='# margin = 2 divides by each column total.',all='# With no margin, divide by everyone in the table.'),
   paste0('percentages <- 100 * prop.table(t1',switch(s$denom,row=', margin = 1',column=', margin = 2',all=''),')'),'percentages',
   'bar_data <- as.data.frame(percentages)','names(bar_data) <- c("row_group", "column_group", "percent")',
   learner_gg('bar_data','x = row_group, y = percent, fill = column_group','geom_col(position = "dodge", na.rm = TRUE)',paste0('x = ',r_string(s$x),', fill = ',r_string(s$y),', y = "',switch(s$denom,row='Percent within each row',column='Percent within each column',all='Percent of all complete observations'),'"')))
 }else if(s$route=='group'){
  groups<-c(s$y,if(nonempty(s$z))s$z)
  index<-if(length(groups)==1)y else paste0('list(',paste(vapply(groups,learner_col,character(1),data='analysis'),collapse=', '),')')
  lines<-c(lines,learner_clean(groups),'# tapply() repeats a summary within each group.',paste0('tapply(',x,', ',index,', summary)'),paste0('tapply(',x,', ',index,', sd, na.rm = TRUE)'),paste0('tapply(',x,', ',index,', var, na.rm = TRUE)'),paste0('tapply(',x,', ',index,', IQR, na.rm = TRUE)'),
   '# FALSE counts recorded measurements; TRUE counts missing measurements.',paste0('table(',paste(c(vapply(groups,learner_col,character(1),data='analysis'),paste0('is.na(',x,')')),collapse=', '),')'),
   learner_gg('analysis',paste0('x = factor(',yn,'), y = ',xn),'geom_boxplot(fill = "#B8DFD9", outlier.shape = NA, na.rm = TRUE) +\n  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 42), alpha = 0.4, na.rm = TRUE)',paste0('x = ',r_string(s$y)),facet))
 }else{
  cols<-c(s$x,s$y,if(nonempty(s$z))s$z);lines<-c(lines,learner_clean(cols),'nrow(analysis)  # Complete pairs')
  corr<-paste0('cor(',x,', ',y,', method = ',r_string(s$cor),')')
  guard<-paste0('nrow(analysis) >= 3 && sd(',x,') > 0 && sd(',y,') > 0')
  if(is.null(facet))lines<-c(lines,'# R Kit reports a correlation only with at least 3 pairs and variation in both variables.',paste0('correlation <- if (',guard,') ',corr,' else NA'),'correlation')else{
   # A short loop avoids repeating a large block for every observed group.
   lines<-c(lines,paste0('complete_data <- analysis'),paste0('groups <- unique(',learner_col(s$z,'analysis'),')'),
    '# A for loop repeats the commands between { and } for each group.', 'for (group in groups) {',paste0('  analysis <- complete_data[',learner_col(s$z,'complete_data'),' == group, ]'),'  print(group)',
    paste0('  correlation <- if (',guard,') ',corr,' else NA'),'  print(correlation)','}','analysis <- complete_data')
  }
  lines<-c(lines,learner_gg('analysis',paste0('x = ',xn,', y = ',yn),'geom_point(colour = "#087F8C", alpha = 0.7)',facet=facet))
 }
 lines
}
learner_script <- function(meta,steps,spec=NULL,raw=NULL) {
 d<-if(!is.null(raw))apply_steps(raw,steps)else NULL
 lines<-learner_preamble(meta,steps)
 if(is.null(spec))lines<-c(lines,'','# Inspect the prepared data','head(study)','str(study)')else lines<-c(lines,'',learner_descriptive(spec,d))
 paste(lines,collapse='\n')
}
learner_estimation_script <- function(meta,steps,s,raw=NULL) {
 mean<-s$kind=='mean';grouped<-nonempty(s$group);compare<-isTRUE(s$compare)
 packages<-c('ggplot2',if(!mean)'DescTools',if(!mean&&compare)'epitools')
 lines<-c(learner_preamble(meta,steps,packages),'','# 2. Estimate the selected measurement or proportion',
  paste0('confidence <- ',r_num(s$conf)),if(!mean)paste0('event <- ',r_string(s$event)))
 if(grouped)lines<-c(lines,learner_clean(s$group),paste0('groups <- sort(unique(as.character(',learner_col(s$group,'analysis'),')))'))else lines<-c(lines,'analysis <- study')
 lines<-c(lines,'estimate_table <- data.frame()',
  if(grouped)'# Repeat the same calculation for each group.',if(grouped)'for (group in groups) {'else'group <- "All observations"',
  paste0('  values <- ',learner_col(s$x,'analysis'),if(grouped)paste0('[',learner_col(s$group,'analysis'),' == group]')else''),
  '  missing <- sum(is.na(values))','  values <- values[!is.na(values)]','  n <- length(values)',
  '  ci <- c(NA, NA)  # NA means that an interval is unavailable.')
 if(mean)lines<-c(lines,'  estimate <- if (n > 0) mean(values) else NA','  spread <- sd(values)','  se <- spread / sqrt(n)',
  '  if (n >= 2 && spread > 0) {',
  '    # t.test() also calculates a mean interval; $conf.int selects only that interval.',
  '    ci <- t.test(values, conf.level = confidence)$conf.int','  }')else lines<-c(lines,
  '  events <- sum(values == event)','  estimate <- if (n > 0) events / n else NA','  se <- sqrt(estimate * (1 - estimate) / n)',
  '  if (n > 0) {','    # BinomCI() returns the proportion and its lower and upper Wilson limits.',
  '    result <- BinomCI(events, n, conf.level = confidence, method = "wilson")','    ci <- result[1, c("lwr.ci", "upr.ci")]','  }')
 lines<-c(lines,'  estimate_table <- rbind(estimate_table,',
  paste0('    data.frame(group = group, n = n, missing = missing, ',if(mean)'sd = spread, 'else'events = events, ','estimate = estimate,'),
  '      se = se, lower = ci[1], upper = ci[2]))',if(grouped)'}',
  if(!mean)c('# Convert proportions to percentages, and SE to percentage points.','estimate_table[c("estimate", "se", "lower", "upper")] <-', '  100 * estimate_table[c("estimate", "se", "lower", "upper")]'),
  'print(estimate_table)',
  '# A dot is the estimate; the line is its confidence interval.',
  learner_gg('estimate_table','x = estimate, y = group','geom_segment(aes(x = lower, xend = upper, yend = group), na.rm = TRUE) +\n  geom_point(na.rm = TRUE)',paste0('x = ',r_string(if(mean)if(nonempty(s$unit))paste('Mean (',s$unit,')')else'Mean'else'Percent'),', y = NULL')))
 if(!compare)return(paste(lines,collapse='\n'))
 ref<-r_string(s$reference);ac<-learner_col(s$group,'analysis');vx<-learner_col(s$x,'analysis')
 lines<-c(lines,'','# 3. Compare the two groups',paste0('reference <- ',ref),'comparison <- setdiff(groups, reference)',
  paste0('reference_values <- ',vx,'[',ac,' == reference]'),paste0('comparison_values <- ',vx,'[',ac,' != reference]'),
  'reference_values <- reference_values[!is.na(reference_values)]','comparison_values <- comparison_values[!is.na(comparison_values)]',
  '# Differences subtract the reference. Ratios divide by the reference.')
 if(mean){
  lines<-c(lines,'difference <- mean(comparison_values) - mean(reference_values)','difference_ci <- c(NA, NA)',
   'if (length(comparison_values) >= 2 && length(reference_values) >= 2 &&',
   '    (sd(comparison_values) > 0 || sd(reference_values) > 0)) {',
   '  # var.equal = FALSE requests the Welch interval used in the app.',
   '  difference_ci <- t.test(comparison_values, reference_values,',
   '    var.equal = FALSE, conf.level = confidence)$conf.int','}',
   'comparison_table <- data.frame(measure = "Mean difference", estimate = difference,',
   '  lower = difference_ci[1], upper = difference_ci[2], null = 0)')
 }else{
  design<-if(nonempty(s$design))s$design else'cohort'
  lines<-c(lines,'# Keep the event and reference order explicit.',
   't1 <- rbind(', '  Reference = c(sum(reference_values != event), sum(reference_values == event)),',
   '  Comparison = c(sum(comparison_values != event), sum(comparison_values == event)))',
   'rownames(t1) <- c(reference, comparison)','colnames(t1) <- c("Other outcome", "Event")','t1','100 * prop.table(t1, margin = 1)',
   '# [row, column] selects one count. The reference is row 1; the event is column 2.',
   'a <- t1[2, 2]  # Events in the comparison group','n1 <- sum(t1[2, ])',
   'c0 <- t1[1, 2]  # Events in the reference group','n0 <- sum(t1[1, ])',
   'difference <- risk_ratio <- odds_ratio <- c(NA, NA, NA)',
   '# Each result below has three numbers: estimate, lower limit, upper limit.',
   'if (n1 > 0 && n0 > 0) {')
  if(design!='case_control')lines<-c(lines,
   '  # method = "score" matches the Newcombe difference interval in R Kit.',
   '  difference <- 100 * as.numeric(BinomDiffCI(a, n1, c0, n0,',
   '    conf.level = confidence, method = "score"))',
   '  risk_ratio[1] <- (a / n1) / (c0 / n0)',
   '  if (a > 0 && c0 > 0 && (a < n1 || c0 < n0)) {',
   '    # $measure selects estimates and intervals; [2, ] selects the comparison row.',
   '    risk_ratio <- as.numeric(riskratio.wald(t1, conf.level = confidence)$measure[2, ])','  }')else lines<-c(lines,
   '  # Sample percentages can be calculated but do not directly estimate population risks',
   '  # under case-control sampling. We therefore report only the odds-ratio comparison.')
  lines<-c(lines,'  odds_ratio[1] <- (a * (n0 - c0)) / ((n1 - a) * c0)',
   '  if (all(t1 > 0)) {','    odds_ratio <- as.numeric(oddsratio.wald(t1, conf.level = confidence)$measure[2, ])','  }','}',
   '# Zero cells may leave a ratio interval unavailable; no artificial counts are added.',
   '# With small counts, approximate log intervals can be unreliable.',
   'comparison_table <- data.frame(',
   paste0('  measure = ',r_string(c(if(design=='cross_sectional')'Prevalence difference'else'Risk difference',if(design=='cross_sectional')'Prevalence ratio'else'Risk ratio','Odds ratio')),','),
   '  estimate = c(difference[1], risk_ratio[1], odds_ratio[1]),',
   '  lower = c(difference[2], risk_ratio[2], odds_ratio[2]),',
   '  upper = c(difference[3], risk_ratio[3], odds_ratio[3]), null = c(0, 1, 1))')
 }
 lines<-c(lines,'print(comparison_table)',
  '# Draw a separate interval figure for each comparison. A dashed line marks no difference.',
  '# The app explains unavailable intervals; here we draw only finite intervals.',
  'comparison_plots <- list()',
  'for (i in seq_len(nrow(comparison_table))) {',
  '  result <- comparison_table[i, ]',
  '  if (all(is.finite(c(result$estimate, result$lower, result$upper)))) {',
  '    p <- ggplot(result, aes(x = estimate, y = measure)) +',
  '      geom_vline(xintercept = result$null, linetype = "dashed") +',
  '      geom_segment(aes(x = lower, xend = upper, yend = measure)) +',
  '      geom_point() + labs(subtitle = paste(comparison, "versus", reference)) + theme_minimal()',
  '    if (result$null == 1) p <- p + scale_x_log10()',
  '    comparison_plots[[i]] <- p','    print(p)','  }','}')
 paste(lines,collapse='\n')
}

learner_simulation_script <- function(kind,n,conf,seed,step=2L) {
 mean<-kind=='mean'
 lines<-c('# R Kit | Repeat the study on a computer',
  paste0('# Install once if needed: install.packages(',r_string(c('ggplot2',if(!mean)'DescTools')),')'),
  'library(ggplot2)',if(!mean)'library(DescTools)',
  '# A seed lets us reproduce the same random samples.',paste0('set.seed(',seed,')'),
  paste0('sample_size <- ',n),paste0('confidence <- ',conf),
  paste0('population_value <- ',if(mean)'7'else'0.4'),
  if(mean)'population_sd <- 1.5',
  'estimates <- standard_errors <- lower <- upper <- numeric(100)',
  '# Repeat the commands inside { and } for 100 fictional studies.',
  'for (i in 1:100) {',
  if(mean)c('  # Draw sleep times from a population with mean 7 and SD 1.5 hours.',
   '  sample <- rnorm(sample_size, mean = population_value, sd = population_sd)',
   '  estimates[i] <- mean(sample)','  standard_errors[i] <- sd(sample) / sqrt(sample_size)',
   '  ci <- t.test(sample, conf.level = confidence)$conf.int')else c(
   '  # Each person is coded 1 (aged 40 or older) or 0 (under 40).',
   '  sample <- rbinom(sample_size, size = 1, prob = population_value)',
   '  estimates[i] <- mean(sample)','  standard_errors[i] <- sqrt(estimates[i] * (1 - estimates[i]) / sample_size)',
   '  ci <- BinomCI(sum(sample), sample_size, conf.level = confidence, method = "wilson")[1, c("lwr.ci", "upr.ci")]'),
  '  lower[i] <- ci[1]','  upper[i] <- ci[2]','}',
  '# The loop leaves the final study in sample.',
  'simulation_table <- data.frame(study = 1:100, estimate = estimates, se = standard_errors, lower = lower, upper = upper)',
  'simulation_table$contains_population <- lower <= population_value & upper >= population_value',
  'print(simulation_table)',
  'sd(estimates)  # The spread of estimates across the 100 studies')
 if(!mean)lines<-c(lines,'# Display proportions as percentages.',
  'simulation_table[c("estimate", "se", "lower", "upper")] <- 100 * simulation_table[c("estimate", "se", "lower", "upper")]',
  'population_value <- 100 * population_value')
 lines<-c(lines,
  'plot_sampling <- ggplot(simulation_table, aes(x = estimate)) +',
  '  geom_histogram(bins = 20, fill = "#087F8C", colour = "white") +',
  '  geom_vline(xintercept = population_value, linetype = "dashed") +',
  paste0('  labs(x = ',r_string(if(mean)'Sample mean (hours)'else'Sample percentage'),', y = "Number of studies") +'),
  '  theme_minimal()','print(plot_sampling)')
 if(step==3L)lines<-c(lines,if(mean)c(
  'spread_data <- rbind(',
  '  data.frame(value = sample, source = "Individual people in the latest study"),',
  '  data.frame(value = estimates, source = "Means from 100 studies"))',
  'plot_spread <- ggplot(spread_data, aes(x = value)) +',
  '  geom_histogram(bins = 25, fill = "#087F8C", colour = "white") +',
  '  facet_wrap(~ source, ncol = 1) + theme_minimal()')else c(
  'people <- data.frame(person = seq_along(sample), event = factor(sample))',
  'plot_spread <- ggplot(people, aes(x = (person - 1) %% 10, y = -((person - 1) %/% 10), colour = event)) +',
  '  geom_point(size = 4) + theme_void()',
  '# %% and %/% arrange the people into rows of ten; the values have not changed.'),'print(plot_spread)')
 if(step%in%c(4L,5L))lines<-c(lines,
  'table(simulation_table$contains_population)  # FALSE misses; TRUE contains the population value.',
  'plot_coverage <- ggplot(simulation_table, aes(x = estimate, y = study, colour = contains_population)) +',
  '  geom_segment(aes(x = lower, xend = upper, yend = study)) +',
  '  geom_point() + geom_vline(xintercept = population_value, linetype = "dashed") +',
  '  theme_minimal()','print(plot_coverage)')
 paste(lines,collapse='\n')
}
