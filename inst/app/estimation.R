# Estimation calculations and export templates share these functions.
wilson_interval <- function(events, n, level = .95) {
  if (n == 0) return(c(NA_real_, NA_real_))
  p <- events / n; z <- stats::qnorm((1 + level) / 2)
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half_width <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(max(0, centre - half_width), min(1, centre + half_width))
}
mean_interval_summary <- function(value, level) {
  observed <- value[!is.na(value)]; n <- length(observed)
  estimate <- if(n) mean(observed) else NA_real_
  sd_value <- if(n > 1) stats::sd(observed) else NA_real_
  se <- if(n > 1) sd_value / sqrt(n) else NA_real_
  available <- n > 1 && is.finite(sd_value) && sd_value > 0
  margin <- if(available) stats::qt((1 + level) / 2, df = n - 1) * se else NA_real_
  status <- if(!n) 'No observed values' else if(n == 1) 'At least two values needed for an interval' else if(sd_value == 0) 'No observed variation: routine t interval unavailable' else if(n < 30) 'Small sample: check shape and unusual values carefully' else 'Check independence, shape and sampling'
  data.frame(rows = length(value), n = n, missing = sum(is.na(value)), estimate = estimate,
    sd = sd_value, se = se, lower = estimate - margin, upper = estimate + margin, note = status)
}
proportion_interval_summary <- function(value, event, level) {
  observed <- as.character(value[!is.na(value)]); n <- length(observed)
  events <- sum(observed == event); estimate <- if(n) events / n else NA_real_
  bounds <- wilson_interval(events, n, level)
  status <- if(!n) 'No observed values' else if(events == 0 || events == n) 'One observed category: interval still reflects uncertainty' else if(min(events,n-events) < 5) 'Few events or non-events: interpret cautiously' else 'Check independence and sampling'
  data.frame(rows = length(value), n = n, missing = sum(is.na(value)), events = events,
    estimate = estimate, se = if(n) sqrt(estimate * (1-estimate) / n) else NA_real_,
    lower = bounds[1], upper = bounds[2], note = status)
}
est_definition <- function(name, fun) paste0(name, ' <- ', paste(deparse(fun, width.cutoff=95), collapse='\n'))
est_wrap <- function(x, width=25) vapply(as.character(x), function(label) {
  pieces <- strwrap(label,width=width)
  pieces <- unlist(lapply(pieces,function(part){i<-seq.int(1L,max(1L,nchar(part)),by=width);substring(part,i,i+width-1L)}))
  paste(pieces,collapse='\n')
},character(1))

estimation_code <- function(s) {
  stopifnot(s$kind %in% c('mean','proportion'),s$conf %in% c(.9,.95,.99))
  grouped <- nonempty(s$group)
  prefix <- c('# Estimate a mean or a binary proportion; each row must be an independent observation.',
    paste0('outcome_name <- ',r_string(s$x)),paste0('group_name <- ',r_string(if(grouped)s$group else '')),
    paste0('confidence_level <- ',r_num(s$conf)),paste0('event_category <- ',r_string(if(s$kind=='proportion')s$event else '')),
    'stopifnot(outcome_name %in% names(study), !nzchar(group_name) || group_name %in% names(study))',
    'if (identical(outcome_name, group_name)) stop("Choose different outcome and grouping variables.")',
    'value <- study[[outcome_name]]',
    if(s$kind=='mean')'if (!is.numeric(value) || any(!is.finite(value) & !is.na(value))) stop("Choose a finite numerical measurement.")' else c(
      'observed_categories <- unique(as.character(value[!is.na(value)]))',
      'if (length(observed_categories) > 2) stop("Choose a binary variable with at most two observed categories.")',
      'if (!nzchar(event_category)) stop("Choose the category you want to count as an event.")',
      'if (length(observed_categories) == 2 && !event_category %in% observed_categories) stop("Choose one of the two observed categories.")'),
    'if (!any(!is.na(value))) stop("This variable has no observed values.")',
    'group <- if(nzchar(group_name)) as.character(study[[group_name]]) else rep("All observations",nrow(study))',
    'excluded_for_missing_group <- sum(is.na(group))',
    'analysis_data <- data.frame(value = value, group = group, stringsAsFactors = FALSE)',
    'analysis_data <- analysis_data[!is.na(analysis_data$group), , drop = FALSE]',
    'if (!nrow(analysis_data)) stop("There are no observations with a recorded group.")',
    'if (length(unique(analysis_data$group)) > 30) stop("Choose a grouping variable with no more than 30 groups.")')
  helper <- if(s$kind=='mean')est_definition('mean_interval_summary',mean_interval_summary) else c(est_definition('wilson_interval',wilson_interval),est_definition('proportion_interval_summary',proportion_interval_summary))
  summary <- c('# Calculate each group separately. These are individual intervals, not an interval for a group difference.',
    'groups <- split(analysis_data$value, analysis_data$group)',
    paste0('estimate_table <- do.call(rbind, lapply(names(groups), function(label) {\n  result <- ',
      if(s$kind=='mean')'mean_interval_summary(groups[[label]], confidence_level)' else 'proportion_interval_summary(groups[[label]], event_category, confidence_level)',
      '\n  data.frame(group = label, result, row.names = NULL)\n}))'),
    'rownames(estimate_table) <- NULL')
  label <- if(s$kind=='mean')paste('Mean of',s$x,if(nonempty(s$unit))paste0(' (',s$unit,')')else'') else paste('Proportion with',s$x,'=',s$event)
  plots <- c(est_definition('est_wrap',est_wrap),
    'plot_theme <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face="bold", size=13))',
    'plot_data <- estimate_table\nplot_data$group <- factor(plot_data$group, levels = rev(estimate_table$group))',
    paste0('plot_main <- ggplot(plot_data, aes(x = .data$group, y = .data$estimate)) +\n',
      '  geom_errorbar(aes(ymin = .data$lower, ymax = .data$upper), width = .12, colour = "#087F8C", na.rm = TRUE) +\n',
      '  geom_point(size = 3, colour = "#087F8C", na.rm = TRUE) + coord_flip() +\n',
      '  scale_x_discrete(labels = function(x) est_wrap(x, 22)) +\n',
      if(s$kind=='proportion')'  scale_y_continuous(limits = c(0,1), labels = function(x) paste0(round(100*x), "%")) +\n'else'',
      '  labs(x = NULL, y = est_wrap(',r_string(label),', 45),\n',
      '       title = ',r_string(paste0(round(s$conf*100),'% confidence intervals')),') + plot_theme'),
    'plot_height_px <- max(330, 150 + 60 * nrow(estimate_table))',
    'plot_export_width <- 8\nplot_export_height <- max(4.5, plot_height_px/96)')
  if(s$kind=='mean') {
    diagnostic <- if(grouped) paste0('plot_diagnostic <- ggplot(analysis_data, aes(x = .data$group, y = .data$value)) +\n',
      '  geom_boxplot(fill="#B8DFD9", outlier.shape=NA, na.rm=TRUE) +\n  geom_point(position=position_jitter(width=.1,height=0,seed=42), colour="#087F8C", alpha=.5, na.rm=TRUE) +\n',
      '  coord_flip() + scale_x_discrete(labels=function(x) est_wrap(x,22)) +\n  labs(x=NULL, y=est_wrap(outcome_name,35), title="Look at the values within each group") + plot_theme') else paste0(
      'plot_diagnostic <- ggplot(analysis_data, aes(x=.data$value)) +\n',
      '  geom_histogram(bins=15, fill="#087F8C", colour="white", na.rm=TRUE) +\n',
      '  labs(x=est_wrap(outcome_name,35), y="Observations", title="Inspect the distribution before estimating") + plot_theme')
  } else diagnostic <- paste0('event_counts <- data.frame(group = rep(estimate_table$group, each=2),\n',
    '  category = rep(c("Event", "Other category"), nrow(estimate_table)),\n',
    '  count = as.vector(rbind(estimate_table$events, estimate_table$n-estimate_table$events)))\n',
    'plot_diagnostic <- ggplot(event_counts, aes(x=.data$group, y=.data$count, fill=.data$category)) +\n',
    '  geom_col(position="dodge") + coord_flip() +\n',
    '  scale_x_discrete(labels=function(x) est_wrap(x,22)) +\n',
    '  scale_fill_manual(values=c("Event"="#087F8C", "Other category"="#B8DFD9")) +\n',
    '  labs(x=NULL,y="Observed count",fill=NULL,title=est_wrap(paste("Event:",event_category),40)) +\n',
    '  plot_theme + theme(legend.position="bottom")')
  paste(c(prefix,helper,summary,plots,diagnostic,if(isTRUE(s$compare))comparison_code(s)),collapse='\n\n')
}
run_estimation <- function(d,s) {
  e<-new.env(parent=globalenv());e$study<-d;eval(parse(text=estimation_code(s)),e)
  list(table=e$estimate_table,plot=e$plot_main,diagnostic=e$plot_diagnostic,
    excluded=e$excluded_for_missing_group,height=e$plot_height_px,
    width=e$plot_export_width,export_height=e$plot_export_height,
    comparison=if(isTRUE(s$compare))e$comparison_table else NULL,comparison_plots=if(isTRUE(s$compare))e$comparison_plots else NULL,
    comparison_group=if(isTRUE(s$compare))e$comparison_group else NULL,counts=if(isTRUE(s$compare)&&s$kind=='proportion')e$comparison_counts else NULL)
}
estimation_script <- function(meta,steps,s) {
  pre<-c('# Learn statistics with R | Complete estimation script',
    '# Run in an RStudio project containing the data/ folder. Each row must be an independent observation.',
    '# install.packages(c("readr", "readxl", "dplyr", "ggplot2"))','library(dplyr)','library(ggplot2)',
    '# 1. Import the original data',import_code(meta),'study <- raw_data')
  for(i in seq_along(steps))pre<-c(pre,paste0('# Preparation step ',i),step_code(steps[[i]]))
  paste(c(pre,'# 2. Estimate with a confidence interval',estimation_code(s),
    'print(estimate_table)','print(plot_diagnostic)','print(plot_main)',
    if(isTRUE(s$compare))c(if(s$kind=='proportion')'print(comparison_counts)','print(comparison_table)','for(p in comparison_plots) print(p)'),
    '# ggsave("estimate.png",plot_main,width=plot_export_width,height=plot_export_height,dpi=300,limitsize=FALSE)',
    'sessionInfo()'),collapse='\n\n')
}

est_display_table <- function(r,s,preview=FALSE) {
  d<-r$table
  if(preview){
    cols<-c('group','rows','n','missing',if(s$kind=='mean')c('estimate','sd')else c('events','estimate'))
    d<-d[cols]
  }
  if(s$kind=='proportion'){
    for(k in intersect(c('estimate','se','lower','upper'),names(d)))d[[k]]<-100*d[[k]]
  }
  labels<-c(group=if(nonempty(s$group))s$group else 'Sample',rows='Rows',n='Observed',missing='Missing',events='Events',
    estimate=if(s$kind=='mean')'Mean'else'Percent',sd='SD',se=if(s$kind=='mean')'SE'else'SE (percentage points)',
    lower=paste0(round(s$conf*100),'% CI lower'),upper=paste0(round(s$conf*100),'% CI upper'),note='Interpretation note')
  names(d)<-unname(labels[names(d)]);d
}
est_description <- function(r,s,preview=FALSE) {
  fmt<-function(x,digits=2)formatC(x,format='f',digits=digits)
  vapply(seq_len(nrow(r$table)),function(i){a<-r$table[i,];group<-if(nonempty(s$group))paste0('In ',a$group,', ')else''
    if(!a$n)return(paste0(group,'there were no recorded values; ',a$missing,' were missing. An estimate could not be calculated.'))
    estimate<-if(s$kind=='mean')paste0('the mean ',s$x,' was ',fmt(a$estimate),if(nonempty(s$unit))paste0(' ',s$unit)else'') else paste0(a$events,' of ',a$n,' observations were ',s$event,' (',fmt(100*a$estimate,1),'%)')
    interval<-if(is.finite(a$lower)&&is.finite(a$upper))paste0(' The ',round(100*s$conf),'% confidence interval was ',fmt(a$lower*if(s$kind=='mean')1 else 100),' to ',fmt(a$upper*if(s$kind=='mean')1 else 100),if(s$kind=='proportion')'%'else if(nonempty(s$unit))paste0(' ',s$unit)else'','.')else' A confidence interval was unavailable; see the note in the table.'
    paste0(group,estimate,'.',if(preview)''else interval,' There were ',a$n,' recorded values and ',a$missing,' missing values.')
  },character(1))
}

# Reproducible simulations use known populations, never the uploaded dataset.
est_simulation_code <- function(kind,n,conf,seed) {
  stopifnot(kind%in%c('mean','proportion'),n%in%5:200,conf%in%c(.9,.95,.99),is.finite(seed))
  lines<-c('# A teaching simulation: 100 independent samples from a known population.',
    'library(ggplot2)',paste0('set.seed(',as.integer(seed),')'),paste0('sample_size <- ',as.integer(n)),
    paste0('confidence_level <- ',r_num(conf)),'number_of_samples <- 100',
    if(kind=='mean')c('population_value <- 7\npopulation_sd <- 1.5',
      'samples <- replicate(number_of_samples, rnorm(sample_size, mean=population_value, sd=population_sd))',
      'estimates <- colMeans(samples)','standard_errors <- apply(samples,2,sd)/sqrt(sample_size)',
      'margin <- qt((1+confidence_level)/2,df=sample_size-1)*standard_errors',
      'lower <- estimates-margin\nupper <- estimates+margin',
      'true_standard_error <- population_sd/sqrt(sample_size)') else c(
      'population_value <- .4','samples <- replicate(number_of_samples, rbinom(sample_size,1,population_value))',
      'events <- colSums(samples)\nestimates <- events/sample_size',
      'standard_errors <- sqrt(estimates*(1-estimates)/sample_size)',est_definition('wilson_interval',wilson_interval),
      'bounds <- t(vapply(events,function(x)wilson_interval(x,sample_size,confidence_level),numeric(2)))',
      'lower <- bounds[,1]\nupper <- bounds[,2]',
      'true_standard_error <- sqrt(population_value*(1-population_value)/sample_size)'),
    'simulation_table <- data.frame(sample=seq_len(number_of_samples),estimate=estimates,se=standard_errors,lower=lower,upper=upper)',
    'simulation_table$contains_population <- lower <= population_value & upper >= population_value',
    'recent_sample <- samples[,number_of_samples]',
    paste0('display_scale <- ',if(kind=='mean')'1'else'100'),
    'plot_sampling <- ggplot(simulation_table,aes(x=.data$estimate*display_scale)) +\n  geom_histogram(bins=20,fill="#087F8C",colour="white") +\n  geom_vline(xintercept=population_value*display_scale,colour="#7161AC",linewidth=1,linetype=2) +\n  labs(x=',r_string(if(kind=='mean')'Sample mean (hours)'else'Sample percentage'),' ,y="Number of samples",title="Estimates from 100 separate samples") + theme_minimal(base_size=12)',
    'plot_coverage <- ggplot(simulation_table,aes(y=.data$sample,x=.data$estimate*display_scale,colour=.data$contains_population)) +\n  geom_segment(aes(x=.data$lower*display_scale,xend=.data$upper*display_scale,yend=.data$sample)) +\n  geom_point(size=.8) +\n  geom_vline(xintercept=population_value*display_scale,linetype=2,colour="#19343D") +\n  scale_colour_manual(values=c("TRUE"="#087F8C","FALSE"="#D26D42"),labels=c("FALSE"="Misses the population value","TRUE"="Contains the population value")) +\n  labs(x=',r_string(if(kind=='mean')'Mean and confidence interval (hours)'else'Percentage and confidence interval'),',y="Repeated sample",colour=NULL,title="Which intervals contain the population value?") +\n  theme_minimal(base_size=12) + theme(legend.position="bottom")',
    if(kind=='mean')c(
      'spread_data <- rbind(data.frame(value=recent_sample,source="Individual values in the latest sample"),data.frame(value=estimates,source="Means from 100 samples"))',
      'plot_spread <- ggplot(spread_data,aes(x=.data$value)) + geom_histogram(bins=25,fill="#087F8C",colour="white") +\n  facet_wrap(~source,ncol=1,scales="fixed") + labs(x="Hours",y="Frequency",title="Different spreads: observations and sample means") + theme_minimal(base_size=12)') else c(
      'sample_points <- data.frame(position=seq_along(recent_sample),event=as.logical(recent_sample))',
      'plot_spread <- ggplot(sample_points,aes(x=(.data$position-1)%%10,y=-((.data$position-1)%/%10),colour=.data$event)) +\n  geom_point(size=4) + scale_colour_manual(values=c("FALSE"="#B8DFD9","TRUE"="#087F8C"),labels=c("FALSE"="No event","TRUE"="Event")) +\n  labs(title="People in the latest sample",colour=NULL) + theme_void(base_size=12) + theme(legend.position="bottom")'),
    'print(simulation_table)','print(plot_sampling)','print(plot_spread)','print(plot_coverage)')
  paste(lines,collapse='\n')
}
run_est_simulation <- function(kind,n,conf,seed) {
  # Capture and restore global RNG so teaching simulations do not alter other work.
  had_seed<-exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  if(had_seed)old_seed<-get('.Random.seed',envir=.GlobalEnv)
  on.exit(if(had_seed)assign('.Random.seed',old_seed,envir=.GlobalEnv)else if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv))
  code<-est_simulation_code(kind,n,conf,seed)
  e<-new.env(parent=globalenv());expressions<-parse(text=code);eval(head(expressions,-4L),e)
  list(table=e$simulation_table,sampling=e$plot_sampling,spread=e$plot_spread,coverage=e$plot_coverage,
    latest=e$recent_sample,population=e$population_value,true_se=e$true_standard_error,code=code)
}
