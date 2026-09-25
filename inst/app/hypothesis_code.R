h_student_script <- function(meta,steps,s,d) {
 p<-h_prepare(apply_steps(d,steps),s);m<-s$method;k<-s$route
 cols<-unique(c(s$x,if(k%in%c('paired','paired_binary','two','many','categorical'))s$y))
 lines<-c(learner_preamble(meta,steps),'','# 2. Keep complete observations for this question',learner_clean(cols),
  '# For paired data, keep a row only when BOTH measurements are recorded.',
  'nrow(study) - nrow(analysis)  # Rows excluded for this analysis',
  paste0('x <- ',learner_col(s$x,'analysis'),'  # ',if(k%in%c('paired','paired_binary'))'Before'else'Selected outcome'))
 if(k%in%c('paired','paired_binary','categorical'))lines<-c(lines,paste0('y <- ',learner_col(s$y,'analysis'),'  # ',if(k%in%c('paired','paired_binary'))'After'else'Second category'))
 if(k%in%c('two','many'))lines<-c(lines,paste0('group <- factor(',learner_col(s$y,'analysis'),', levels = ',r_string(levels(p$g)),')'))
 if(isTRUE(s$log))lines<-c(lines,'','# Natural logarithms change the question to geometric means / ratios.',
  'stopifnot(all(x > 0))','x <- log(x)',if(k=='paired')c('stopifnot(all(y > 0))','y <- log(y)'))
 if(k=='paired')lines<-c(lines,'change <- y - x  # After minus before')
 if(k%in%c('one','paired')){
  v<-if(k=='paired')'change'else'x';lines<-c(lines,paste0('summary(',v,')'),paste0('sd(',v,')'),paste0('plot_data <- data.frame(value = ',v,')'),
   'plot_main <- ggplot(plot_data, aes(x = value)) +', '  geom_histogram(bins = 15, colour = "white", fill = "#087f8c") +',paste0('  labs(x = ',r_string(if(isTRUE(s$log)){if(k=='paired')paste0('log(',s$y,') minus log(',s$x,')')else paste('log(',s$x,')')}else if(k=='paired')paste(s$y,'minus',s$x)else s$x),', y = "Observations") +'),'  theme_minimal()')
 }else if(k%in%c('two','many'))lines<-c(lines,'tapply(x, group, summary)','tapply(x, group, var)  # Variance in each group',
  'plot_data <- data.frame(value = x, group = group)',
  'plot_main <- ggplot(plot_data, aes(x = group, y = value)) +','  geom_boxplot(fill = "#b8dfd9") +',paste0('  labs(x = ',r_string(s$y),', y = ',r_string(if(isTRUE(s$log))paste('log(',s$x,')')else s$x),') +'),'  theme_minimal()')
 else if(k=='binary')lines<-c(lines,paste0('event <- ',r_string(s$event)),
  'events <- sum(x == event)','total <- length(x)','events / total  # Sample proportion',
  't1 <- table(factor(ifelse(x == event, "Event", "Other"), levels = c("Other", "Event")))','t1',
  'plot_data <- as.data.frame(t1)','plot_main <- ggplot(plot_data, aes(x = Var1, y = Freq)) +','  geom_col(fill = "#087f8c") + labs(x = "Outcome", y = "People") + theme_minimal()')
 else if(k=='categorical')lines<-c(lines,paste0('x <- factor(x, levels = ',r_string(rownames(p$counts)),')'),paste0('y <- factor(y, levels = ',r_string(colnames(p$counts)),')'),
  't1 <- table(x, y)','t1  # Rows are the first variable; columns are the second',
  '100 * prop.table(t1, margin = 1)  # Percentages within rows',
  'expected <- outer(rowSums(t1), colSums(t1)) / sum(t1)','expected  # Counts expected under independence',
  'plot_data <- as.data.frame(100 * prop.table(t1, margin = 1))','plot_main <- ggplot(plot_data, aes(x = x, y = Freq, fill = y)) +',
  '  geom_col(position = "dodge") + labs(y = "Percent within each row") + theme_minimal()')
 else lines<-c(lines,paste0('event <- ',r_string(s$event)),
  'before <- factor(ifelse(x == event, "Event", "Other"), levels = c("Other", "Event"))',
  'after <- factor(ifelse(y == event, "Event", "Other"), levels = c("Other", "Event"))',
  't1 <- table(Before = before, After = after)','t1',
  'plot_data <- as.data.frame(t1)','plot_main <- ggplot(plot_data, aes(x = Before, y = After, fill = Freq)) +',
  '  geom_tile() + geom_text(aes(label = Freq)) + theme_minimal()')
 lines<-c(lines,'print(plot_main)')
 if(h_numeric(k)){
  if(k%in%c('one','paired'))lines<-c(lines,'plot_qq <- ggplot(plot_data, aes(sample = value)) +', '  stat_qq() + stat_qq_line() + theme_minimal()')
  else lines<-c(lines,'plot_qq <- ggplot(plot_data, aes(sample = value)) +','  stat_qq() + stat_qq_line() + facet_wrap(~ group) + theme_minimal()')
  lines<-c(lines,'print(plot_qq)')
 }
 lines<-c(lines,'','# 3. Run the chosen test',paste0('# ',unname(h_methods[m])),paste0('alpha <- ',r_num(s$alpha)),
  paste0('alternative <- ',r_string(s$alternative),'  # Choose direction BEFORE examining results'))
 if(k=='two')lines<-c(lines,paste0('reference <- x[group == ',r_string(levels(p$g)[1]),']'),paste0('comparison <- x[group == ',r_string(levels(p$g)[2]),']'), '# The test compares comparison MINUS reference.')
 mu<-r_num(p$null);v<-if(k=='paired')'change'else'x'
 test<-switch(m,
  t_one=paste0('test <- t.test(x, mu = ',mu,', alternative = alternative, conf.level = 1 - alpha)'),
  t_paired=paste0('test <- t.test(y, x, paired = TRUE, mu = ',mu,', alternative = alternative, conf.level = 1 - alpha)'),
  welch='test <- t.test(comparison, reference, var.equal = FALSE, alternative = alternative, conf.level = 1 - alpha)',
  pooled='test <- t.test(comparison, reference, var.equal = TRUE, alternative = alternative, conf.level = 1 - alpha)',
  signed=paste0('test <- wilcox.test(',v,', mu = ',mu,', alternative = alternative, exact = ',if(h_exact(p,s))'TRUE'else'FALSE',', correct = TRUE, digits.rank = 7)'),
  rank=paste0('test <- wilcox.test(comparison, reference, alternative = alternative, exact = ',if(h_exact(p,s))'TRUE'else'FALSE',', correct = TRUE, digits.rank = 7)'),
  sign=c(paste0('deviation <- ',v,' - ',mu),'deviation <- deviation[deviation != 0]  # Omit exact ties',
   'test <- binom.test(sum(deviation > 0), length(deviation), p = 0.5, alternative = alternative, conf.level = 1 - alpha)'),
  anova='test <- oneway.test(value ~ group, data = plot_data, var.equal = TRUE)',
  welch_anova='test <- oneway.test(value ~ group, data = plot_data, var.equal = FALSE)',
  kruskal='test <- kruskal.test(value ~ group, data = plot_data)',
  binomial=paste0('test <- binom.test(events, total, p = ',r_num(s$null),', alternative = alternative, conf.level = 1 - alpha)'),
  prop=paste0('test <- prop.test(events, total, p = ',r_num(s$null),', alternative = alternative, conf.level = 1 - alpha, correct = FALSE)'),
  chi='test <- chisq.test(t1, correct = FALSE)',
  fisher='test <- fisher.test(t1, alternative = "two.sided", conf.level = 1 - alpha)',
  fisher_mc=c('set.seed(2026)  # Reproduce the same simulated tables','test <- fisher.test(t1, simulate.p.value = TRUE, B = 49999)'),
  mcnemar='test <- mcnemar.test(t1, correct = TRUE)',
  mcnemar_exact=c('became_event <- t1["Other", "Event"]','no_longer_event <- t1["Event", "Other"]',
   'test <- binom.test(became_event, became_event + no_longer_event, p = 0.5, conf.level = 1 - alpha)',
   '# This binomial interval concerns direction among changers, NOT the difference in overall proportions.')
 )
 lines<-c(lines,test,'test','test$p.value  # $ selects the p-value from the test result')
 if(m%in%c('signed','rank'))lines<-c(lines,'# digits.rank = 7 avoids artificial rank differences from computer rounding.',if(h_exact(p,s))'# exact = TRUE uses a small-sample exact calculation without relevant ties.'else'# exact = FALSE uses a normal approximation, with continuity correction. Very small tied samples need care.')
 if(isTRUE(s$log)&&k!='many')lines<-c(lines,'','# Return to the original scale: geometric mean or geometric mean ratio.',
  if(k=='two')'exp(mean(comparison) - mean(reference))'else if(k=='paired')'exp(mean(change))'else'exp(mean(x))','exp(test$conf.int)')
 if(isTRUE(s$posthoc)&&k=='many'){
  adjust<-if(is.null(s$adjust))if(m=='anova')'tukey'else'holm'else s$adjust
  lines<-c(lines,'','# Optional: account for all pairwise comparisons as one family',
   if(adjust=='bonferroni')'# Bonferroni multiplies each raw p-value by the number of pairs, capped at 1.',
   if(adjust=='tukey')c('model <- aov(value ~ group, data = plot_data)','TukeyHSD(model, conf.level = 1 - alpha)')else
   if(m=='kruskal')paste0('pairwise.wilcox.test(x, group, exact = FALSE, correct = TRUE, digits.rank = 7, p.adjust.method = ',r_string(adjust),')')else
   paste0('pairwise.t.test(x, group, pool.sd = ',if(m=='anova')'TRUE'else'FALSE',', p.adjust.method = ',r_string(adjust),')'))
 }
 lines<-c(lines,'','# The p-value is NOT the probability that the null hypothesis is true.',
 '# A non-significant result does not prove equality. Consider size, uncertainty and design.')
 paste(lines,collapse='\n')
}
h_exact_script <- function(meta,steps,s) {
 fn<-c('h_numeric','h_levels','h_prepare','h_exact','h_run','h_wrap','h_plot_width','h_theme','h_plots','h_ptext','h_interpret')
 body<-vapply(fn,function(n)paste0(n,' <- ',paste(deparse(get(n,mode='function')),collapse='\n')),character(1))
 paste(c(full_script(meta,steps),'# Full app calculation and plotting functions',paste0('h_methods <- ',paste(capture.output(dput(h_methods)),collapse='\n')),body,
  paste0('spec <- ',paste(capture.output(dput(s)),collapse='\n')),
  'result <- h_run(study, spec)','print(result$summary)','print(result$counts)','print(result$expected)','print(result$test)','print(result$effect)','print(result$posthoc)',
  'cat(paste(h_interpret(result, spec), collapse = "\n"))','print(result$notes)',
  'figures <- h_plots(result, spec)','print(figures$main)','if (!is.null(figures$hist)) print(figures$hist)','if (!is.null(figures$qq)) print(figures$qq)'),collapse='\n\n')
}
