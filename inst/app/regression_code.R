reg_student_script <- function(meta,steps,s,raw,view=NULL){
 r<-reg_fit(apply_steps(raw,steps),s)
 if(is.null(view))view<-list(focal=s$predictors[1],by='',strata='',effect=s$predictors[1],profile=reg_profile(r))
 lines<-c(learner_preamble(meta,steps),'','# 2. Keep the same complete observations for this model',learner_clean(c(s$outcome,s$predictors)),'nrow(analysis)  # People included','nrow(study) - nrow(analysis)  # Rows excluded')
 for(v in s$categorical)lines<-c(lines,paste0(learner_col(v,'analysis'),' <- factor(',learner_col(v,'analysis'),', levels = ',r_string(levels(r$data[[v]])),')'),paste0('contrasts(',learner_col(v,'analysis'),') <- contr.treatment(levels(',learner_col(v,'analysis'),'))'))
 f<-paste(deparse(reg_formula(s)),collapse=' ')
 lines<-c(lines,'','# 3. Fit a linear regression',
 '# ~ means: explain the outcome on the left using predictors on the right.',
 '# + adds a predictor. : adds an interaction; keep both main predictors too.',
 paste0('model <- lm(',f,', data = analysis)'),
 'summary(model)  # Estimates, standard errors, t tests and model fit',paste0('confint(model, level = ',s$conf,')'),
 '# Test eligible whole terms while respecting interactions (model hierarchy).',
 'drop1(model, test = "F")',
 '# I(category = level) in an equation means 1 for that level and 0 otherwise.')
 if(length(s$predictors)>1&&!length(s$interactions))lines<-c(lines,'','# Compare the focal association before and after adjustment.',
 '# Use analysis for BOTH fits, so differences do not come from different people.',paste0('unadjusted <- lm(',reg_name(s$outcome),' ~ ',reg_name(view$focal),', data = analysis)'),
 'summary(unadjusted)',paste0('confint(unadjusted, level = ',s$conf,')'),'summary(model)',
 '# A change after adjustment can suggest confounding; design and subject knowledge are essential.')
 lines<-c(lines,'','# 4. Check residuals: observed minus fitted values',
 'diagnostics <- data.frame(fitted = fitted(model), residual = residuals(model),',
 '  standardised = rstandard(model), cooks = cooks.distance(model))',
 'ggplot(diagnostics, aes(x = fitted, y = residual)) +',
 '  geom_point() + geom_hline(yintercept = 0, linetype = "dashed") + theme_minimal()',
 'ggplot(diagnostics, aes(sample = standardised)) +',
 '  stat_qq() + stat_qq_line() + theme_minimal()',
 '# Curves or changing spread need attention. Normality concerns residuals, not every predictor.',
 '# Cook\'s distance helps find influential observations; do not delete them automatically.',
 'plot(diagnostics$cooks, type = "h", ylab = "Cook\'s distance", xlab = "Complete observation")',
 '','# 5. Describe one profile: hold other predictors at these chosen values.',
 paste0('profile <- analysis[1, ',r_string(s$predictors),', drop = FALSE]'))
 for(v in s$predictors){val<-view$profile[[v]][1];lines<-c(lines,paste0(learner_col(v,'profile'),' <- ',if(v%in%s$categorical)paste0('factor(',r_string(as.character(val)),', levels = levels(',learner_col(v,'analysis'),'))')else r_num(val)))}
 lines<-c(lines,paste0('predict(model, newdata = profile, interval = "confidence", level = ',s$conf,')'),paste0('predict(model, newdata = profile, interval = "prediction", level = ',s$conf,')'),
 '# Confidence: uncertainty about the mean. Prediction: uncertainty about one new person.',
 '','# 6. Plot predictions from this SAME full model')
 focal<-view$focal;by<-view$by
 xs<-if(focal%in%s$categorical)paste0('levels(',learner_col(focal,'analysis'),')')else paste0('seq(min(',learner_col(focal,'analysis'),'), max(',learner_col(focal,'analysis'),'), length.out = 80)')
 gs<-if(!nzchar(by))NULL else if(by%in%s$categorical)paste0('levels(',learner_col(by,'analysis'),')')else paste0('unique(as.numeric(quantile(',learner_col(by,'analysis'),', c(0.25, 0.5, 0.75))))')
 lines<-c(lines,paste0('plot_data <- expand.grid(',reg_name(focal),' = ',xs,if(nzchar(by))paste0(', ',reg_name(by),' = ',gs),', KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)'))
 for(v in setdiff(s$predictors,c(focal,by)))lines<-c(lines,paste0(learner_col(v,'plot_data'),' <- ',learner_col(v,'profile')))
 for(v in s$categorical)lines<-c(lines,paste0(learner_col(v,'plot_data'),' <- factor(',learner_col(v,'plot_data'),', levels = levels(',learner_col(v,'analysis'),'))'))
 lines<-c(lines,paste0('predictions <- predict(model, newdata = plot_data, interval = "confidence", level = ',s$conf,')'),
 'plot_data <- cbind(plot_data, predictions)',paste0('plot_main <- ggplot(plot_data, aes(x = ',reg_name(focal),', y = fit',if(nzchar(by))paste0(', colour = factor(',reg_name(by),'), group = factor(',reg_name(by),')'),')) +'),
 if(focal%in%s$categorical)c('  geom_point(position = position_dodge(0.45)) +','  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1, position = position_dodge(0.45)) +')else c('  geom_line() +',paste0('  geom_ribbon(aes(ymin = lwr, ymax = upr',if(nzchar(by))paste0(', fill = factor(',reg_name(by),')'),'), alpha = 0.15, colour = NA) +')),
 paste0('  labs(y = ',r_string(paste('Predicted mean',s$outcome)),') + theme_minimal()'),'print(plot_main)')
 # Advanced topic is isolated below the short lm() / summary() / confint() workflow.
 ef<-view$effect;strata<-view$strata
 lines<-c(lines,'','# 7. Optional advanced step: conditional effects from the SAME model',
 '# Compare two model predictions. Their errors are related, so use vcov(model).',
 '# This uses full-model coefficients and uncertainty; no separate group models are fitted.',
 paste0('strata <- ',if(nzchar(strata))paste0('levels(',learner_col(strata,'analysis'),')')else'"All observations"'),
 paste0('values <- ',if(ef%in%s$categorical)paste0('levels(',learner_col(ef,'analysis'),')[-1]')else'1'),
 'effect_table <- data.frame()',
 '# A loop repeats these commands for each stratum and effect.',
 'for (stratum in strata) {','  for (value in values) {','    low <- high <- profile')
 if(nzchar(strata))lines<-c(lines,paste0('    ',learner_col(strata,'low'),' <- ',learner_col(strata,'high'),' <- factor(stratum, levels = levels(',learner_col(strata,'analysis'),'))'))
 if(ef%in%s$categorical)lines<-c(lines,paste0('    ',learner_col(ef,'low'),' <- factor(levels(',learner_col(ef,'analysis'),')[1], levels = levels(',learner_col(ef,'analysis'),'))'),paste0('    ',learner_col(ef,'high'),' <- factor(value, levels = levels(',learner_col(ef,'analysis'),'))'),paste0('    effect <- paste(value, "minus", levels(',learner_col(ef,'analysis'),')[1])'))else lines<-c(lines,paste0('    ',learner_col(ef,'high'),' <- ',learner_col(ef,'low'),' + 1'),paste0('    effect <- ',r_string(paste('Per 1-unit increase in',ef))))
 lines<-c(lines,'    # model.matrix() translates each profile into the terms in the equation.',
 '    terms_only <- delete.response(terms(model))',
 '    A <- model.matrix(terms_only, high, contrasts.arg = model$contrasts, xlev = model$xlevels)',
 '    B <- model.matrix(terms_only, low, contrasts.arg = model$contrasts, xlev = model$xlevels)',
 '    difference <- A - B',
 '    # %*% combines the coefficient terms needed for this contrast.',
 '    estimate <- as.numeric(difference %*% coef(model))',
 '    se <- sqrt(as.numeric(difference %*% vcov(model) %*% t(difference)))',
 paste0('    margin <- qt(',r_num((1+s$conf)/2),', df.residual(model)) * se'),
 '    p <- 2 * pt(-abs(estimate / se), df.residual(model))',
 '    effect_table <- rbind(effect_table, data.frame(Stratum = stratum, Effect = effect,',
 '      Estimate = estimate, SE = se, Lower = estimate - margin, Upper = estimate + margin, p_value = p))',
 '  }','}','print(effect_table)',
 '# These are pointwise intervals and unadjusted tests, not simultaneous comparisons.',
 '# See the exact app script for the complete conditional equations and styled output.')
 paste(lines,collapse='\n')
}
reg_exact_script <- function(meta,steps,s,view){
 fn<-c('reg_name','reg_formula','reg_prepare','reg_fit','reg_profile','reg_set','reg_matrix','reg_linear','reg_effects','reg_conditional','reg_grid','reg_theme','reg_wrap','reg_plot','reg_observed','reg_diagnostic','reg_compare','reg_p','reg_interpret')
 body<-vapply(fn,function(n)paste0(n,' <- ',paste(deparse(get(n,mode='function')),collapse='\n')),character(1))
 paste(c(full_script(meta,steps),body,paste0('spec <- ',paste(capture.output(dput(s)),collapse='\n')),paste0('view <- ',paste(capture.output(dput(view)),collapse='\n')),
 'result <- reg_fit(study, spec)','print(result$coefficients)','print(result$fit_table)','print(result$term_tests)',
 'effects <- reg_effects(result, view$effect, view$strata, view$profile)','print(effects$table)',
 'equations <- reg_conditional(result, view$strata)','print(equations$equations)','print(equations$table)',
 'print(reg_compare(result, view$focal))','print(reg_plot(result, view$focal, view$by, view$profile))',
 'for (kind in c("residual", "qq", "cooks")) print(reg_diagnostic(result, kind))'),collapse='\n\n')
}
