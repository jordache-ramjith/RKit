library(mmbslearn)
library(ggplot2)
library(dplyr)
app<-system.file('app',package='mmbslearn')
for(f in c('engine.R','learner_code.R','regression.R','regression_code.R','regression_lessons.R'))source(file.path(app,f))
eq<-function(a,b)stopifnot(isTRUE(all.equal(unname(a),unname(b),tolerance=1e-9,check.attributes=FALSE)))
fail<-function(expr)stopifnot(inherits(try(expr,silent=TRUE),'try-error'))
td<-tempfile();dir.create(td);dir.create(file.path(td,'data'));old<-getwd();setwd(td)
for(route in names(reg_routes)){
 t<-reg_example(route);r<-reg_fit(t$raw,t$spec);s<-t$spec
 stopifnot(r$n==120,all(is.finite(r$coefficients$p_value)))
 reference<-lm(reg_formula(s),data=r$data);eq(coef(reference),r$coefficients$Estimate);eq(confint(reference),as.matrix(r$coefficients[c('Lower','Upper')]))
 for(kind in c('residual','qq','cooks'))ggplot_build(reg_diagnostic(r,kind))
 view<-list(focal='sleep_hours',by=if(route=='interaction')'programme'else'',strata=if(route=='interaction')'programme'else'',effect='sleep_hours',profile=reg_profile(r))
 ggplot_build(reg_plot(r,view$focal,view$by,view$profile));ggplot_build(reg_observed(r,'sleep_hours'))
 readr::write_csv(t$raw,file.path('data',t$meta$name));e<-new.env();eval(parse(text=reg_student_script(t$meta,list(),s,t$raw,view)),e)
 eq(coef(e$model),coef(r$fit));eq(e$effect_table$Estimate,reg_effects(r,view$effect,view$strata,view$profile)$table$Estimate);eq(e$effect_table$SE,reg_effects(r,view$effect,view$strata,view$profile)$table$SE)
 writeLines(reg_exact_script(t$meta,list(),s,view),'exact.R');status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','exact.R'),env='R_TESTS=',stdout='exact.log',stderr='exact.log');if(status!=0){cat(readLines('exact.log'),sep='\n');stop('Exact script failed')}
 cat('PASS model, plots, student and fresh-session exact script:',route,'\n')
}
# Reparameterising the FULL model at each reference must give the same slopes/SEs.
t<-reg_example('interaction');r<-reg_fit(t$raw,t$spec);ef<-reg_effects(r,'sleep_hours','programme')$table;eqs<-reg_conditional(r,'programme')
for(g in levels(r$data$programme)){
 s<-t$spec;s$references$programme<-g;ref<-reg_fit(t$raw,s);cc<-summary(ref$fit)$coefficients
 eq(ef$Estimate[ef$Stratum==g],cc['sleep_hours',1]);eq(ef$SE[ef$Stratum==g],cc['sleep_hours',2]);eq(ef$p_value[ef$Stratum==g],cc['sleep_hours',4])
 z<-eqs$table[eqs$table$Stratum==g,];eq(z$Estimate,coef(ref$fit)[c('(Intercept)','sleep_hours','age')]);eq(z$SE,cc[c('(Intercept)','sleep_hours','age'),2])
}
# Numerical-by-numerical plus categorical interaction: conditional slopes depend on profile.
s<-t$spec;s$interactions<-c(s$interactions,list(c('sleep_hours','age')));r<-reg_fit(t$raw,s);pr<-reg_profile(r);pr$age<-40
 ef<-reg_effects(r,'sleep_hours','programme',pr)$table;b<-coef(r$fit);eq(ef$Estimate[1],b['sleep_hours']+40*b['sleep_hours:age'])
 eqs<-reg_conditional(r,'programme');stopifnot(any(grepl('sleep_hours × age',eqs$table$Term,fixed=TRUE)))
 for(g in levels(r$data$programme)){z<-eqs$table[eqs$table$Stratum==g,];nd<-reg_set(r,pr,'programme',g);manual<-sum(z$Estimate*c(1,nd$sleep_hours,nd$age,nd$sleep_hours*nd$age));eq(manual,predict(r$fit,nd))}
# Category-by-category interaction with non-reference contrasts.
t$raw$shift<-rep(c('Day','Night'),60);s<-t$spec;s$predictors<-c('programme','shift','age');s$categorical<-c('programme','shift');s$references<-list(programme='Usual routine',shift='Day');s$interactions<-list(c('programme','shift'));r<-reg_fit(t$raw,s)
ef<-reg_effects(r,'programme','shift')$table
for(g in levels(r$data$shift))for(v in levels(r$data$programme)[-1]){pr<-reg_profile(r);a<-reg_set(r,reg_set(r,pr,'shift',g),'programme',v);b<-reg_set(r,reg_set(r,pr,'shift',g),'programme','Usual routine');eq(ef$Estimate[ef$Stratum==g&ef$Effect==paste(v,'minus Usual routine')],predict(r$fit,a)-predict(r$fit,b))}
view<-list(focal='programme',by='shift',strata='shift',effect='programme',profile=reg_profile(r));ggplot_build(reg_plot(r,view$focal,view$by,view$profile));readr::write_csv(t$raw,file.path('data',t$meta$name));e<-new.env();eval(parse(text=reg_student_script(t$meta,list(),s,t$raw,view)),e);eq(e$effect_table$Estimate,ef$Estimate);eq(e$effect_table$SE,ef$SE)
# Missingness, common sample for adjustment, singularity and unsupported outcomes.
t<-reg_example('adjusted');t$raw$age[c(2,8)]<-NA;r<-reg_fit(t$raw,t$spec);stopifnot(r$n==118,r$excluded==2,identical(r$row_id,setdiff(1:120,c(2,8))))
c<-reg_compare(r,'sleep_hours');a<-lm(wellbeing_score~sleep_hours,data=r$data);eq(c$Estimate[1],coef(a)['sleep_hours']);eq(c$Estimate[2],coef(r$fit)['sleep_hours'])
bad<-t$raw;bad$wellbeing_score<-as.numeric(bad$wellbeing_score>50);fail(reg_fit(bad,t$spec));bad<-t$raw;bad$age<-bad$sleep_hours;fail(reg_fit(bad,t$spec))
# Non-syntactic names and preparation survive complete script export.
t<-reg_example('simple');names(t$raw)[1:2]<-c('score [0-100]','sleep hours');t$spec$outcome<-'score [0-100]';t$spec$predictors<-'sleep hours';step<-list(kind='filter',column='',join='all',rules=list(list(column='age',operator='ge',value='30',numeric=TRUE)))
r<-reg_fit(apply_steps(t$raw,list(step)),t$spec);view<-list(focal='sleep hours',by='',strata='',effect='sleep hours',profile=reg_profile(r));readr::write_csv(t$raw,file.path('data',t$meta$name));e<-new.env();eval(parse(text=reg_student_script(t$meta,list(step),t$spec,t$raw,view)),e);eq(coef(e$model),coef(r$fit));stopifnot(nrow(e$analysis)==r$n)
for(i in 1:7){t<-reg_lesson_state(i);stopifnot(length(reg_lesson_text(i,t))>=4);ggplot_build(reg_plot(t$result,t$view$focal,t$view$by,t$view$profile))}
# Tutorial equations must reproduce the original full models, not separate fits.
t<-reg_lesson_state(3);w<-reg_dummy_worked(t)
eq(w$means,as.numeric(tapply(t$result$data$wellbeing_score,t$result$data$programme,mean)))
eq(w$means,c(55.95,55.9825,59.6775))
stopifnot(identical(w$coding$x_1,c(0L,1L,0L)),identical(w$coding$x_2,c(0L,0L,1L)))
t<-reg_lesson_state(5);w<-reg_interaction_worked(t)
for(z in w$steps){nd<-expand.grid(sleep_hours=c(5,7,9),age=c(25,40,60));nd$programme<-factor(z$group,levels=levels(t$result$data$programme));eq(z$intercept+z$slope*nd$sleep_hours+w$age*nd$age,predict(t$result$fit,nd))}
t<-reg_lesson_state(4);stopifnot(identical(t$spec$predictors,c('sleep_hours','age')),!any(grepl('interaction',c(reg_lesson_text(4,t),reg_lesson_student_script(4,t)),ignore.case=TRUE)))
invisible(ggplot_build(reg_confounding_plot(t)))
stopifnot(length(reg_confounding_text(t))==5,any(grepl("0.0116",reg_interaction_text(reg_lesson_state(5)),fixed=TRUE)))
for(i in c(3,4,5)){
 t<-reg_lesson_state(i);readr::write_csv(t$raw,file.path('data',t$meta$name))
 for(kind in c('student','exact')){
  code<-if(kind=='student')reg_lesson_student_script(i,t)else reg_lesson_exact_script(i,t)
  writeLines(code,'tutorial.R');status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','tutorial.R'),env='R_TESTS=',stdout='tutorial.log',stderr='tutorial.log')
  if(status!=0){cat(readLines('tutorial.log'),sep='\n');stop(paste('Tutorial script failed:',i,kind))}
 }
 cat('PASS tutorial algebra and fresh-session downloads:',i,'\n')
}
setwd(old);unlink(td,recursive=TRUE)
cat('PASS full-model conditional equations, covariance, category/numeric interactions, edge cases, preparation and tutorials\n')
