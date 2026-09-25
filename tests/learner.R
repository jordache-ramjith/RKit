library(mmbslearn);library(dplyr);library(ggplot2)
for(f in c('engine.R','estimation.R','comparisons.R','learner_code.R'))source(system.file('app',f,package='mmbslearn'))
meta<-list(kind='csv',name='study.csv',delim=',',decimal='.',encoding='UTF-8',na=c('','NA'))
d<-read_source(system.file('extdata','wellbeing.csv',package='mmbslearn'),meta)
td<-tempfile();dir.create(td);dir.create(file.path(td,'data'));readr::write_csv(d,file.path(td,'data','study.csv'))
root<-getwd();setwd(td)
run<-function(code){e<-new.env();eval(parse(text=code),e);e}
for(route in c('cat','num','group','cat2','num2')){
 s<-list(route=route,x=switch(route,cat='programme',num='sleep_hours',group='sleep_hours',cat2='programme',num2='sleep_hours'),y=switch(route,group='programme',cat2='smoking',num2='wellbeing_score',''),z='',measure='percent',denom='row',cor='pearson',bins=15)
 e<-run(learner_script(meta,list(),s,d));r<-run_analysis(d,s)
 if(route=='cat')stopifnot(identical(as.numeric(e$t1),as.numeric(r$table$n)))
 if(route=='cat2')stopifnot(isTRUE(all.equal(as.numeric(e$percentages),as.numeric(r$percentages))))
 if(route=='num2')stopifnot(isTRUE(all.equal(e$correlation,r$table$correlation)))
 ggplot_build(e$plot_main)
 cat('PASS learner descriptive',route,'\n')
}
if(requireNamespace('DescTools',quietly=TRUE)&&requireNamespace('epitools',quietly=TRUE)){
for(kind in c('mean','proportion'))for(group in c('','programme'))for(compare in c(FALSE,TRUE)){
 if(compare&&!nzchar(group))next
 s<-list(kind=kind,x=if(kind=='mean')'sleep_hours'else'short_sleep',group=group,event='Yes',compare=compare,reference='Usual routine',design='cross_sectional',unit='hours',conf=.95)
 e<-run(learner_estimation_script(meta,list(),s,d));r<-run_estimation(d,s)
 mult<-if(kind=='mean')1 else 100
 stopifnot(isTRUE(all.equal(as.numeric(e$estimate_table$estimate),r$table$estimate*mult)),isTRUE(all.equal(e$estimate_table$lower,r$table$lower*mult,tolerance=1e-8)),isTRUE(all.equal(e$estimate_table$upper,r$table$upper*mult,tolerance=1e-8)))
 if(compare){stopifnot(isTRUE(all.equal(e$comparison_table$estimate,r$comparison$estimate)),isTRUE(all.equal(e$comparison_table$lower,r$comparison$lower,tolerance=1e-8)),isTRUE(all.equal(e$comparison_table$upper,r$comparison$upper,tolerance=1e-8)))}
 cat('PASS learner estimation',kind,group,compare,'\n')
}
for(kind in c('mean','proportion')){
 e<-run(learner_simulation_script(kind,30,.95,2026,4));r<-run_est_simulation(kind,30,.95,2026)
 mult<-if(kind=='mean')1 else 100
 stopifnot(isTRUE(all.equal(e$simulation_table$estimate,r$table$estimate*mult)),isTRUE(all.equal(e$simulation_table$lower,r$table$lower*mult,tolerance=1e-8)))
 cat('PASS learner simulation',kind,'\n')
}
}
setwd(root)

# Every preparation operation used in student scripts agrees with the existing engine.
steps<-list(
 list(kind='type',column='programme',type='categorical'),
 list(kind='filter',join='any',column='',rules=list(list(column='age',operator='ge',value='40',numeric=TRUE),list(column='programme',operator='eq',value='Usual routine',numeric=FALSE))),
 list(kind='derive',column='marker',operation='log',name='log_marker'),
 list(kind='derive',column='age',operation='threshold',name='age_group',number=40,above='Older',below='Younger'),
 list(kind='derive',column='age',operation='scale',name='twice_age',number=2),
 list(kind='derive',column='age',operation='add',name='next_age',number=1),
 list(kind='derive',column='twice_age',operation='subtract',other='age',name='age_difference'),
 list(kind='derive',column='twice_age',operation='ratio',other='age',name='age_ratio'),
 list(kind='derive',column='programme',operation='recode',name='renamed',old='Usual routine',new='Reference'))
setwd(td);e<-run(learner_script(meta,steps));setwd(root)
stopifnot(isTRUE(all.equal(as.data.frame(e$study),apply_steps(d,steps),check.attributes=FALSE)))
# Full student script executes in a fresh process, without sourced app helpers.
s<-list(route='cat',x='programme',y='',z='',measure='count',denom='row',cor='pearson',bins=15)
writeLines(learner_script(meta,steps,s,d),file.path(td,'student.R'))
writeLines(c('source("student.R")','saveRDS(list(data=study,counts=t1),"student.rds")'),file.path(td,'check.R'))
setwd(td);status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','check.R'),stdout='run.log',stderr='run.log',env='R_TESTS=')
if(status)stop(paste(readLines('run.log'),collapse='\n'))
e<-readRDS('student.rds');setwd(root)
stopifnot(sum(e$counts)==nrow(e$data))
# Safely quote unusual names; percentages update with all three denominator choices.
u<-d;names(u)[names(u)=='programme']<-'group name';names(u)[names(u)=='smoking']<-'smoking ` category'
readr::write_csv(u,file.path(td,'data','study.csv'))
for(denom in c('row','column','all')){
 s<-list(route='cat2',x='group name',y='smoking ` category',z='',measure='percent',denom=denom,cor='pearson',bins=15)
 setwd(td);e<-run(learner_script(meta,list(),s,u));setwd(root);ref<-run_analysis(u,s)
 stopifnot(isTRUE(all.equal(as.numeric(e$percentages),as.numeric(ref$percentages))))
}
unlink(td,recursive=TRUE)
cat('PASS student preparation, standalone execution, unusual names and denominators.\n')
