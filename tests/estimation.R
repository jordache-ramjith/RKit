library(mmbslearn)
library(dplyr)
library(ggplot2)
source(system.file('app','engine.R',package='mmbslearn'))
source(system.file('app','estimation.R',package='mmbslearn'))
spec<-function(kind,x,group='',event='',conf=.95)list(kind=kind,x=x,group=group,event=event,conf=conf,unit='')
# Independent reference implementations from R stats.
d<-data.frame(value=c(2,3,5,6,9,NA),group=c('A','A','A','B','B','B'),binary=c('Yes','No','Yes','Yes','No',NA))
for(level in c(.9,.95,.99)){
 a<-run_estimation(d,spec('mean','value',conf=level))$table
 ref<-t.test(d$value,conf.level=level)
 stopifnot(isTRUE(all.equal(c(a$lower,a$upper),as.numeric(ref$conf.int))),a$n==5,a$missing==1,
   isTRUE(all.equal(a$se,sd(d$value,na.rm=TRUE)/sqrt(5))))
 for(n in c(1,5,30,100))for(x in unique(c(0L,1L,floor(n/2),n))){
   ci<-wilson_interval(x,n,level); ref<-suppressWarnings(prop.test(x,n,correct=FALSE,conf.level=level)$conf.int)
   stopifnot(isTRUE(all.equal(ci,as.numeric(ref),tolerance=1e-9)),all(ci>=0 & ci<=1))
 }
}
r<-run_estimation(d,spec('proportion','binary','group','Yes'))
stopifnot(all(r$table$events==c(2,1)),all(r$table$n==c(3,2)),r$table$missing[2]==1)
# Missing group exclusions; all missing and singleton outcomes retained with unavailable intervals.
edge<-data.frame(value=c(NA,NA,2,3,3,8),g=c('empty','empty','single','constant','constant',NA),b=c(NA,NA,'Y','Y','Y','N'))
r<-run_estimation(edge,spec('mean','value','g'))
stopifnot(r$excluded==1,all(is.na(r$table$lower)),r$table$n[r$table$group=='empty']==0)
r<-run_estimation(edge,spec('proportion','b','g','Y'))
stopifnot(is.na(r$table$lower[r$table$group=='empty']),r$table$lower[r$table$group=='constant']<1)
r<-run_estimation(data.frame(b=rep('No',5)),spec('proportion','b',event='Yes'))
stopifnot(r$table$estimate==0,r$table$upper>0)
stopifnot(inherits(try(run_estimation(d,spec('mean','value','value')),silent=TRUE),'try-error'))
stopifnot(inherits(try(run_estimation(data.frame(b=letters[1:3]),spec('proportion','b',event='a')),silent=TRUE),'try-error'))
# Two-way counts and denominators, including a zero cell and missing observation.
ct<-data.frame(a=c('A','A','B',NA),b=c('X','Y','X','Y'))
for(denom in c('row','column','all')){
 s<-list(route='cat2',x='a',y='b',z='',bins=15,measure='percent',denom=denom,cor='pearson')
 r<-run_analysis(ct,s)
 stopifnot(r$counts['Sum','Sum']==3,r$counts['B','Y']==0)
 sums<-switch(denom,row=rowSums(r$percentages),column=colSums(r$percentages),all=sum(r$percentages))
 stopifnot(all(abs(sums-100)<1e-8))
}
# Generated estimation scripts reproduce preparation, estimates and plots without app helpers.
meta<-list(kind='csv',name='study.csv',delim=',',decimal='.',encoding='UTF-8',na=c('','NA'))
steps<-list(list(kind='filter',column='',join='all',rules=list(list(column='value',operator='ge',value='3',numeric=TRUE))))
for(s in list(spec('mean','value'),spec('mean','value','group'),spec('proportion','binary',event='Yes'),spec('proportion','binary','group','No',.99))){
 td<-tempfile();dir.create(td);dir.create(file.path(td,'data'));readr::write_csv(d,file.path(td,'data','study.csv'))
 writeLines(estimation_script(meta,steps,s),file.path(td,'analysis.R'))
 writeLines(c('source("analysis.R")','saveRDS(estimate_table,"table.rds")'),file.path(td,'check.R'))
 wd<-getwd();setwd(td)
 status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','check.R'),stdout='run.log',stderr='run.log',env='R_TESTS=')
 if(status)stop(paste(readLines('run.log'),collapse='\n'))
 actual<-readRDS('table.rds');setwd(wd)
 expected<-run_estimation(apply_steps(d,steps),s)
 stopifnot(isTRUE(all.equal(actual,expected$table)))
 ggplot_build(expected$plot);ggplot_build(expected$diagnostic);unlink(td,recursive=TRUE)
}
# Simulated intervals and reproducibility, including the actual exported code.
for(kind in c('mean','proportion')){
 sim<-run_est_simulation(kind,30,.95,42)
 again<-run_est_simulation(kind,30,.95,42)
 stopifnot(identical(sim$table,again$table),nrow(sim$table)==100)
 e<-new.env();code<-parse(text=sim$code);eval(head(code,-4L),e)
 stopifnot(identical(e$simulation_table,sim$table))
 ggplot_build(sim$sampling);ggplot_build(sim$spread);ggplot_build(sim$coverage)
 if(kind=='mean')stopifnot(isTRUE(all.equal(sim$true_se,1.5/sqrt(30))))
}
cat('PASS: t and Wilson references, edge cases, contingency denominators, clean-session estimation scripts and simulations.\n')
