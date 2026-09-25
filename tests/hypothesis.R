library(mmbslearn)
library(dplyr)
library(ggplot2)
app<-system.file('app',package='mmbslearn')
for(f in c('engine.R','learner_code.R','hypothesis.R','hypothesis_code.R','hypothesis_lessons.R'))source(file.path(app,f))
eq<-function(a,b)stopifnot(isTRUE(all.equal(unname(a),unname(b),tolerance=1e-10)))
fail<-function(expr)stopifnot(inherits(try(expr,silent=TRUE),'try-error'))
methods<-list(one=c('t_one','signed','sign'),paired=c('t_paired','signed','sign'),two=c('welch','pooled','rank'),many=c('anova','welch_anova','kruskal'),binary=c('binomial','prop'),categorical=c('chi','fisher','fisher_mc'),paired_binary=c('mcnemar','mcnemar_exact'))
td<-tempfile();dir.create(td);dir.create(file.path(td,'data'));old<-getwd();setwd(td)
for(k in names(methods))for(m in methods[[k]]){
 t<-h_example(k);s<-t$spec;s$method<-m;s$posthoc<-k=='many'
 if(m=='fisher_mc')t$raw<-rbind(t$raw,data.frame(programme=rep('Third',20),response=rep(c('Yes','No'),10)))
 r<-h_run(t$raw,s);stopifnot(is.finite(r$test$p.value),r$test$p.value>=0,r$test$p.value<=1)
 ggplot_build(h_plots(r,s)$main)
 readr::write_csv(t$raw,file.path('data',t$meta$name))
 e<-new.env();eval(parse(text=h_student_script(t$meta,list(),s,t$raw)),e);eq(e$test$p.value,r$test$p.value)
 if(!is.null(r$posthoc))stopifnot(all(r$posthoc$Adjusted_p>=0&r$posthoc$Adjusted_p<=1))
 cat('PASS test, plot and standalone student script:',k,m,'\n')
}
# Known counts: two independent groups, preserving the two-way table.
t<-h_example('categorical');r<-h_run(t$raw,t$spec);eq(r$test$statistic,8);eq(r$test$p.value,pchisq(8,1,lower.tail=FALSE));stopifnot(all(r$expected==50))
t<-h_example('paired_binary');r<-h_run(t$raw,t$spec);eq(r$test$p.value,binom.test(12,15,.5)$p.value);stopifnot(r$counts[1,2]==12,r$counts[2,1]==3)
# Pairing is complete-case by row, and uses after minus before.
t<-h_example('paired');t$raw$before[1]<-NA;t$raw$after[2]<-NA;r<-h_run(t$raw,t$spec);a<-t$raw[complete.cases(t$raw),];eq(r$test$p.value,t.test(a$after-a$before,mu=0)$p.value);eq(r$effect$Estimate,mean(a$after-a$before));stopifnot(r$n==10,r$excluded==2)
# Reversing reference changes signs and one-sided direction correctly.
t<-h_example('two');s<-t$spec;s$alternative<-'greater';r<-h_run(t$raw,s);s2<-s;s2$reference<-'Walking programme';s2$alternative<-'less';r2<-h_run(t$raw,s2);eq(r$test$p.value,r2$test$p.value);eq(r$effect$Estimate,-r2$effect$Estimate)
# Log test reports a geometric mean ratio, not an arithmetic difference.
for(k in c('one','paired','two','many')){
 t<-h_example(k);s<-t$spec;s$log<-TRUE;r<-h_run(t$raw,s)
 if(k=='paired')eq(r$effect$Estimate,exp(mean(log(t$raw$after/t$raw$before))))
 if(k=='two')eq(r$effect$Estimate,exp(mean(log(t$raw$sleep_hours[t$raw$programme=='Walking programme']))-mean(log(t$raw$sleep_hours[t$raw$programme=='Usual routine']))))
 readr::write_csv(t$raw,file.path('data',t$meta$name));e<-new.env();eval(parse(text=h_student_script(t$meta,list(),s,t$raw)),e);eq(e$test$p.value,r$test$p.value)
}
# Full import and preparation, non-syntactic names, and exact app export.
t<-h_example('two');names(t$raw)[1]<-'hours of sleep';s<-t$spec;s$x<-'hours of sleep'
steps<-list(list(kind='filter',join='all',column='',rules=list(list(column='hours of sleep',operator='ge',value='6',numeric=TRUE))))
r<-h_run(apply_steps(t$raw,steps),s);readr::write_csv(t$raw,file.path('data',t$meta$name))
writeLines(h_student_script(t$meta,steps,s,t$raw),'student.R');writeLines(h_exact_script(t$meta,steps,s),'exact.R')
a<-new.env();sys.source('student.R',a);b<-new.env();sys.source('exact.R',b);eq(a$test$p.value,r$test$p.value);eq(b$result$test$p.value,r$test$p.value)
writeLines(c('source("student.R")','stopifnot(is.finite(test$p.value))','source("exact.R")','stopifnot(is.finite(result$test$p.value))'),'fresh.R')
status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','fresh.R'),stdout='fresh.log',stderr='fresh.log',env='R_TESTS=');if(status)stop(paste(readLines('fresh.log'),collapse='\n'))
# Boundary observations and invalid choices produce clear failures.
t<-h_example('binary');t$raw$response<-'No';r<-h_run(t$raw,t$spec);eq(r$effect$Estimate,0);stopifnot(r$effect$Upper>0)
t<-h_example('paired_binary');t$raw$after<-t$raw$before;fail(h_run(t$raw,t$spec))
t<-h_example('paired');t$spec$log<-TRUE;t$raw$before[1]<-0;fail(h_run(t$raw,t$spec))
t<-h_example('one');t$raw$sleep_hours<-7;fail(h_run(t$raw,t$spec));t$spec$method<-'signed';fail(h_run(t$raw,t$spec))
t<-h_example('two');t$spec$y<-t$spec$x;fail(h_prepare(t$raw,t$spec))
t<-h_example('categorical');t$spec$method<-'fisher_mc';fail(h_run(t$raw,t$spec))
# Rank ties are assessed at the same precision used by wilcox.test.
t<-h_example('paired');t$spec$method<-'signed';t$raw<-data.frame(before=c(1.1,2.1,3.1,4.1),after=c(1.2,2.2,3.2,4.2));r<-h_run(t$raw,t$spec);stopifnot(!r$exact)
eq(r$test$p.value,suppressWarnings(wilcox.test(t$raw$after-t$raw$before,exact=FALSE,correct=TRUE,digits.rank=7)$p.value))
# Simulation is reproducible, and its exported script returns the same p-values.
sim<-h_simulation(20,7,2026);e<-new.env();eval(parse(text=sim$code),e);stopifnot(sum(e$p_values<.05)==sim$reject)
setwd(old);unlink(td,recursive=TRUE)
cat('PASS hypothesis edge cases, pairing, orientation, transformations, preparation and exports\n')
# Bonferroni and Holm operate on the whole family of group pairs.
for(m in c('anova','welch_anova','kruskal'))for(adj in c('bonferroni','holm')){
 t<-h_example('many');s<-t$spec;s$method<-m;s$posthoc<-TRUE;s$adjust<-adj;r<-h_run(t$raw,s)
 eq(r$posthoc$Adjusted_p,p.adjust(r$posthoc$Unadjusted_p,method=adj));stopifnot(nrow(r$posthoc)==3,all(r$posthoc$Method==if(adj=='holm')'Holm'else'Bonferroni'))
 p<-h_prepare(t$raw,s);ref<-if(m=='kruskal')pairwise.wilcox.test(p$x,p$g,exact=FALSE,correct=TRUE,digits.rank=7,p.adjust.method=adj)$p.value else pairwise.t.test(p$x,p$g,pool.sd=m=='anova',p.adjust.method=adj)$p.value
 eq(sort(r$posthoc$Adjusted_p),sort(na.omit(as.vector(ref))))
}
cat('PASS Bonferroni and Holm comparisons against base R\n')
