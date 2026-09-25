# Sourced inside the shared server: preparation, navigation and downloads are shared.
h <- reactiveValues(route='one',stage=1,base=NULL,prepared=NULL,log=FALSE,method=NULL,result=NULL,steps=NULL,raw=NULL,meta=NULL,lesson=1,topic='t_one',notice='',draw=0)
clear_hypothesis <- function(){h$stage<-1;h$base<-NULL;h$prepared<-NULL;h$log<-FALSE;h$method<-NULL;h$result<-NULL;h$notice<-'';h$raw<-NULL;h$steps<-NULL;h$meta<-NULL}
observeEvent(input$go_hypothesis,page('h_choices'))
observeEvent(input$h_change_question,page('h_choices'))
for(k in names(h_routes))local({key<-k;observeEvent(input[[paste0('h_choose_',key)]],{clear_hypothesis();h$route<-key;h$lesson<-1;h$topic<-h_example(key)$spec$method;if(!is.null(rv$meta$h_route)&&!length(rv$steps))h_example_load()else{if(!is.null(rv$meta$h_route)&&length(rv$steps))h$notice<-'Your prepared example has been kept. Use Try the worked example instead to load data for this question.';page('h_analysis')}})})
h_example_load <- function(){
 t<-h_example(h$route);tmp<-tempfile(fileext='.csv');readr::write_csv(t$raw,tmp,na='')
 t$meta$h_route<-h$route;rv$draft<-t$raw;rv$draftmeta<-t$meta;rv$draftpath<-tmp;use_draft();unlink(tmp)
 h$base<-t$spec;page('h_analysis')
}
observeEvent(input$h_example,{
 if(!is.null(rv$raw))showModal(modalDialog(title='Use the worked example?',p('This replaces your session data and clears preparation and results. Download your current script first if you need to keep it.'),footer=tagList(modalButton('Keep my data'),actionButton('h_confirm_example','Use example',class='btn-primary'))))else h_example_load()
})
observeEvent(input$h_confirm_example,{removeModal();h_example_load()})
h_jump <- function(i,topic=NULL){h$lesson<-i;if(!is.null(topic))h$topic<-topic;updateTabsetPanel(session,'h_mode',selected='Tutorial')}
observeEvent(input$h_to_tutorial,h_jump(1))
observeEvent(input$h_to_analysis,updateTabsetPanel(session,'h_mode',selected='Analyse'))
observeEvent(input$h_why_design,h_jump(2))
observeEvent(input$h_why_normal,h_jump(3))
observeEvent(input$h_why_log,h_jump(3))
observeEvent(input$h_why_variance,h_jump(3))
observeEvent(input$h_why_expected,h_jump(3))
observeEvent(input$h_why_nonparam,h_jump(4,switch(h$route,one='signed',paired='signed',two='rank',many='kruskal')))
observeEvent(input$h_why_sign,h_jump(4,'sign'))
observeEvent(input$h_why_method,h_jump(4,h$method))
observeEvent(input$h_why_pvalue,h_jump(5))
observeEvent(input$h_why_adjust,h_jump(7))
observeEvent(input$h_tut_why_adjust,h_jump(7))
observeEvent(input$h_lesson_prev,{h$lesson<-max(1,h$lesson-1)})
observeEvent(input$h_lesson_next,{h$lesson<-min(if(h$route=='many')7 else 6,h$lesson+1)})
observeEvent(input$h_tutorial_method,{h$topic<-input$h_tutorial_method})
h_available <- function(k) switch(k,one=c('t_one','signed','sign'),paired=c('t_paired','signed','sign'),two=c('welch','pooled','rank'),many=c('anova','welch_anova','kruskal'),binary=c('binomial','prop'),categorical=c('chi','fisher','fisher_mc'),paired_binary=c('mcnemar_exact','mcnemar'))
h_link <- function(id,label='(Tutorial)')actionButton(id,label,class='btn-link')
h_selected <- function(name,choices,fallback=NULL){old<-isolate(h$base[[name]]);if(length(old)&&old%in%choices)old else if(!is.null(fallback)&&fallback%in%choices)fallback else if(length(choices))choices[1]else''}
output$h_analysis_ui<-renderUI({
 k<-h$route;stage<-h$stage
 if(is.null(rv$raw))return(div(class='panel-card',h3('Begin with data or a worked example'),p('You can learn from the Tutorial without uploading anything. To analyse, import your own data or load the example for this question.'),div(class='nav-actions',actionButton('go_import','Import / prepare data'),actionButton('h_example','Use the worked example',class='btn-primary'),actionButton('h_to_tutorial','Open tutorial'))))
 heading<-tagList(p(class='progress-label',paste('Step',stage,'of 5 ·',c('Define the question','Inspect the observations','Choose assumptions','Review the test','Read the result')[stage])),
  if(nzchar(h$notice))help_box(h$notice,'lesson-note'))
 if(stage==1){
  d<-data();nums<-names(d)[vapply(d,is.numeric,logical(1))];cats<-names(d)[vapply(d,function(x)length(h_levels(x))<=10,logical(1))]
  choices<-if(h_numeric(k))nums else if(k%in%c('binary','paired_binary'))names(d)[vapply(d,function(x)length(h_levels(x))<=2,logical(1))]else cats
  x<-h_selected('x',choices,if(k%in%c('paired','paired_binary'))'before'else if(h_numeric(k))'sleep_hours'else if(k=='binary')'short_sleep'else'programme')
  return(tagList(heading,div(class='two-col',div(class='panel-card',h3('1. Select the measurements'),
   selectInput('h_x',if(k%in%c('paired','paired_binary'))'Before / first measurement'else if(k=='categorical')'First categorical variable (table rows)'else if(k=='binary')'Binary outcome variable'else'Numerical outcome variable',choices,selected=x),
   if(k%in%c('paired','paired_binary','two','many','categorical'))uiOutput('h_y_ui'),
   if(k=='two')uiOutput('h_reference_ui'),
   if(k%in%c('binary','paired_binary'))uiOutput('h_event_ui'),
   if(k%in%c('one','paired'))numericInput('h_null',if(k=='paired')'No-change value: after minus before'else'Value to compare with',value=if(!is.null(isolate(h$base$null)))isolate(h$base$null)else if(k=='one')7 else 0),
   if(k=='binary')numericInput('h_null_percent','Population percentage under the null hypothesis',value=if(!is.null(isolate(h$base$null)))100*isolate(h$base$null)else 50,min=.01,max=99.99),
   tags$details(tags$summary('Direction and significance level'),
    if(k%in%c('one','paired','two','binary'))selectInput('h_alternative','Direction of the question',c('Different in either direction'='two.sided','Greater than the reference'='greater','Less than the reference'='less'),selected=if(!is.null(isolate(h$base$alternative)))isolate(h$base$alternative)else'two.sided'),
    selectInput('h_alpha','Significance level',c('5%'=.05,'1%'=.01,'10%'=.1),selected=if(!is.null(isolate(h$base$alpha)))isolate(h$base$alpha)else .05),
    p(class='small-muted','Choose this rule before examining test results. For paired data, direction concerns after minus before; for two groups it concerns comparison minus reference.')),
   checkboxInput('h_design_ok',if(k%in%c('paired','paired_binary'))'Each row is one person or matched pair, with correctly matched measurements; different pairs are independent.'else'Each row contributes one independent observation; these are not repeated, matched or clustered observations.',FALSE),
   h_link('h_why_design','Why does the study structure matter? (Tutorial)'),
   actionButton('h_inspect','Inspect these data',class='btn-primary')),
   div(class='panel-card',h3('What are we comparing?'),p(unname(h_prompts[k])),
    if(k%in%c('paired','paired_binary'))help_box('Choose measurements of the SAME outcome at two times or conditions. Use the same units, or the same binary labels, in both columns. Pairing is determined by the rows, not by the order of separately sorted columns.'),
    p('The first step is a precise question. We will inspect observations and assumptions before calculating a p-value.'),
    actionButton('h_to_tutorial','Learn this step by step'),hr(),actionButton('h_example','Try the worked example instead'),actionButton('go_import','Import / prepare data')))))
 }
 if(stage==2)return(tagList(heading,div(class='panel-card',h3(if(isTRUE(h$log))'Review the log-transformed measurements'else'2. Look at the observations'),
  p(paste(h$prepared$n,'complete',if(k%in%c('paired','paired_binary'))'pairs'else'observations','used;',h$prepared$excluded,'of',h$prepared$total,'rows omitted because at least one selected value is missing.')),
  if(!is.null(h$prepared$counts))tagList(h4('Observed counts'),p(if(k=='paired_binary')'Rows: before. Columns: after. Event means the category you selected.'else if(k=='categorical')paste('Rows:',h$base$x,'· Columns:',h$base$y)else paste('Event:',h$base$event)),scroll_table('h_counts')),
  if(!is.null(h$prepared$summary))tagList(if(isTRUE(h$log))p('The summaries below are on the natural log scale.'),scroll_table('h_summary')),
  scroll_plot('h_diagnostic',400,h_plot_width(h$prepared)),
  if(h_numeric(k))tagList(
   p(if(k=='paired')'For a paired t-test, judge the shape of the changes (after minus before), not the two columns separately.'else if(k%in%c('two','many'))'Judge each group separately. Boxplots show spread and extremes; use the histograms and Q–Q plots to help judge shape.'else'Use the histogram and Q–Q plot to judge shape as well as the boxplot.'),
   scroll_plot('h_hist',max(340,ceiling(nrow(h$prepared$summary)/2)*250),660),
   tags$details(tags$summary('Look at the normal Q–Q plots'),p('Dots roughly following the line support a normal approximation. Curves, very distant end points and small samples need careful judgement. These pictures do not prove normality.'),scroll_plot('h_qq',max(340,ceiling(nrow(h$prepared$summary)/2)*250),660)),
   radioButtons('h_normal',if(k=='paired')'Are the changes reasonably compatible with a normal shape?'else'Are the relevant distributions reasonably compatible with a normal shape?',c('Choose after inspecting the plots'='','Yes, approximately'='yes','No'='no','I am unsure'='unsure'),selected=''),
   h_link('h_why_normal','Help me read these plots (Tutorial)'),uiOutput('h_normal_options'))else uiOutput('h_count_options'),
  hr(),actionButton('h_back_question','Back to the question'))))
 if(stage==3)return(tagList(heading,div(class='panel-card',
  if(h$method=='signed')tagList(h3('Are the deviations roughly symmetric?'),
   p(if(k=='paired')'Look at the changes around their centre. Are the left and right sides roughly balanced? A signed-rank test needs this symmetry even though it does not need a normal shape.'else'Look at the measurements around their centre. Are the left and right sides roughly balanced? A signed-rank test needs this symmetry even though it does not need a normal shape.'),
   scroll_plot('h_hist',360,660),radioButtons('h_symmetry','Which description fits?',c('Choose after inspecting'='','Approximately symmetric: use signed ranks'='yes','Asymmetric or unsure: use signs only'='no'),selected=''),h_link('h_why_nonparam','Signed-rank test (Tutorial)'),h_link('h_why_sign','Sign test (Tutorial)'),actionButton('h_confirm_symmetry','Continue',class='btn-primary'))else tagList(h3('Compare the group variances'),
   p('Variance measures spread in squared units. Larger values mean a wider spread. Compare these values with the boxplots and group sizes. There is no fixed ratio that proves variances are equal.'),scroll_table('h_summary'),
   scroll_plot('h_diagnostic',390,h_plot_width(h$prepared)),radioButtons('h_equal','Do approximately equal population variances seem reasonable?',c('Choose after comparing'='','Yes: use the equal-variance method'='yes','No: allow unequal variances'='no','Unsure: use Welch'='unsure'),selected=''),
   h_link('h_why_variance','Why compare variances? (Tutorial)'),actionButton('h_confirm_variance','Continue',class='btn-primary')),
  hr(),actionButton('h_back_inspect','Back to the observations'))))
 if(stage==4)return(tagList(heading,div(class='panel-card',h3(unname(h_methods[h$method])),
  p(h_method_lesson(h$method)[1]),h_link('h_why_method',paste0('(',unname(h_methods[h$method]),' tutorial)')),
  help_box(paste('Observations used:',h$prepared$n,'· Significance level:',100*h$base$alpha,'% ·',if(k%in%c('many','categorical','paired_binary'))'Overall / two-sided question'else paste('Direction:',switch(h$base$alternative,two.sided='different in either direction',greater='greater than reference',less='less than reference')))),
  if(k=='two')p(paste('Comparison:',levels(h$prepared$g)[2],'minus reference:',levels(h$prepared$g)[1])),
  if(isTRUE(h$log))tagList(help_box('This test now concerns geometric means or ratios on the original scale. It does not test the original arithmetic means.','warning-note'),checkboxInput('h_log_ok','I understand the question changes when I use logarithms.',FALSE)),
  if(h$method=='signed')p('You selected a roughly symmetric distribution. The signed-rank test does not generally test an arithmetic mean.'),
  if(h$method%in%c('rank','kruskal'))p('Rank tests compare distributions through ranks. A median-only interpretation needs similarly shaped distributions.'),
  actionButton('h_run','Run this test',class='btn-primary'),actionButton('h_back_inspect','Review the observations'))))
 tagList(heading,div(class='panel-card',uiOutput('h_interpretation'),
  if(isTRUE(h$log))p('These descriptive summaries are on the natural log scale. The estimate and interval below are returned to the original scale.'),scroll_table('h_result_summary'),if(!is.null(h$result$counts))scroll_table('h_result_counts'),
  if(!is.null(h$result$effect))tagList(h4('Estimate and uncertainty'),p(if(h$base$alternative=='two.sided')paste0(100*(1-h$base$alpha),'% confidence interval; read the quantity label before interpreting its scale.')else'A one-sided confidence bound is shown; an infinite end means the bound is open in that direction.'),scroll_table('h_effect')),
  tags$details(tags$summary('Read the R test output'),verbatimTextOutput('h_test_output')),
  uiOutput('h_result_notes'),scroll_plot('h_result_plot',420,h_plot_width(h$result)),
  if(k=='many')tagList(h4('Which groups differ?'),p('An overall test does not answer this by itself. Follow-up comparisons account for testing multiple pairs. Their conclusions may differ from the overall test.'),checkboxInput('h_posthoc','Show adjusted pairwise comparisons',FALSE),uiOutput('h_posthoc_ui')),
  h_link('h_why_pvalue','What does this p-value mean? (Tutorial)'),
  code_box('h_code','h_copy','h_download','h_zip'),
  div(class='nav-actions',downloadButton('h_table_download','Download results table'),downloadButton('h_plot_download','Download figure'),actionButton('h_back_inspect','Review test choices'),actionButton('h_back_question','Change variables'))))
})
output$h_y_ui<-renderUI({req(rv$raw,input$h_x);d<-data();k<-h$route
 choices<-if(k=='paired')names(d)[vapply(d,is.numeric,logical(1))]else names(d)[vapply(d,function(v)length(h_levels(v))%in%switch(k,two=2L,many=3:10,paired_binary=1:2,categorical=2:10),logical(1))]
 choices<-setdiff(choices,input$h_x);selected<-h_selected('y',choices,if(k%in%c('paired','paired_binary'))'after'else if(k=='categorical')'short_sleep'else'programme')
 tagList(selectInput('h_y',if(k%in%c('paired','paired_binary'))'After / second measurement'else if(k=='categorical')'Second categorical variable (table columns)'else'Grouping variable',choices,selected=selected),if(!length(choices))help_box('No separate suitable variable is available. Import or prepare another variable, or use the worked example.'))
})
output$h_reference_ui<-renderUI({req(input$h_y);lev<-h_levels(data()[[input$h_y]]);selectInput('h_reference','Reference group (subtracted from the other group)',lev,selected=h_selected('reference',lev,'Usual routine'))})
output$h_event_ui<-renderUI({req(input$h_x);d<-data();v<-d[[input$h_x]];lev<-if(is.factor(v))levels(v)else h_levels(v)
 if(h$route=='paired_binary'&&nonempty(input$h_y))lev<-unique(c(lev,h_levels(d[[input$h_y]])))
 selectizeInput('h_event','Category counted as the event',lev,selected=h_selected('event',lev,'Yes'),options=list(create=length(lev)==1))
})
observeEvent(input$h_inspect,{
 tryCatch({
  if(!isTRUE(input$h_design_ok))stop('Confirm the study structure first. The tutorial explains independence and pairing.')
  k<-h$route;s<-list(route=k,x=input$h_x,y=if(k%in%c('paired','paired_binary','two','many','categorical'))input$h_y else'',reference=if(k=='two')input$h_reference else'',event=if(k%in%c('binary','paired_binary'))input$h_event else'',null=if(k=='binary')input$h_null_percent/100 else if(k%in%c('one','paired'))input$h_null else 0,alpha=as.numeric(input$h_alpha),alternative=if(k%in%c('one','paired','two','binary'))input$h_alternative else'two.sided',log=FALSE,method='',posthoc=FALSE)
  if(is.null(s$null)||!is.finite(s$null))stop('Enter a finite reference value.')
  if(k=='binary'&&(s$null<=0||s$null>=1))stop('Choose a reference percentage strictly between 0 and 100.')
  if(!length(s$x)||!nzchar(s$x))stop('Choose a suitable outcome variable.')
  if(k%in%c('paired','paired_binary','two','many','categorical')&&(!length(s$y)||!nzchar(s$y)))stop('Choose a separate suitable second variable, or load the worked example.')
  h$prepared<-h_prepare(data(),s);h$base<-s;h$steps<-rv$steps;h$raw<-rv$raw;h$meta<-rv$meta;h$log<-FALSE;h$result<-NULL;h$notice<-'';h$stage<-2
 },error=notify_error)
})
h_set_method <- function(m){h$method<-m;h$topic<-m;h$stage<-4}
h_nonparam <- function(message='You chose a non-parametric route on the original measurements.'){
 s<-h$base;s$log<-FALSE;h$prepared<-h_prepare(data(),s);h$log<-FALSE;h$notice<-message
 if(h$route%in%c('one','paired')){h$method<-'signed';h$topic<-'signed';h$stage<-3}else h_set_method(if(h$route=='two')'rank'else'kruskal')
}
output$h_normal_options<-renderUI({
 req(h$stage==2,h_numeric(h$route));a<-input$h_normal
 if(is.null(a)||a=='')return(NULL)
 if(a=='yes')return(tagList(p('Continue to the mean-based test. For independent groups, you will compare variances next.'),actionButton('h_parametric','Continue to a t-test / ANOVA',class='btn-primary')))
 if(isTRUE(h$log))return(help_box('Returning to a non-parametric route on the original scale.'))
 canlog<-all(h$prepared$x>0)&&(h$route!='paired'||all(h$prepared$y>0))&&(h$route!='one'||h$base$null>0)&&(h$route!='paired'||h$base$null==0)
 tagList(p('Consider a non-parametric comparison, or inspect a natural log transformation for positive, right-skewed measurements. Both can change the question being answered.'),
  div(class='nav-actions',actionButton('h_nonparam','Use a non-parametric test',class='btn-primary'),h_link('h_why_nonparam',paste0('(',unname(h_methods[switch(h$route,one='signed',paired='signed',two='rank',many='kruskal')]),' tutorial)'))),
  if(canlog)div(class='nav-actions',actionButton('h_try_log','Try the natural log'),h_link('h_why_log','(Transformation tutorial)'))else p(class='small-muted','The log option requires strictly positive measurements. A one-sample reference must be positive; paired log comparisons here use a zero no-change value. No rows will be dropped to force a log transformation.'))
})
observeEvent(input$h_normal,{if(h$stage==2&&isTRUE(h$log)&&input$h_normal%in%c('no','unsure'))h_nonparam('You judged the log-transformed shape unsuitable or uncertain. We have returned to a non-parametric route using the ORIGINAL measurements. The log transformation is not retained.')},ignoreInit=TRUE)
observeEvent(input$h_try_log,{tryCatch({s<-h$base;s$log<-TRUE;h$prepared<-h_prepare(data(),s);h$log<-TRUE;h$notice<-if(h$route=='paired')'Inspect log(after) minus log(before), which describes within-person ratios.'else'Inspect the transformed shape within each relevant group. These summaries are on the natural log scale.'},error=notify_error)})
observeEvent(input$h_nonparam,h_nonparam())
observeEvent(input$h_parametric,{
 if(!identical(input$h_normal,'yes'))return()
 if(h$route%in%c('two','many')){h$method<-'variance';h$stage<-3}else h_set_method(if(h$route=='paired')'t_paired'else't_one')
})
observeEvent(input$h_confirm_variance,{if(!nonempty(input$h_equal)){showNotification('Compare the variances and choose a response first.',type='warning');return()};h_set_method(if(h$route=='two'){if(input$h_equal=='yes')'pooled'else'welch'}else{if(input$h_equal=='yes')'anova'else'welch_anova'})})
observeEvent(input$h_confirm_symmetry,{if(!nonempty(input$h_symmetry)){showNotification('Choose a symmetry judgement first.',type='warning');return()};h_set_method(if(input$h_symmetry=='yes')'signed'else'sign')})
output$h_count_options<-renderUI({
 p<-h$prepared;k<-h$route;req(p)
 if(k=='paired_binary'){
  b<-p$counts[1,2];c<-p$counts[2,1];choices<-c('Exact McNemar test'='mcnemar_exact',if(b+c>=25)c('McNemar approximation (continuity corrected)'='mcnemar'))
  intro<-paste(b,'changed towards the event and',c,'changed away;',b+c,'discordant pairs. The exact option compares only these two counts. The approximate option is offered from 25 discordant pairs in this teaching route.')
 }else if(k=='binary'){
  choices<-c('Exact binomial test'='binomial',if(all(p$expected>=5))c('One-sample proportion test (approximation)'='prop'))
  intro<-paste('Under the chosen null percentage, the expected counts are',paste(round(p$expected,2),collapse=' and '),'. The approximate option is offered when both are at least 5.')
 }else{
  small<-sum(p$expected<5);two<-all(dim(p$counts)==2)
  choices<-c(if(small==0)c('Pearson chi-squared test'='chi'),if(two)c("Fisher's exact test"='fisher')else c("Fisher's test with simulated p-value"='fisher_mc'))
  intro<-paste(small,'expected cells are below 5. This teaching route offers chi-squared only when every expected cell is at least 5. Fisher’s option uses the same observed table.')
 }
 tagList(if(k=='categorical')tagList(h4('Counts expected under independence'),p('Compare this with the observed table above. The row and column totals are retained; the association is removed.'),scroll_table('h_expected')),help_box(intro),h_link('h_why_expected','Why do these counts matter? (Tutorial)'),
 selectInput('h_count_method','Choose an approach',choices),actionButton('h_count_continue','Continue',class='btn-primary'))
})
observeEvent(input$h_count_continue,{req(input$h_count_method);h_set_method(input$h_count_method)})
h_active_spec<-reactive({req(h$base);s<-h$base;s$log<-isTRUE(h$log);s$method<-h$method;s$posthoc<-h$stage==5&&h$route=='many'&&isTRUE(input$h_posthoc);s$adjust<-if(nonempty(input$h_adjust)&&input$h_adjust%in%c('holm','bonferroni',if(s$method=='anova')'tukey'))input$h_adjust else if(s$method=='anova')'tukey'else'holm';s})
observeEvent(input$h_run,{tryCatch({if(isTRUE(h$log)&&!isTRUE(input$h_log_ok))stop('Confirm that you understand the question changes on the log scale.');s<-h_active_spec();h$result<-h_run(data(),s);h$stage<-5},error=notify_error)})
observeEvent(input$h_back_question,{h$stage<-1;h$result<-NULL;h$notice<-''})
observeEvent(input$h_back_inspect,{s<-h$base;s$log<-FALSE;h$prepared<-h_prepare(data(),s);h$log<-FALSE;h$result<-NULL;h$notice<-'';h$stage<-2})
h_result<-reactive({req(h$result);if(h$route=='many'&&isTRUE(input$h_posthoc))h_run(data(),h_active_spec())else h$result})
h_figures<-reactive({req(h$prepared,h$base);s<-h$base;s$log<-h$log;h_plots(h$prepared,s)})
output$h_diagnostic<-renderPlot(h_figures()$main,res=96)
output$h_hist<-renderPlot(h_figures()$hist,res=96)
output$h_qq<-renderPlot(h_figures()$qq,res=96)
output$h_summary<-renderTable(h$prepared$summary,digits=3,striped=TRUE)
output$h_counts<-renderTable({p<-h$prepared;if(is.matrix(p$counts))as.data.frame.matrix(p$counts)else data.frame(Category=names(p$counts),Count=as.integer(p$counts))},rownames=reactive(is.matrix(h$prepared$counts)),striped=TRUE)
output$h_expected<-renderTable(as.data.frame.matrix(h$prepared$expected),digits=2,rownames=TRUE,striped=TRUE)
output$h_interpretation<-renderUI({r<-h_result();tagList(h3('What do these results tell us?'),lapply(h_interpret(r,r$spec),p))})
output$h_result_summary<-renderTable(h_result()$summary,digits=3,striped=TRUE)
output$h_result_counts<-renderTable({r<-h_result();if(is.matrix(r$counts))as.data.frame.matrix(r$counts)else data.frame(Category=names(r$counts),Count=as.integer(r$counts))},rownames=reactive(is.matrix(h_result()$counts)),striped=TRUE)
output$h_effect<-renderTable(h_result()$effect,digits=4,striped=TRUE)
output$h_test_output<-renderPrint(h_result()$test)
output$h_result_notes<-renderUI({tagList(lapply(h_result()$notes,function(n)help_box(n)))})
output$h_result_plot<-renderPlot(h_plots(h_result(),h_result()$spec)$main,res=96)
output$h_posthoc_ui<-renderUI({if(!isTRUE(input$h_posthoc))return(NULL);tagList(selectInput('h_adjust','Correction for the family of all group pairs',c(if(h$method=='anova')c('Tukey simultaneous comparisons'='tukey'),'Holm correction'='holm','Bonferroni correction'='bonferroni'),selected=if(h$method=='anova')'tukey'else'holm'),h_link('h_why_adjust','Why adjust? Bonferroni, Holm and Tukey (Tutorial)'),scroll_table('h_posthoc_table'),downloadButton('h_posthoc_download','Download pairwise comparisons'),p('Adjusted p-values account for all pairwise comparisons shown. These are follow-up analyses, not a reason to search repeatedly for significance.'))})
output$h_posthoc_table<-renderTable({a<-h_result()$posthoc;names(a)<-gsub('_',' ',names(a));a},digits=5,striped=TRUE)
h_code<-reactive({req(h$result);h_student_script(h$meta,h$steps,h_active_spec(),h$raw)})
output$h_code<-renderText(h_code())
observeEvent(input$h_copy,session$sendCustomMessage('copyCode',h_code()))
output$h_download<-downloadHandler(filename=function()'hypothesis_analysis.R',content=function(file)writeLines(h_code(),file,useBytes=TRUE))
output$h_download_exact<-downloadHandler(filename=function()'hypothesis_exact.R',content=function(file)writeLines(h_exact_script(h$meta,h$steps,h_active_spec()),file,useBytes=TRUE))
output$h_zip<-downloadHandler(filename=function()'hypothesis_analysis.zip',content=function(file)make_zip(file,h_code(),rv$source,h$meta$name,h_exact_script(h$meta,h$steps,h_active_spec())) )
output$h_table_download<-downloadHandler(filename=function()'hypothesis_result.csv',content=function(file){
 r<-h_result();t<-data.frame(Test=unname(h_methods[r$spec$method]),n=r$n,excluded=r$excluded,statistic=if(is.null(r$test$statistic))NA_real_ else unname(r$test$statistic)[1],degrees_of_freedom=paste(unname(r$test$parameter),collapse=', '),p_value=r$test$p.value,alpha=r$spec$alpha,alternative=r$spec$alternative,log_scale=r$spec$log)
 if(!is.null(r$effect))t<-cbind(t,r$effect);readr::write_csv(t,file)
})
output$h_posthoc_download<-downloadHandler(filename=function()'hypothesis_pairwise_comparisons.csv',content=function(file){req(h_result()$posthoc);readr::write_csv(h_result()$posthoc,file)})
output$h_plot_download<-downloadHandler(filename=function()'hypothesis_figure.png',content=function(file)ggsave(file,h_plots(h_result(),h_result()$spec)$main,width=9,height=6,dpi=300,bg='white'))

# Tutorial data and choices remain separate from the learner's analysis.
h_tutorial_state<-reactive({t<-h_example(h$route);m<-h$topic;if(h$route=='categorical'&&m=='fisher_mc')t$raw<-rbind(t$raw,data.frame(programme=rep('Sleep workshop',100),response=c(rep('Yes',50),rep('No',50))));if(m%in%h_available(h$route))t$spec$method<-m;t$spec$posthoc<-h$route=='many';t$result<-h_run(t$raw,t$spec);t})
output$h_tutorial_ui<-renderUI({
 i<-h$lesson;k<-h$route
 tagList(div(class='nav-actions',actionButton('h_to_analysis','Return to my analysis')),
  div(class='stepbar',actionButton('h_lesson_prev','← Previous'),span(class='progress-label',paste('Tutorial step',i,'of',if(k=='many')7 else 6)),actionButton('h_lesson_next','Next →')),
  div(class='panel-card lesson-copy',h3(h_lesson_titles[i]),
   if(i==4)tagList(selectInput('h_tutorial_method','Learn about this test',setNames(h_available(k),h_methods[h_available(k)]),selected=isolate(h$topic)),uiOutput('h_method_text'))else if(i==7)h_multiple_lesson()else lapply(h_lesson_text(k,i),p)),
  if(i==7)tagList(scroll_plot('h_multiple_plot',390,660),tags$details(tags$summary('Show the R code for this example'),tags$pre('p <- c(0.01, 0.03, 0.20)
p.adjust(p, method = "bonferroni")
p.adjust(p, method = "holm")')))else if(i==5)tagList(div(class='panel-card',h3('Try 100 imaginary studies'),p('Each study samples independent people with normally distributed sleep, SD 1.5 hours. Every study performs a two-sided one-sample t-test against 7 hours, using a 5% threshold. These are simulated studies, separate from your data and the route’s worked example.'),
   selectInput('h_sim_n','People per study',c(10,20,50,100),selected=20),selectInput('h_sim_truth','True population mean used to create the data',c('7 hours: null is true'=7,'7.5 hours: a small difference'=7.5,'8 hours: a larger difference'=8),selected=7),actionButton('h_sim_draw','Draw 100 new studies'),uiOutput('h_sim_text'),scroll_plot('h_sim_plot',530,660),
   tags$details(tags$summary('Show R code for this simulation'),downloadButton('h_sim_download','Download simulation script'),verbatimTextOutput('h_sim_code'))))else tagList(
   div(class='panel-card',h3('Worked example: fictional observations'),p('This example is separate from your uploaded data. Read the observations first, then connect the test result to the question.'),
    uiOutput('h_tutorial_context'),scroll_table('h_tutorial_summary'),uiOutput('h_tutorial_counts_ui'),scroll_plot('h_tutorial_plot',420,660),
    if(i==3&&h_numeric(k))tags$details(tags$summary('See this example’s normal Q–Q plot'),p('Look for an approximately straight pattern. Small departures are common in small samples; pronounced curves or isolated end points deserve attention.'),scroll_plot('h_tutorial_qq',420,660)),
    if(i==6)tagList(uiOutput('h_tutorial_interpret'),scroll_table('h_tutorial_effect'),tags$details(tags$summary('R test output'),verbatimTextOutput('h_tutorial_test')),uiOutput('h_tutorial_notes'),if(k=='many')tagList(h4('Adjusted follow-up comparisons'),p('The Method column names the correction. Compare Adjusted p with the chosen significance level.'),h_link('h_tut_why_adjust','Understand Bonferroni, Holm and Tukey (Tutorial)'),scroll_table('h_tutorial_posthoc')))),
   code_box('h_tutorial_code','h_tutorial_copy','h_tutorial_download','h_tutorial_zip')))
})
output$h_method_text<-renderUI({tagList(lapply(h_method_lesson(h$topic),p))})
output$h_tutorial_context<-renderUI({t<-h_tutorial_state();s<-t$spec;tagList(p(paste('Method:',unname(h_methods[s$method]),'· Complete observations:',t$result$n)),if(s$method=='mcnemar')help_box('This worked table has only 15 discordant pairs. The exact method is preferred for this example; the approximation is shown here for comparison only.'),if(s$method=='fisher_mc')p('For the simulated Fisher example, a third group, Sleep workshop, has 50 Yes responses among 100 people (50%).'),p(switch(h$route,one='These 12 fictional adults have recorded sleep hours. We compare the population centre with 7 hours.',paired='These 12 fictional people have before and after sleep measurements. The figure shows their after-minus-before changes.',two='There are 12 different people in each programme. The comparison is Walking programme minus Usual routine.',many='There are 12 different people in each of three programmes. Start with an overall comparison.',binary='There are 14 Yes responses among 40 people: 35%. We compare the population Yes proportion with 50%.',categorical='There are 40 Yes responses among 100 in Usual routine and 60 among 100 in Walking programme: 40% and 60%.',paired_binary='Among 60 pairs, 12 change from No to Yes and 3 from Yes to No. Before, 18/60 (30%) are Yes; after, 27/60 (45%) are Yes.')))} )
output$h_tutorial_summary<-renderTable(h_tutorial_state()$result$summary,digits=3,striped=TRUE)
output$h_tutorial_counts_ui<-renderUI({r<-h_tutorial_state()$result;if(is.null(r$counts))return(NULL);tagList(h4('Observed counts'),scroll_table('h_tutorial_counts'),if(h$route=='categorical')tagList(h4('Expected counts under independence'),scroll_table('h_tutorial_expected')))})
output$h_tutorial_counts<-renderTable({r<-h_tutorial_state()$result;if(is.matrix(r$counts))as.data.frame.matrix(r$counts)else data.frame(Category=names(r$counts),Count=as.integer(r$counts))},rownames=reactive(is.matrix(h_tutorial_state()$result$counts)),striped=TRUE)
output$h_tutorial_expected<-renderTable(as.data.frame.matrix(h_tutorial_state()$result$expected),digits=2,rownames=TRUE,striped=TRUE)
output$h_tutorial_plot<-renderPlot({t<-h_tutorial_state();p<-h_plots(t$result,t$spec);if(h$lesson==3&&h_numeric(h$route))p$hist else p$main},res=96)
output$h_tutorial_interpret<-renderUI({t<-h_tutorial_state();tagList(lapply(h_interpret(t$result,t$spec),p))})
output$h_tutorial_effect<-renderTable(h_tutorial_state()$result$effect,digits=4,striped=TRUE)
output$h_tutorial_qq<-renderPlot({t<-h_tutorial_state();h_plots(t$result,t$spec)$qq},res=96)
output$h_tutorial_posthoc<-renderTable(h_tutorial_state()$result$posthoc,digits=5,striped=TRUE)
output$h_tutorial_test<-renderPrint(h_tutorial_state()$result$test)
output$h_tutorial_notes<-renderUI({tagList(lapply(h_tutorial_state()$result$notes,function(n)help_box(n)))})
h_tutorial_code<-reactive({t<-h_tutorial_state();h_student_script(t$meta,list(),t$spec,t$raw)})
output$h_tutorial_code<-renderText(h_tutorial_code())
observeEvent(input$h_tutorial_copy,session$sendCustomMessage('copyCode',h_tutorial_code()))
output$h_tutorial_download<-downloadHandler(filename=function()'hypothesis_example.R',content=function(file)writeLines(h_tutorial_code(),file,useBytes=TRUE))
output$h_tutorial_download_exact<-downloadHandler(filename=function()'hypothesis_example_exact.R',content=function(file){t<-h_tutorial_state();writeLines(h_exact_script(t$meta,list(),t$spec),file,useBytes=TRUE)})
output$h_tutorial_zip<-downloadHandler(filename=function()'hypothesis_example.zip',content=function(file){t<-h_tutorial_state();tmp<-tempfile(fileext='.csv');on.exit(unlink(tmp));readr::write_csv(t$raw,tmp);make_zip(file,h_tutorial_code(),tmp,t$meta$name,h_exact_script(t$meta,list(),t$spec))})
observeEvent(input$h_sim_draw,{h$draw<-h$draw+1})
h_sim_state<-reactive({h_simulation(if(is.null(input$h_sim_n))20 else as.integer(input$h_sim_n),if(is.null(input$h_sim_truth))7 else as.numeric(input$h_sim_truth),2026+h$draw)})
output$h_sim_plot<-renderPlot(h_sim_state()$plot,res=96)
output$h_sim_text<-renderUI({p(paste(h_sim_state()$reject,'of these 100 studies have p < 0.05.',if(is.null(input$h_sim_truth)||as.numeric(input$h_sim_truth)==7)'Here the null is true, so those are false positives. A new batch need not give the same number.'else'Here the null is false. The highlighted fraction estimates power for this particular setting.'))})
output$h_sim_code<-renderText(h_sim_state()$code)
output$h_sim_download<-downloadHandler(filename=function()'hypothesis_simulation.R',content=function(file)writeLines(h_sim_state()$code,file,useBytes=TRUE))

output$h_multiple_plot<-renderPlot(h_multiple_plot(),res=96)
