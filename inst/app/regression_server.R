reg_display<-function(d){if(is.null(d))return(NULL);for(v in names(d)[grepl('(^p_value$|_p$)',names(d))])d[[v]]<-reg_p(d[[v]]);names(d)<-gsub('_',' ',names(d));d}
greg<-reactiveValues(route='simple',stage=1,base=NULL,prepared=NULL,result=NULL,raw=NULL,meta=NULL,steps=NULL,lesson=1,notice='')
clear_regression<-function(){greg$stage<-1;greg$base<-NULL;greg$prepared<-NULL;greg$result<-NULL;greg$raw<-NULL;greg$meta<-NULL;greg$steps<-NULL;greg$notice<-''}
observeEvent(input$go_regression,page('reg_choices'))
observeEvent(input$reg_home,page('home'))
observeEvent(input$reg_change,page('reg_choices'))
for(k in names(reg_routes))local({key<-k;observeEvent(input[[paste0('reg_choose_',key)]],{
 clear_regression();greg$route<-key;greg$lesson<-switch(key,simple=1,adjusted=4,interaction=5)
 if(isTRUE(rv$meta$reg_example)&&!length(rv$steps))reg_example_load()else page('reg_analysis')
})})
reg_example_load<-function(){t<-reg_example(greg$route);tmp<-tempfile(fileext='.csv');readr::write_csv(t$raw,tmp);rv$draft<-t$raw;rv$draftmeta<-t$meta;rv$draftpath<-tmp;use_draft();unlink(tmp);greg$base<-t$spec;page('reg_analysis')}
observeEvent(input$reg_example,{
 if(!is.null(rv$raw))showModal(modalDialog(title='Use the regression worked example?',p('This replaces your session data and clears preparation and results. Download your script first if you need it.'),footer=tagList(modalButton('Keep my data'),actionButton('reg_confirm_example','Use example',class='btn-primary'))))else reg_example_load()
})
observeEvent(input$reg_confirm_example,{removeModal();reg_example_load()})
reg_jump<-function(i){greg$lesson<-i;updateTabsetPanel(session,'reg_mode',selected='Tutorial')}
observeEvent(input$reg_tutorial,reg_jump(1))
observeEvent(input$reg_to_analysis,updateTabsetPanel(session,'reg_mode',selected='Analyse'))
for(i in 1:7)local({n<-i;observeEvent(input[[paste0('reg_why_',n)]],reg_jump(n))})
observeEvent(input$reg_lesson_prev,greg$lesson<-max(1,greg$lesson-1))
observeEvent(input$reg_lesson_next,greg$lesson<-min(7,greg$lesson+1))
reg_link<-function(n,label='(Tutorial)')actionButton(paste0('reg_why_',n),label,class='btn-link')
output$reg_analysis_ui<-renderUI({
 if(is.null(rv$raw))return(div(class='panel-card',h3('Begin with a dataset or worked example'),p('The tutorial has its own fictional data. Analyse your own independent observations, or load our example to try the controls.'),div(class='nav-actions',actionButton('go_import','Import / prepare data'),actionButton('reg_example','Use the worked example',class='btn-primary'),actionButton('reg_tutorial','Open tutorial'))))
 stage<-greg$stage
 heading<-p(class='progress-label',paste('Step',stage,'of 3 ·',c('Choose the model','Inspect the data','Read and explore the model')[stage]))
 if(stage==1){d<-data();nums<-names(d)[vapply(d,is.numeric,logical(1))];old<-isolate(greg$base$outcome);sel<-if(length(old)&&old%in%nums)old else if('wellbeing_score'%in%nums)'wellbeing_score'else nums[1]
 return(tagList(heading,div(class='two-col',div(class='panel-card',h3('1. What are you trying to explain?'),
  selectInput('reg_outcome','Numerical outcome variable',nums,selected=sel),uiOutput('reg_predictors_ui'),uiOutput('reg_types_ui'),uiOutput('reg_refs_ui'),
  if(greg$route!='simple')tags$details(open=if(greg$route=='interaction')NA else NULL,tags$summary('Allow interactions'),p('Select specific pairs when your question is whether an association changes across another variable. Both main predictors are retained.'),uiOutput('reg_interactions_ui'),reg_link(5,'How do interactions work? (Tutorial)')),
  selectInput('reg_conf','Confidence level',c('95%'=.95,'90%'=.90,'99%'=.99),selected=.95),
  checkboxInput('reg_design_ok','Each row is one independent observation, and the outcome is a numerical measurement rather than a Yes/No code.',FALSE),
  actionButton('reg_inspect','Inspect these observations',class='btn-primary')),
  div(class='panel-card',h3('Choose variables for a reason'),p(reg_prompts[[greg$route]]),p('A predictor is also called an independent or explanatory variable. Predictors may be related to one another; independence of observations concerns the study rows.'),p('Use 1–6 predictors. Exclude participant IDs and variables calculated directly from the outcome. Mark category codes as categorical, and choose a reference level.'),reg_link(1,'What does regression answer? (Tutorial)'),reg_link(4,'Adjustment and confounding (Tutorial)'),hr(),actionButton('reg_example','Try the worked example instead'),actionButton('go_import','Review / prepare data')))))
 }
 if(stage==2){r<-greg$prepared
 return(tagList(heading,div(class='panel-card',h3('2. Look before fitting'),p(paste(r$n,'complete observations will be used;',r$excluded,'of',r$total,'prepared rows are excluded because a selected value is missing.')),p('The same complete rows are used for every term and for the optional unadjusted comparison. Missingness can still affect interpretation.'),
  tags$pre(paste(deparse(reg_formula(r$spec)),collapse=' ')),scroll_table('reg_data_summary'),selectInput('reg_inspect_focal','View the outcome against',r$spec$predictors),scroll_plot('reg_observed_plot',410,700),
  p('Look for implausible values, sparse categories and relationships that may not be straight. For multiple regression these are unadjusted plots; the fitted model will account for all selected predictors together.'),
  if(length(r$spec$categorical))tagList(h4('Check category counts and combinations'),scroll_table('reg_category_counts'),p('Sparse or absent combinations can prevent an interaction from being estimated. A successful fit still needs enough information for useful precision.')),
  help_box('We assess residuals after fitting. Numerical predictors do not have to be normally distributed. The usual tests and intervals assume an appropriate mean model, independent errors and constant error variance; a normal-error approximation is especially relevant in small samples.'),reg_link(6,'How do we check assumptions? (Tutorial)'),
  div(class='nav-actions',actionButton('reg_fit','Fit this model',class='btn-primary'),actionButton('reg_back','Change variables')))))
 }
 r<-greg$result;cats<-unique(unlist(r$spec$interactions));cats<-intersect(cats,r$spec$categorical)
 tagList(heading,div(class='panel-card',h3('3. Read the fitted model'),p(paste('Outcome:',r$spec$outcome,'· Complete observations:',r$n,'· Excluded:',r$excluded)),
  h4('Coefficient estimates'),p('Read these as parts of the equation. Lower and Upper are confidence limits. SE is the coefficient standard error. The p-value tests one coefficient against zero.'),scroll_table('reg_coefficients'),
  selectInput('reg_coefficient','Explain one coefficient',r$coefficients$Term,selected=r$coefficients$Term[2]),uiOutput('reg_coefficient_text'),reg_link(2,'How do coefficients become an equation? (Tutorial)'),
  tags$details(tags$summary('Overall fit and whole-term tests'),scroll_table('reg_fit_table'),p('Overall p tests all non-intercept coefficients together. R-squared describes this sample; it does not measure prediction accuracy in new people.'),scroll_table('reg_term_tests'),p('These partial F tests compare the full model with a model omitting the named whole term, using the same rows. Main terms involved in interactions are retained for model hierarchy, so only eligible terms appear. A multi-level interaction is tested jointly.')),
  tags$details(tags$summary('Read the original R summary'),verbatimTextOutput('reg_native'))),
  div(class='panel-card',h3('4. Explore the full model'),p('Every prediction and effect below uses the same fitted coefficients. Changing these display settings does not refit the model.'),
   selectInput('reg_focal','Horizontal-axis predictor',r$spec$predictors,selected=r$spec$predictors[1]),uiOutput('reg_by_ui'),
   tags$details(tags$summary('Values to hold fixed for predictions and effects'),p('Only predictors not varied in a given plot or contrast are held at these values. Defaults are numerical medians and reference categories. Changing them changes the profile, not the model.'),uiOutput('reg_profile_ui')),
   uiOutput('reg_profile_text'),scroll_plot('reg_prediction_plot',440,max(700,110*max(vapply(r$data[r$spec$categorical],nlevels,integer(1)),1))),
   p(paste0('Lines or dots are predicted means; bands or bars are ',100*r$spec$conf,'% pointwise confidence intervals for those means, not intervals for individual people. Curves extend over the observed overall range; some group-specific combinations can still lack data.')),
   if(length(cats))tagList(h4('Equations and effects within an interacting category'),selectInput('reg_strata','Categorical interactor: view each level of',cats),p('The equations below substitute each selected category into the FULL model. They are not separate within-group regressions. All coefficients, covariance and residual degrees of freedom come from the full fit.'))else h4('The fitted equation and conditional effects'),
   uiOutput('reg_equations'),tags$details(tags$summary('Coefficients for these equations, with uncertainty'),scroll_table('reg_conditional_table')),
   uiOutput('reg_effect_choice'),scroll_table('reg_effects_table'),p('For a numerical predictor, the effect is a one-unit slope. For a categorical predictor, it is a difference from its reference category. Other interactors are evaluated at the profile values above. Intervals and p-values here are unadjusted; a significant effect in only one group does not establish interaction.'),reg_link(5,'Interpret interactions step by step (Tutorial)'),
   if(length(r$spec$predictors)>1&&!length(r$spec$interactions))tags$details(tags$summary('Compare the association before and after adjustment'),scroll_table('reg_comparison'),p('Both models use the same complete observations. A coefficient change can suggest confounding, but subject knowledge and study design determine whether adjustment is appropriate.'),reg_link(4,'Understand adjustment (Tutorial)')),
   tags$details(tags$summary('Predict at the chosen profile'),scroll_table('reg_profile_prediction'),p('The confidence interval concerns a population mean at this profile. The wider prediction interval concerns one new independent person under the same model. These are not validated forecasts.'))),
  div(class='panel-card',h3('5. Check what the model leaves unexplained'),p('Residuals are observed minus fitted values. Look at these plots before trusting the usual intervals and tests.'),
   h4('Residuals versus fitted values'),p('Look for a cloud around zero with roughly similar vertical spread. Curves or a widening funnel need attention.'),scroll_plot('reg_residual_plot',360,700),
   h4('Normal Q–Q plot of residuals'),p('Dots roughly following the line support a normal-error approximation. Pronounced curves or distant points deserve inspection, particularly with a small sample.'),scroll_plot('reg_qq_plot',360,700),
   tags$details(tags$summary('Inspect influential observations'),p('A large Cook’s distance means an observation may influence the coefficients. Check the record and context; no rows are removed automatically.'),scroll_plot('reg_cooks_plot',340,700),scroll_table('reg_influence_table')),
   radioButtons('reg_diagnostic_judgement','How do the diagnostic patterns look?',c('Choose after inspecting'='','Reasonably compatible with the model'='yes','Concerning or unsure'='no'),selected=''),uiOutput('reg_diagnostic_response'),reg_link(6,'Help me read these plots (Tutorial)')),
  div(class='panel-card',h3('Keep your analysis'),code_box('reg_code','reg_copy','reg_download','reg_zip'),div(class='nav-actions',downloadButton('reg_table_download','Download coefficients'),downloadButton('reg_effect_download','Download conditional effects'),downloadButton('reg_plot_download','Download figure'),actionButton('reg_back','Change variables'),actionButton('reg_home','Home'))))
})
output$reg_predictors_ui<-renderUI({req(input$reg_outcome);choices<-setdiff(names(data()),input$reg_outcome);old<-greg$base$predictors;chosen<-intersect(old,choices);if(!length(chosen))chosen<-if('sleep_hours'%in%choices)'sleep_hours'else choices[1];if(greg$route=='simple')chosen<-head(chosen,1);selectizeInput('reg_predictors','Predictor variables',choices,selected=chosen,multiple=greg$route!='simple',options=list(maxItems=if(greg$route=='simple')1 else 6))})
output$reg_types_ui<-renderUI({req(input$reg_predictors);xs<-input$reg_predictors;old<-isolate(input$reg_categorical);auto<-xs[!vapply(data()[xs],is.numeric,logical(1))];sel<-unique(c(auto,intersect(old,xs),intersect(greg$base$categorical,xs)));checkboxGroupInput('reg_categorical','Treat these predictors as categorical (including number codes)',xs,selected=sel)})
output$reg_refs_ui<-renderUI({xs<-intersect(input$reg_categorical,input$reg_predictors);tagList(lapply(xs,function(v){lev<-h_levels(data()[[v]]);idx<-match(v,names(data()));old<-greg$base$references[[v]];selectInput(paste0('reg_ref_',idx),paste('Reference category for',v),lev,selected=if(length(old)&&old%in%lev)old else lev[1])}))})
reg_pairs<-reactive({xs<-input$reg_predictors;if(length(xs)<2)return(list());combn(xs,2,simplify=FALSE)})
output$reg_interactions_ui<-renderUI({pairs<-reg_pairs();if(!length(pairs))return(p('Choose at least two predictors first.'));labels<-vapply(pairs,paste,character(1),collapse=' × ');chosen<-which(vapply(pairs,function(p)any(vapply(greg$base$interactions,function(q)setequal(p,q),logical(1))),logical(1)));checkboxGroupInput('reg_interactions','Which associations may vary?',setNames(as.character(seq_along(pairs)),labels),selected=as.character(chosen))})
observeEvent(input$reg_inspect,{tryCatch({
 if(!isTRUE(input$reg_design_ok))stop('Confirm what the rows and outcome represent first.')
 cats<-intersect(input$reg_categorical,input$reg_predictors);refs<-setNames(lapply(cats,function(v)input[[paste0('reg_ref_',match(v,names(data())))]]),cats)
 indices<-if(greg$route=='simple')integer()else suppressWarnings(as.integer(input$reg_interactions));indices<-indices[indices%in%seq_along(reg_pairs())]
 s<-list(outcome=input$reg_outcome,predictors=input$reg_predictors,categorical=cats,references=refs,interactions=reg_pairs()[indices],conf=as.numeric(input$reg_conf))
 greg$prepared<-reg_prepare(data(),s);greg$base<-s;greg$raw<-rv$raw;greg$steps<-rv$steps;greg$meta<-rv$meta;greg$stage<-2
},error=notify_error)})
output$reg_data_summary<-renderTable({r<-greg$prepared;req(r);do.call(rbind,lapply(c(r$spec$outcome,r$spec$predictors),function(v){x<-r$data[[v]];data.frame(Variable=v,Type=if(is.factor(x))'Categorical'else'Numerical',Description=if(is.factor(x))paste(nlevels(x),'categories; reference:',levels(x)[1])else paste('Mean',round(mean(x),3),'· SD',round(sd(x),3),'· range',paste(round(range(x),3),collapse=' to ')))}))},striped=TRUE)
output$reg_category_counts<-renderTable({r<-greg$prepared;req(length(r$spec$categorical));{rows<-lapply(r$spec$categorical,function(v){a<-table(r$data[[v]]);data.frame(Variable=v,Category=names(a),Count=as.integer(a))});for(pair in r$spec$interactions)if(all(pair%in%r$spec$categorical)){a<-as.data.frame(table(r$data[[pair[1]]],r$data[[pair[2]]]));rows[[length(rows)+1]]<-data.frame(Variable=paste(pair,collapse=' × '),Category=paste(a[[1]],a[[2]],sep=' / '),Count=a$Freq)};do.call(rbind,rows)}},striped=TRUE)
output$reg_observed_plot<-renderPlot({req(greg$prepared,input$reg_inspect_focal);reg_observed(greg$prepared,input$reg_inspect_focal)},res=96)
observeEvent(input$reg_fit,{tryCatch({greg$result<-reg_fit(data(),greg$base);greg$stage<-3},error=notify_error)})
observeEvent(input$reg_back,{greg$result<-NULL;greg$stage<-1})
output$reg_coefficients<-renderTable(reg_display(greg$result$coefficients),digits=4,striped=TRUE)
output$reg_coefficient_text<-renderUI({req(greg$result,input$reg_coefficient);tagList(lapply(reg_interpret(greg$result,input$reg_coefficient),p))})
output$reg_fit_table<-renderTable(reg_display(greg$result$fit_table),digits=4,striped=TRUE)
output$reg_term_tests<-renderTable(reg_display(greg$result$term_tests),digits=5,striped=TRUE)
output$reg_native<-renderPrint(summary(greg$result$fit))
output$reg_by_ui<-renderUI({r<-greg$result;req(r,input$reg_focal);choices<-setdiff(r$spec$predictors,input$reg_focal);partners<-unique(unlist(Filter(function(p)input$reg_focal%in%p,r$spec$interactions)));sel<-intersect(choices,partners);selectInput('reg_by','Separate lines or points by',c('No grouping'='',choices),selected=if(length(sel))sel[1]else'')})
output$reg_profile_ui<-renderUI({r<-greg$result;req(r);pr<-reg_profile(r);tagList(lapply(seq_along(r$spec$predictors),function(i){v<-r$spec$predictors[i];if(v%in%r$spec$categorical)selectInput(paste0('reg_profile_',i),v,levels(r$data[[v]]),selected=as.character(pr[[v]]))else numericInput(paste0('reg_profile_',i),v,value=pr[[v]])}))})
reg_current_profile<-reactive({r<-greg$result;req(r);pr<-reg_profile(r);for(i in seq_along(r$spec$predictors)){value<-input[[paste0('reg_profile_',i)]];if(!is.null(value)&&length(value))pr<-reg_set(r,pr,r$spec$predictors[i],value)};pr})
reg_strata<-reactive({r<-greg$result;req(r);cats<-intersect(unique(unlist(r$spec$interactions)),r$spec$categorical);if(length(cats)){if(!is.null(input$reg_strata)&&input$reg_strata%in%cats)input$reg_strata else cats[1]}else''})
output$reg_effect_choice<-renderUI({r<-greg$result;req(r);by<-reg_strata();choices<-if(nzchar(by))setdiff(unique(unlist(Filter(function(p)by%in%p,r$spec$interactions))),by)else r$spec$predictors;selectInput('reg_effect','Effect to interpret',choices,selected=choices[1])})
reg_view<-reactive({r<-greg$result;req(r);f<-if(!is.null(input$reg_focal)&&input$reg_focal%in%r$spec$predictors)input$reg_focal else r$spec$predictors[1];b<-if(!is.null(input$reg_by)&&input$reg_by%in%setdiff(r$spec$predictors,f))input$reg_by else'';st<-reg_strata();choices<-if(nzchar(st))setdiff(unique(unlist(Filter(function(p)st%in%p,r$spec$interactions))),st)else r$spec$predictors;e<-if(!is.null(input$reg_effect)&&input$reg_effect%in%choices)input$reg_effect else choices[1];list(focal=f,by=b,strata=st,effect=e,profile=reg_current_profile())})
output$reg_profile_text<-renderUI({r<-greg$result;v<-reg_view();fixed<-setdiff(r$spec$predictors,c(v$focal,v$by));desc<-vapply(fixed,function(n)paste(n,'=',as.character(v$profile[[n]])),character(1));nums<-setdiff(r$spec$predictors,r$spec$categorical);outside<-nums[vapply(nums,function(n)v$profile[[n]]<min(r$data[[n]])||v$profile[[n]]>max(r$data[[n]]),logical(1))];tagList(p(if(length(desc))paste('For the figure, held fixed:',paste(desc,collapse='; '))else'The figure varies all selected predictors.'),if(nzchar(v$by)&&!v$by%in%r$spec$categorical)p(paste('Separate lines use the 25th, 50th and 75th observed percentiles of',v$by)),if(length(outside))help_box(paste('Profile outside observed ranges:',paste(outside,collapse=', '),'. This is extrapolation; interpret with care.'),'warning-note'))})
output$reg_prediction_plot<-renderPlot({v<-reg_view();reg_plot(greg$result,v$focal,v$by,v$profile)},res=96)
output$reg_equations<-renderUI({r<-greg$result;req(r);eq<-reg_conditional(r,reg_strata());tagList(lapply(names(eq$equations),function(g)tagList(h4(g),tags$pre(style='white-space:pre-wrap;overflow-wrap:anywhere',eq$equations[[g]]))),p('I(category = level) is 1 for that level and 0 otherwise. Intercepts use numerical predictors at zero and remaining categories at their references. Rounded equations are for reading; calculations use full precision.'))})
output$reg_conditional_table<-renderTable(reg_display(reg_conditional(greg$result,reg_strata())$table),digits=4,striped=TRUE)
output$reg_effects_table<-renderTable({v<-reg_view();reg_display(reg_effects(greg$result,v$effect,v$strata,v$profile)$table)},digits=4,striped=TRUE)
output$reg_comparison<-renderTable({v<-reg_view();reg_display(reg_compare(greg$result,v$focal))},digits=4,striped=TRUE)
output$reg_profile_prediction<-renderTable({r<-greg$result;pr<-reg_current_profile();a<-predict(r$fit,pr,interval='confidence',level=r$spec$conf);b<-predict(r$fit,pr,interval='prediction',level=r$spec$conf);data.frame(Quantity=c('Mean at profile','One new person'),Estimate=c(a[1],b[1]),Lower=c(a[2],b[2]),Upper=c(a[3],b[3]))},digits=3,striped=TRUE)
output$reg_residual_plot<-renderPlot({req(greg$result);reg_diagnostic(greg$result,'residual')},res=96)
output$reg_qq_plot<-renderPlot({req(greg$result);reg_diagnostic(greg$result,'qq')},res=96)
output$reg_cooks_plot<-renderPlot({req(greg$result);reg_diagnostic(greg$result,'cooks')},res=96)
output$reg_influence_table<-renderTable({req(greg$result);d<-greg$result$diagnostics;head(d[order(d$Cooks_distance,decreasing=TRUE),],5)},digits=4,striped=TRUE)
output$reg_diagnostic_response<-renderUI({if(identical(input$reg_diagnostic_judgement,'no'))help_box('Pause before interpreting the usual p-values and intervals. Check units and records, reconsider the mean relationship, and discuss a suitable model with your teacher. More complicated dependence or unequal variance needs methods beyond this introductory module. The app has not removed any observations.','warning-note')else if(identical(input$reg_diagnostic_judgement,'yes'))p('These plots support your judgement but do not prove the assumptions. Describe what you checked when reporting the model.')})
reg_code<-reactive({req(greg$result);reg_student_script(greg$meta,greg$steps,greg$base,greg$raw,reg_view())})
output$reg_code<-renderText(reg_code())
observeEvent(input$reg_copy,session$sendCustomMessage('copyCode',reg_code()))
output$reg_download<-downloadHandler(filename=function()'linear_regression.R',content=function(file)writeLines(reg_code(),file,useBytes=TRUE))
output$reg_download_exact<-downloadHandler(filename=function()'linear_regression_exact.R',content=function(file)writeLines(reg_exact_script(greg$meta,greg$steps,greg$base,reg_view()),file,useBytes=TRUE))
output$reg_zip<-downloadHandler(filename=function()'linear_regression.zip',content=function(file)make_zip(file,reg_code(),rv$source,greg$meta$name,reg_exact_script(greg$meta,greg$steps,greg$base,reg_view())))
output$reg_table_download<-downloadHandler(filename=function()'regression_coefficients.csv',content=function(file)readr::write_csv(greg$result$coefficients,file))
output$reg_effect_download<-downloadHandler(filename=function()'regression_conditional_effects.csv',content=function(file){v<-reg_view();a<-reg_effects(greg$result,v$effect,v$strata,v$profile)$table;a$Confidence<-greg$base$conf;a$Residual_df<-df.residual(greg$result$fit);a$Profile<-paste(vapply(names(v$profile),function(n)paste(n,as.character(v$profile[[n]]),sep='='),character(1)),collapse='; ');readr::write_csv(a,file)})
output$reg_plot_download<-downloadHandler(filename=function()'regression_predictions.png',content=function(file){v<-reg_view();ggsave(file,reg_plot(greg$result,v$focal,v$by,v$profile),width=10,height=6,dpi=300,bg='white')})
# Tutorial state is independent from uploaded data and fitted analysis.
reg_tutorial_state<-reactive(reg_lesson_state(greg$lesson))
output$reg_tutorial_ui<-renderUI({i<-greg$lesson;t<-reg_tutorial_state();tagList(
 div(class='nav-actions',actionButton('reg_to_analysis','Return to my analysis')),
 div(class='stepbar',actionButton('reg_lesson_prev','← Previous'),span(class='progress-label',paste('Tutorial step',i,'of 7')),actionButton('reg_lesson_next','Next →')),
 div(class='panel-card lesson-copy',h3(reg_lesson_titles[i]),lapply(reg_lesson_text(i,t),p)),
 if(i==4)div(class='panel-card',h3('Picture the shared information'),p('The square represents all variation in wellbeing scores. Start with sleep alone, then reveal age.'),checkboxInput('reg_reveal_age','Reveal the second predictor: age',FALSE),uiOutput('reg_confounding_diagram'),uiOutput('reg_confounding_diagram_text'),p(class='small-muted','Schematic illustration, not measured areas or percentages from these data. Shared explained variation and a regression coefficient are different quantities; the picture does not calculate the change in slope.')),
 div(class='panel-card',h3('Worked example: 120 fictional adults'),p('Wellbeing is a numerical score; sleep is hours per night; age is years. Each programme has 40 different people. These are simulated teaching data, separate from your analysis.'),
  if(i==3)reg_dummy_ui(t),
  if(i==5)reg_interaction_ui(t),
  if(i==4)tagList(h4('Compare the actual fitted associations'),p('The unadjusted line mixes ages. The adjusted line holds age at the sample median; its slope compares sleep values at the same age. Shading shows 95% confidence intervals for predicted means.')),
  if(i==1)scroll_plot('reg_tut_observed',410,700)else if(i==6)tagList(h4('Residuals versus fitted'),scroll_plot('reg_tut_residual',360,700),h4('Normal Q–Q plot'),scroll_plot('reg_tut_qq',360,700),h4('Influence'),scroll_plot('reg_tut_cooks',340,700))else scroll_plot('reg_tut_plot',420,700),
  if(i==2)tagList(uiOutput('reg_tut_equations'),scroll_table('reg_tut_coefs')),
  if(i%in%c(3,5))tagList(h4('Match the equation to the R coefficient table'),scroll_table('reg_tut_coefs')),
  if(i==4)tagList(scroll_table('reg_tut_comparison'),h4('What do these numbers mean?'),lapply(reg_confounding_text(t),p)),
  if(i==5)tagList(h4('Sleep slopes from the full model'),scroll_table('reg_tut_effects'),h4('Joint interaction test'),scroll_table('reg_tut_interactions'),h4('Interpret this example in words'),lapply(reg_interaction_text(t),p)),
  if(i==7)tagList(h4('The profile being predicted'),scroll_table('reg_tut_profile'),h4('Mean versus one new person'),scroll_table('reg_tut_prediction'),h4('Model fit'),scroll_table('reg_tut_fit'))),
 code_box('reg_tut_code','reg_tut_copy','reg_tut_download','reg_tut_zip'),
 tags$details(tags$summary('R function references'),tags$a(href='https://stat.ethz.ch/R-manual/R-patched/library/stats/html/lm.html',target='_blank',rel='noopener','lm()'),p(''),tags$a(href='https://stat.ethz.ch/R-manual/R-patched/library/stats/html/predict.lm.html',target='_blank',rel='noopener','predict() and intervals')))
})
output$reg_tut_observed<-renderPlot({t<-reg_tutorial_state();reg_observed(t$result,t$view$focal)},res=96)
output$reg_tut_plot<-renderPlot({t<-reg_tutorial_state();if(greg$lesson==4)reg_confounding_plot(t)else reg_plot(t$result,t$view$focal,t$view$by,t$view$profile)},res=96)
output$reg_tut_equations<-renderUI({t<-reg_tutorial_state();eq<-reg_conditional(t$result,t$view$strata);tagList(lapply(names(eq$equations),function(n)tagList(h4(n),tags$pre(style='white-space:pre-wrap;overflow-wrap:anywhere',eq$equations[[n]]))))})
output$reg_tut_coefs<-renderTable(reg_display(reg_tutorial_state()$result$coefficients),digits=4,striped=TRUE)
output$reg_tut_comparison<-renderTable({req(greg$lesson==4);reg_display(reg_compare(reg_tutorial_state()$result,'sleep_hours'))},digits=4,striped=TRUE)
output$reg_tut_effects<-renderTable({req(greg$lesson==5);t<-reg_tutorial_state();reg_display(reg_effects(t$result,'sleep_hours','programme',t$view$profile)$table)},digits=4,striped=TRUE)
output$reg_tut_interactions<-renderTable(reg_display(reg_tutorial_state()$result$interaction_tests),digits=4,striped=TRUE)
output$reg_tut_residual<-renderPlot(reg_diagnostic(reg_tutorial_state()$result,'residual'),res=96)
output$reg_tut_qq<-renderPlot(reg_diagnostic(reg_tutorial_state()$result,'qq'),res=96)
output$reg_tut_cooks<-renderPlot(reg_diagnostic(reg_tutorial_state()$result,'cooks'),res=96)
output$reg_tut_profile<-renderTable(reg_tutorial_state()$view$profile,digits=2)
output$reg_tut_prediction<-renderTable({t<-reg_tutorial_state();a<-predict(t$result$fit,t$view$profile,interval='confidence');b<-predict(t$result$fit,t$view$profile,interval='prediction');data.frame(Quantity=c('Population mean','One new person'),Estimate=c(a[1],b[1]),Lower=c(a[2],b[2]),Upper=c(a[3],b[3]))},digits=3)
output$reg_tut_fit<-renderTable(reg_display(reg_tutorial_state()$result$fit_table),digits=4)
reg_tut_code<-reactive({t<-reg_tutorial_state();reg_lesson_student_script(greg$lesson,t)})
output$reg_tut_code<-renderText(reg_tut_code())
observeEvent(input$reg_tut_copy,session$sendCustomMessage('copyCode',reg_tut_code()))
output$reg_tut_download<-downloadHandler(filename=function()'regression_example.R',content=function(file)writeLines(reg_tut_code(),file,useBytes=TRUE))
output$reg_tut_download_exact<-downloadHandler(filename=function()'regression_example_exact.R',content=function(file){t<-reg_tutorial_state();writeLines(reg_lesson_exact_script(greg$lesson,t),file,useBytes=TRUE)})
output$reg_tut_zip<-downloadHandler(filename=function()'regression_example.zip',content=function(file){t<-reg_tutorial_state();tmp<-tempfile(fileext='.csv');on.exit(unlink(tmp));readr::write_csv(t$raw,tmp);make_zip(file,reg_tut_code(),tmp,t$meta$name,reg_lesson_exact_script(greg$lesson,t))})

output$reg_confounding_diagram<-renderUI({req(greg$lesson==4);reg_confounding_svg(isTRUE(input$reg_reveal_age))})
output$reg_confounding_diagram_text<-renderUI({req(greg$lesson==4);if(isTRUE(input$reg_reveal_age))tagList(p('Now the circles overlap. The shared region is variation that sleep and age both help account for. It cannot be counted as uniquely explained by sleep.'),p('The sleep-only region shows what sleep adds beyond the information already in age. This is smaller than the whole original sleep circle in this illustration. The combined model can account for more total variation even while the part uniquely accounted for by sleep is smaller.'))else p('With only sleep in view, we see the whole sleep circle. Some of that information may also be carried by age, but we cannot see the shared part until age is included.')})
