id<-'sig_lap1'
run.time
MAP_HR
MAP_R

#Need to send this to correct directory
jpeg(paste0('','sig_lap1_',round(time.min*100), '_', round(time.max*100), '.jpeg'), 
     res = 300, units = 'in', width = 8, height = 8)
traceplot(fit.stan, pars='HR')+
ggtitle(paste("sig_lap1 HR Traceplot GP Imputation:", round(time.min, digits = 2), 'to', round(time.max, digits = 2)))
dev.off()

sink(paste0('/home/mbarsted/Documents/Dissertation/Result_Summaries/','sig_lap1_',round(time.min*100), '_', round(time.max*100),'.txt'))
cat(paste0('ID:', '\t\t\t', id))
cat(paste0('T1:', '\t\t\t', round(time.min, digits = 2)))
cat(paste0('T2:', '\t\t\t', round(time.max, digits = 2)))
cat(paste0('Total Time', '\t\t\t', round(time.max, digits = 2)-round(time.min, digits = 2)))
cat(paste0('Run Time:', '\t\t\t', run_time=round(run_time, digits = 2)))
cat(paste0('MAP HR:', '\t\t\t', round(mu_HR2*60, digits = 2)))
cat(paste0('MAP R:', '\t\t\t',round(mu_R2*60, digits = 2)))
cat(paste0('adapt_delta:', '\t\t\t', rv$delta))
cat(paste0('Iterations:', '\t\t\t', rv$GP.iter))
cat(paste0('Warmup:', '\t\t\t', rv$GP.wrm))
cat('\n\nSystem Information:')
cat('-------------------------------------------------------------------------------------')
cat(paste0('Processor:', '\t\t\t', get_cpu()$model_name))
cat(paste0('Number of Cores:', '\t\t\t', detectCores(logical=F)))
cat(paste0('Number of Threads:', '\t\t\t', detectCores(logical=T)))
cat(paste0('RAM:', '\t\t\t', get_ram()))
cat()
sink()
