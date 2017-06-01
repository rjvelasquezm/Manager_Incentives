library(tidyverse)
library(reshape2)
library(lubridate)
library(xts)
library(scales)
library(ReporteRs)
library(RollingWindow)


#Read in data
returns <- read_csv("//sigisdev/Risk Share/Share/RV/Projects/overdiversification/returns.csv")

#Melt data and select only managers benchmarked to S&P500 and then repivot
returns_melt = melt(returns,id.vars = c("Firm Name","Product Name","Vehicle Name","Benchmark","VT","RM"))


returns_pivot = returns_melt %>% dcast(variable ~ `Firm Name` + `Product Name` + `Vehicle Name`, value.var = "value") %>% as_tibble()

#Change no return months to NA for easier maniupulation
returns_pivot[returns_pivot =="---"] = NA

#Turn date column into date object
returns_pivot$variable = mdy( returns_pivot$variable)


#Only pick managers with full history between 2006/6/30 and 2016/06/30
#returns_subset = returns_pivot %>% filter(variable > ymd("2006/06/30"))

returns_subset = returns_pivot
returns_subset = returns_subset[colSums(is.na(returns_subset))<nrow(returns_subset)]

returns_subset[,-1] = lapply(returns_subset[,-1],function(x) as.numeric(x)/100)

returns_subset_no_na = returns_subset
returns_subset_no_na[is.na(returns_subset)] = 0
rolling_alpha = RollingSum(returns_subset_no_na[,-1],window = 12,na_method = "ignore" ) %>% as.data.frame()
#rolling_alpha$date = returns_subset$variable


returns_subset_no_dates = returns_subset[,-1] %>% as.data.frame()
rolling_alpha_df = rolling_alpha %>% as.data.frame()
dates = returns_subset$variable %>% as.data.frame()

x = 1
returns = returns_subset_no_dates[,x]
alpha = rolling_alpha_df[,x]




# Create Function ---------------------------------------------------------



get10Date = function(returns,alpha,dates){
        
        
        returns = returns %>% as.data.frame()
        alpha = alpha %>% as.data.frame()
        dates = dates %>% as.data.frame()
        
        #mgr = colnames(alpha)
        
        colnames(alpha) = "returns"
        colnames(returns) = "returns"
        colnames(dates) = "dates"
        
        
        data_alpha = cbind(alpha,dates) %>% as.data.frame() %>%
                arrange(dates) %>%
                mutate(dates = as_date(dates))
        
        dates_alpha = data_alpha %>%
                dplyr::filter(returns>0.1)
        
        if (nrow(dates_alpha) ==0) { 
                
                te_dif = data.frame(0,as_date(0),0,0)
                colnames(te_dif) = c("returns","dates","te_before","te_after")
                
                
                } else{
        
        data_returns = cbind(returns,dates) %>% as.data.frame() %>%
                arrange(dates) %>%
                mutate(dates = as_date(dates)) 
        
        te_before = sapply(dates_alpha$dates,function(x) {
                rets_sd =  data_returns %>% dplyr::filter(dates<=x)
                stdev = sd(rets_sd$returns,na.rm = T)
                return(stdev)
        }) %>% as.data.frame()
        
        te_after = sapply(dates_alpha$dates,function(x) {
                rets_sd =  data_returns %>% dplyr::filter(dates>x)
                stdev = sd(rets_sd$returns,na.rm = T)
                return(stdev)
        }) %>% as.data.frame()
        
        te_dif = data.frame(dates_alpha,te_before, te_after)
        colnames(te_dif) = c("returns","dates","te_before","te_after")
        #colnames(te_dif) = c("before","after")
        #te_dif$dif = te_dif[,1] - te_dif[,2]
        
        
                }
        return(te_dif)
}

get10Date_months = function(returns,alpha,dates,m,buffer){
        
        
        #If buffer is true
        if(buffer) {b = 12} else {b = 0}
        
        
        returns = returns %>% as.data.frame()
        alpha = alpha %>% as.data.frame()
        dates = dates %>% as.data.frame()
        
        #mgr = colnames(alpha)
        
        colnames(alpha) = "returns"
        colnames(returns) = "returns"
        colnames(dates) = "dates"
        
        
        data_alpha = cbind(alpha,dates) %>% as.data.frame() %>%
                arrange(dates) %>%
                mutate(dates = as_date(dates))
        
        dates_alpha = data_alpha %>%
                dplyr::filter(returns>0.1 )#& month(dates)==12)
        
        
        
        
        
        if (nrow(dates_alpha) ==0) { 
                
                te_dif = data.frame(0,as_date(0),0,0)
                colnames(te_dif) = c("returns","dates","te_before","te_after")
                
                
        } else{
                
                data_returns = cbind(returns,dates) %>% as.data.frame() %>%
                        arrange(dates) %>%
                        mutate(dates = as_date(dates)) 
                
                te_before = sapply(dates_alpha$dates,function(x) {
                        rets_sd =  data_returns %>% dplyr::filter(dates<= (x %m-% months(b)) & (x %m-% months(m+b)) <= (dates ))
                        stdev = sd(rets_sd$returns,na.rm = T)
                        return(stdev)
                }) %>% as.data.frame()
                
                te_after = sapply(dates_alpha$dates,function(x) {
                        rets_sd =  data_returns %>% dplyr::filter( dates > x & (dates <= ( x  %m+% months(m))))
                        
                        stdev = sd(rets_sd$returns,na.rm = T)
                        
                        return(stdev)
                }) %>% as.data.frame()
                
                te_dif = data.frame(dates_alpha,te_before, te_after)
                colnames(te_dif) = c("returns","dates","te_before","te_after")
                #colnames(te_dif) = c("before","after")
                #te_dif$dif = te_dif[,1] - te_dif[,2]
                
                
        }
        return(te_dif)
}

get10Date_months_no_overlap = function(returns,alpha,dates,m,buffer){
        
        
        #If buffer is true
        if(buffer) {b = 12} else {b = 0}
        
        
        returns = returns %>% as.data.frame()
        alpha = alpha %>% as.data.frame()
        dates = dates %>% as.data.frame()
        
        
        colnames(alpha) = "returns"
        colnames(returns) = "returns"
        colnames(dates) = "dates"
        
        
        data_alpha = cbind(alpha,dates) %>% as.data.frame() %>%
                arrange(dates) %>%
                mutate(dates = as_date(dates))
        
        dates_alpha = data_alpha %>%
                dplyr::filter(returns>0.1 )#& month(dates)==12)
        
        
        
        
        
        
        
        if (nrow(dates_alpha) ==0) { 
                
                te_dif = data.frame(0,as_date(0),0,0)
                colnames(te_dif) = c("returns","dates","te_before","te_after")
                
                
        } else{
                
                #Remove Overlapping Periods
                for (i in 1:nrow(dates_alpha)) {
                        
                        d = dates_alpha$dates[i]
                        
                        if(is.na(d)){} else{
                                
                                
                                dates_alpha =  dates_alpha %>%
                                        filter(!(dates >d & dates < d %m+% months(12)))
                        }
                        
                }
                
                #Create Return df
                data_returns = cbind(returns,dates) %>% as.data.frame() %>%
                        arrange(dates) %>%
                        mutate(dates = as_date(dates)) 
                
                
                #Calculate TE before and after
                
                te_before = sapply(dates_alpha$dates,function(x) {
                        rets_sd =  data_returns %>% dplyr::filter(dates<= (x %m-% months(b)) & (x %m-% months(m+b)) <= (dates ))
                        stdev = sd(rets_sd$returns,na.rm = T)
                        return(stdev)
                }) %>% as.data.frame()
                
                te_after = sapply(dates_alpha$dates,function(x) {
                        rets_sd =  data_returns %>% dplyr::filter( dates > x & (dates <= ( x  %m+% months(m))))
                        
                        stdev = sd(rets_sd$returns,na.rm = T)
                        
                        return(stdev)
                }) %>% as.data.frame()
                
                #Calculate Average TE for the rolling period #TODO
                
                
                
                
                #Create Data Frame
                te_dif = data.frame(dates_alpha,te_before, te_after)
                colnames(te_dif) = c("returns","dates","te_before","te_after")
                
                
                
        }
        return(te_dif)
}



# Run Function for TE before and after ------------------------------------------------------------




# Run Regular -------------------------------------------------------------


#Get number of managers
managers = 1:ncol(rolling_alpha)


#Apply function to each manager
results = lapply(managers, function(x){
        
        get10Date(
                returns_subset_no_dates[,x],
                rolling_alpha[,x],
                dates
        )})
        

#Name values
names(results) = colnames(returns_subset_no_dates)

#Bind into a dataframe
results_df = bind_rows(results,.id = "manager")




# Run Months --------------------------------------------------------------


results_months = lapply(managers, function(x){
        
        print(x)
        
        get10Date_months(
                returns_subset_no_dates[,x],
                rolling_alpha[,x],
                dates,
                24,
                FALSE
        )})


#Name values
names(results_months) = colnames(returns_subset_no_dates)

#Bind into a dataframe
results_months_df = bind_rows(results_months,.id = "manager")
results_months_df$dif = results_months_df$te_before - results_months_df$te_after

mean(results_months_df$te_after,na.rm = T)
mean(results_months_df$te_before,na.rm = T)

t.test(results_months_df$te_before,results_months_df$te_after,alternative = "greater",paired = T)

ggplot(results_months_df,aes(dif)) + geom_histogram()


#Perform on a manager weighted basis instead of ocurrence basis
results_months_df_grouped = results_months_df %>% group_by(manager) %>% summarise(te_before = mean(te_before),te_after = mean(te_after))

t.test(results_months_df_grouped$te_before,results_months_df_grouped$te_after,alternative = "greater",paired = T)

#test                
x = 1

test = get10Date_months(
        returns_subset_no_dates[,x],
        rolling_alpha[,x],
        dates,
        24,
        TRUE
)
        
        
        










# Run No Overlap ----------------------------------------------------------

results_overlap = lapply(managers, function(x){
        
        print(x)
        
        get10Date_months_no_overlap(
                returns_subset_no_dates[,x],
                rolling_alpha[,x],
                dates,
                m = 24,
                buffer = TRUE
        )})


#Name values
names(results_overlap) = colnames(returns_subset_no_dates)

#Bind into a dataframe
results_overlap_df = bind_rows(results_overlap,.id = "manager")
results_overlap_df$dif = results_overlap_df$te_before - results_overlap_df$te_after

mean(results_overlap_df$te_after,na.rm = T)
mean(results_overlap_df$te_before,na.rm = T)

t.test(results_overlap_df$te_before,results_overlap_df$te_after,alternative = "greater",paired = T)

ggplot(results_overlap_df,aes(dif)) + geom_histogram()




# Calculate TE Trend Through Time -----------------------------------------

mean_abs_te = rowMeans(abs(returns_subset_no_dates),na.rm = T) %>% as.data.frame()
colnames(mean_abs_te) = "mean_abs_te"

mean_abs_te$date = dates$dates

ggplot(mean_abs_te,aes(x = date, y = mean_abs_te)) + geom_line()

mean_abs_te = rowMeans(abs(returns_subset_no_dates),na.rm = T) %>% as.data.frame()
colnames(mean_abs_te) = "mean_abs_te"

mean_abs_te$date = dates$dates

ggplot(mean_abs_te,aes(x = date, y = mean_abs_te)) + geom_line()






