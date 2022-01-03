library("ggplot2")
output_path = "/Users/peterabraham/Desktop/econ11/hw9_Peter_Abraham"
input_data = "/Users/peterabraham/Downloads/monthly_temp-1.csv"
temp_df = read.csv(input_data)
temp_df[,"Date"] = as.Date(temp_df[,"Date"], format= "%m/%d/%Y")

simple_exp_smooth = function(temp_df, alpha)
{

#set intial conditions
n = length(temp_df[,"temperature"])
temp_df[1,"simple_smooth"] = temp_df[1,"temperature"]

#loop through remainder of data
for (i in 1:n)
  #read in actua
{
  temp_actual = temp_df[i,"temperature"]
  
  #create smooth term
  temp_df[i+1,"simple_smooth"] = alpha *temp_df[i,"temperature"] + (1-alpha) * temp_df[i,"simple_smooth"]
  #writing data to temp df
  temp_df[i+1,"simple_forecast"] =   temp_df[i+1,"simple_smooth"]
  temp_df[i+1,"simple_residual"] = temp_df[i+1,"temperature"] - temp_df[i+1,"simple_smooth"]
  temp_df[i+1,"simp_res_squared"] = temp_df[i+1,"simple_residual"]^2
}
#Return
simple_exp_smooth = temp_df
}

#Call function
temp_df = simple_exp_smooth(temp_df,0.25)

#plots
p = ggplot(temp_df)
p = p + geom_line(aes(x=Date,y = temperature), color ="darkgrey", size=0.75)
p = p + geom_point(aes(x=Date,y = temperature), color ="darkmagenta", size=1, shape = 21)
p = p + geom_line(aes(x= Date,y = simple_forecast), color ="red", size = 1)
p = p + labs(title = "Simple Exponential Smoothing", x = "Date", y = "Advanced Retail Sales")
p = p + ggsave(paste0(output_path,"simple_Peter_Abraham.png"),width = 10, height = 5)


#Holt Exponential Smoothing
holt_exp_smooth = function(temp_df, alpha, beta)
{
  
  #set intial conditions
  n = length(na.omit(temp_df[,"temperature"]))
  temp_df[1,"holt_smooth"] = temp_df[1,"temperature"]
  temp_df[1,"holt_trend"] =(temp_df[n,"temperature"] - temp_df[1,"temperature"])/n
  #loop through remainder of data
  for (i in 1:n)
    #read in actual
  {
    temp_actual = temp_df[i,"temperature"]
    
    #create smooth term
    temp_df[i+1,"holt_smooth"] = alpha *temp_df[i,"temperature"] + (1-alpha) * (temp_df[i,"holt_smooth"] + temp_df[i,"holt_trend"]) 
    #writing data to temp df
    #create trend
    temp_df[i+1,"holt_trend"] = beta *(temp_df[i+1,"holt_smooth"]-temp_df[i,"holt_smooth"]) + (1-beta) *  (temp_df[i,"holt_trend"]) 
    #columns
    temp_df[i+1,"holt_forecast"] =   temp_df[i,"holt_smooth"] + temp_df[i,"holt_trend"]
    temp_df[i+1,"holt_residual"] = temp_df[i+1,"temperature"] - temp_df[i+1,"holt_forecast"]
    temp_df[i+1,"holt_res_squared"] = temp_df[i+1,"holt_residual"]^2
  }
  #Return
 holt_exp_smooth = temp_df
}

#Call function
temp_df = holt_exp_smooth(temp_df,0.25,0.8)



#Plots 
p = ggplot(temp_df)
p = p + geom_line(aes(x=Date,y = temperature), color ="darkgrey", size=0.75)
p = p + geom_point(aes(x=Date,y = temperature), color ="darkmagenta", size=1, shape = 21)
p = p + geom_line(aes(x= Date,y = holt_forecast), color ="red", size = 1)
p = p + labs(title = "Holt Exponential Smoothing", x = "Date", y = "Advanced Retail Sales")
p = p + ggsave(paste0(output_path,"holt_Peter_Abraham.png"),width = 10, height = 5)


