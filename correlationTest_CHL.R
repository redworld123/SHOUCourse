# CHL-CPUE相关性测试
correlationTest_CHL = function (inputPath, month = 1, step = 0.02, show = 10) {
  
  # 点采样后数据输入
  file_path = paste(inputPath, "/", sep = "")
  file_path = paste(file_path, month, sep = "")
  file_path = paste(file_path, ".csv", sep = "")
  my_data = read.table(file = file_path, header = TRUE, sep = ",")
  
  # 筛选需要数据列
  yield = my_data$Yeild
  chl = my_data$ef
  cpue = my_data$CPUE
  
  # K-S基础参数计算
  chl_max = max(chl)
  chl_min = min(chl)
  chl_num = length(chl)
  
  cpue_mean = mean(cpue)
  cpue_cpue_mean = cpue / cpue_mean
  
  start_chl = floor(chl_min)
  end_chl = ceiling(chl_max)
  
  # K-S步进值计算
  chli = seq(from = start_chl, to = end_chl, by = step)
  
  # ft函数计算
  ft = vector()
  for (i in c(1:length(chli))) {
    
    tmp_yield = 0
    for (j in c(1:length(chl))) {
      
      if(chl[j] < chli[i]) {
        
        tmp_yield = tmp_yield + 1
      }
    }
    
    ft = append(ft,tmp_yield)
  }
  ft = ft / chl_num
  
  # gt函数计算
  gt = vector()
  for (i in c(1:length(chli))) {
    
    tmp_yield = 0
    for (j in c(1:length(chl))) {
      
      if(chl[j] < chli[i]) {
        
        tmp_yield = tmp_yield + cpue_cpue_mean[j]
      }
    }
    
    gt = append(gt,tmp_yield)
  }
  gt = gt / chl_num
  
  # dt函数计算
  dt = abs(round(ft,digits = 3) - round(gt,digits = 3))
  
  # 数据输入
  result = data.frame("x" = chli,"y1" = ft, "y2" = gt, "y3" = dt)
  title_str = paste(month,"月chl K-S",sep = "")
  ggplot()+
    geom_line(data = result, aes(x = x, y = y1), size = 2)+
    geom_line(data = result, aes(x = x, y = y2), size = 2, colour = "red")+
    geom_line(data = result, aes(x = x, y = y3*show), size = 2, colour = "yellow")+
    scale_y_continuous(sec.axis = sec_axis(~. *0.1, name = "dt"))+
    labs(y = "ft and gt",title = title_str)
}
