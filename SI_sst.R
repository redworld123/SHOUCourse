# SI_sst
SI_sst = function(inputPath, outYear, month, step, flag) {
  
  # 数据输入
  file_path = paste(inputPath, "/", sep = "")
  file_path = paste(file_path, month, sep = "")
  file_path = paste(file_path, ".csv", sep = "")
  my_data = read.csv(file = file_path, header = TRUE, sep = ",")
  
  # 筛选数据
  yield = my_data$Yeild
  sst = my_data$ef
  
  # 计算极值
  max_sst = max(sst)
  min_sst = min(sst)#5_29 6_29 10_29.5 11_30
  
  # 经验值、观察数据和多次前期拟合得来
  start_sst = min_sst
  end_sst = max_sst
  
  # 计算间距
  ssti_tmp = seq(from = start_sst, to = end_sst, by = step)
  ssti = vector()
  for (i in c(1:(length(ssti_tmp)-1))) {
    ssti[i] = (ssti_tmp[i] + ssti_tmp[i+1]) / 2
  }
  
  # 分阶筛选
  c_sst_yield = vector()
  for (i in c(1:length(ssti))) {
    
    tmp_yield = 0
    for (j in c(1:length(sst))) {
      
      if(sst[j] >= (ssti[i] - 0.1) && sst[j] < (ssti[i] + 0.1)) {
        
        tmp_yield = tmp_yield + yield[j]
      }
    }
    
    c_sst_yield = append(c_sst_yield,tmp_yield)
  }
  
  # 最终计算
  c_sst_yield_max = max(c_sst_yield)
  si_sst = c_sst_yield / c_sst_yield_max
  
  # 拟合曲线 
  x = vector()
  y = vector()
  for (i in c(1:length(si_sst))) {
    
    if(si_sst[i] != 0) {
      x = append(x,ssti[i])
      y = append(y,si_sst[i])
    }
  }
  x = x
  y = log(y)
  show_info = nls(y ~ a*x^2+b*x+c, start = list(a = 0,b = 0,c = 0))
  res = environment(environment(show_info[["m"]][["getPars"]])[["convCrit"]])[["internalPars"]]
  
  # 统计结果
  y = exp(y)
  result_1 = data.frame("x" = x, "y" = y)
  
  # 拟合结果
  a = sqrt(-(1/res[1]))
  b = (res[2]*a^2)/2
  a = round(a, digits = 1)
  b = round(b, digits = 1)
  c = c(a,b)
  gs = exp(-(1/(a)^2)*(x-b)^2)
  result_2 = data.frame("x" = x,"y" = gs)
  
  # 输出数据
  SST_I = data.frame("res1" = y, "res2" = gs)
  outputPath = paste("./output/", outYear, sep = "")
  outputPath = paste(outputPath, "_", sep = "")
  outputPath = paste(outputPath, month, sep = "")
  outputPath = paste(outputPath, "_SI_sst.csv", sep = "")
  write.csv(SST_I, outputPath)
  
  if (flag) {
    
    # 条形图
    title_str = paste(month, "月不同海表面温度(SST)及对应的SI指数", sep = "")
    ggplot(data = result_1, mapping = aes(x = x, y = y))+
      geom_bar(stat = 'identity')+
      geom_line(data = result_2, aes(x = x, y = y), size = 2)+
      geom_point(size = 4, shape = 20)+
      labs(title = title_str)
  } else {
    
    return(c)
  }
}
