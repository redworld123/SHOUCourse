# SI_chl
SI_chl = function(inputPath, outYear, month, step, flag) {
  
  # 数据输入
  file_path = paste(inputPath, "/", sep = "")
  file_path = paste(file_path, month, sep = "")
  file_path = paste(file_path, ".csv", sep = "")
  my_data = read.csv(file = file_path, header = TRUE, sep = ",")
  
  # 筛选数据
  yield = my_data$Yeild
  chl = my_data$ef
  
  # 计算极值
  max_chl = max(chl)
  min_chl = min(chl)
  
  # 经验值，观察数据和多次前期拟合得来
  start_chl = min_chl
  end_chl = max_chl #2_0.2 4_0.18 5_0.2 6_0.15 7_0.15
  
  # 计算间距
  chli_tmp = seq(from = start_chl, to = end_chl, by = step)
  chli = vector()
  for (i in c(1:(length(chli_tmp)-1))) {
    chli[i] = (chli_tmp[i] + chli_tmp[i+1]) / 2
  }
  
  # 分阶筛选
  c_chl_yield = vector()
  for (i in c(1:length(chli))) {
    
    tmp_yield = 0
    for (j in c(1:length(chl))) {
      
      if(chl[j] >= (chli[i] - 0.01) && chl[j] < (chli[i] + 0.01)) {
        
        tmp_yield = tmp_yield + yield[j]
      }
    }
    
    c_chl_yield = append(c_chl_yield,tmp_yield)
  }
  
  # 最终计算
  c_chl_yield_max = max(c_chl_yield)
  si_chl = c_chl_yield / c_chl_yield_max
  
  # 拟合曲线
  x = vector()
  y = vector()
  for (i in c(1:length(si_chl))) {
    
    if(si_chl[i] != 0) {
      x = append(x,chli[i])
      y = append(y,si_chl[i])
    }
  }
  x = log(x)
  y = log(y)
  show_info = nls(y ~ a*x^2+b*x+c, start = list(a = 0,b = 0,c = 0))
  res = environment(environment(show_info[["m"]][["getPars"]])[["convCrit"]])[["internalPars"]]
  
  # 统计结果
  x = exp(x)
  y = exp(y)
  result_1 = data.frame("x" = x, "y" = y)
  
  # 拟合结果 
  a = sqrt(-(1/res[1]))
  b = (res[2]*a^2)/2
  a = round(a, digits = 1)
  b = round(b, digits = 1)
  c = c(a,b)
  gs = exp(-(1/(a)^2)*(log(x)-b)^2)
  result_2 = data.frame("x" = x,"y" = gs)
  
  # 输出数据
  CHL_I = data.frame("res1" = y,"res2" = gs)
  outputPath = paste("./output/", outYear, sep = "")
  outputPath = paste(outputPath, "_", sep = "")
  outputPath = paste(outputPath, month, sep = "")
  outputPath = paste(outputPath, "_SI_chl.csv", sep = "")
  write.csv(CHL_I, outputPath)
  
  if (flag) {
    
    # 条形图
    title_str = paste(month, "月不同海表面叶绿素浓度(chl)及对应的SI指数",sep = "")
    ggplot(data = result_1, mapping = aes(x = x, y = y))+
      geom_bar(stat = 'identity')+
      geom_line(data = result_2, aes(x = x, y = y), size = 2)+
      geom_point(size = 4, shape = 20)+
      labs(title = title_str)
  } else {
    
    return(c)
  }
}
