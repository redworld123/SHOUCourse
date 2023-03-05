# 鱼捞网格数据预处理
dataPre = function (inputPath, outYear) {
  
  # 目录检查
  d = paste("./output/", outYear, sep = "")
  if (!dir.exists(d)) {
    dir.create(d)
  }
  
  # 筛选数据
  my_data = read_excel(inputPath, sheet = 1, na = "NA")
  my_data = my_data %>% filter(my_data$yy == outYear)
  my_data = my_data %>% filter(my_data$days != 0)
  my_data = my_data %>% filter(my_data$yft_c_una + my_data$yft_c_log + my_data$yft_c_dfad + my_data$yft_c_afad + my_data$yft_c_oth != -1)
  
  # 纬度标准化
  for (i in c(1:length(my_data$lat_short))) {
    
    if(grepl('N',my_data$lat_short[i])) {
      
      my_data$lat_short[i] = as.integer(substr(my_data$lat_short[i],1,2))
    } else if(grepl('S',my_data$lat_short[i])) {
      
      tmp = substr(my_data$lat_short[i],1,2)
      tmp = paste('-',tmp,sep = '')
      my_data$lat_short[i] = as.integer(tmp)
    } else {
      
      error = paste('error lat_short,', i, sep = '')
      print(error)
    }
  }
  
  # 经度标准化
  for (i in c(1:length(my_data$lon_short))) {
    
    if(grepl('E',my_data$lon_short[i])) {
      
      my_data$lon_short[i] = as.integer(substr(my_data$lon_short[i],1,3))
    } else if(grepl('W',my_data$lon_short[i])) {
      
      tmp = substr(my_data$lon_short[i],1,3)
      tmp = paste('-',tmp,sep = '')
      my_data$lon_short[i] = 360 + as.integer(tmp)
    } else {
      
      error = paste('error lon_short,',i,sep = '')
      print(error)
    }
  }
  
  # 按月份输出数据
  csv_content = data.frame()
  csv_content_x = vector()
  csv_content_y = vector()
  my_data_month = unique(my_data$mm)
  for (month in my_data_month) {
    
    # 输出文件准备
    if (month < 10) {
      
      file_name_month = paste(outYear, "_0", sep = "")
      file_name_month = paste(file_name_month, month, sep = "")
      file_name = paste(file_name_month, ".csv", sep = "")
      file_path = paste("./output/", outYear, sep = "")
      file_path = paste(file_path, "/", sep = "")
      file_path = paste(file_path, file_name, sep = "")
    } else {
      
      file_name_month = paste(outYear, "_", sep = "")
      file_name_month = paste(file_name_month, month, sep = "")
      file_name = paste(file_name_month, ".csv", sep = "")
      file_path = paste("./output/", outYear, sep = "")
      file_path = paste(file_path, "/", sep = "")
      file_path = paste(file_path, file_name, sep = "")
    }
    
    # 筛选月份数据
    csv_content = my_data %>% filter(my_data$mm == month)
    
    # 通过月份数据构建需要的数据列
    year = csv_content$yy
    month = csv_content$mm
    lat = csv_content$lat_short
    lon = csv_content$lon_short
    days = csv_content$days
    yeild = csv_content$yft_c_una + csv_content$yft_c_log + csv_content$yft_c_dfad + csv_content$yft_c_afad + csv_content$yft_c_oth
    yeild = yeild * 1000
    cpue = yeild / days
    
    # 构建年份数据结构，并赋值给输出数据容器
    csv_content_month = data.frame("Years" = year,
                                   "Months" = month,
                                   "Lat" = lat,
                                   "Lon" = lon,
                                   "Days" = days,
                                   "Yeild" = yeild,
                                   "CPUE" = cpue
    )
    
    # 计算渔场重心
    Ci = as.integer(cpue)
    Xi = as.integer(lon)
    Yi = as.integer(lat)
    
    SumCi = sum(Ci)
    CiXi = Ci * Xi
    CiYi = Ci * Yi
    
    SumXi = sum(CiXi)
    SumYi = sum(CiYi)
    
    pointX = SumXi / SumCi
    pointY = SumYi / SumCi
    
    csv_content_x = append(csv_content_x,pointX)
    csv_content_y = append(csv_content_y,pointY)
    
    # 输出年份数据
    write_csv(csv_content_month,file_path)
  }
  
  # 输出渔场重心 Center Gravity of Fishery Ground
  csv_content_CGFG = data.frame("Month" = c(1:12), "Lon" = csv_content_x, "Lat" = csv_content_y)
  CGFG_path = paste("./output/", outYear, sep = "")
  CGFG_path = paste(CGFG_path, "_CGFG.csv", sep = "")
  write_csv(csv_content_CGFG, CGFG_path)
}
