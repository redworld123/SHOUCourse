# 采样点数据转化
csv2shp = function (inputPath) {
  
  # 读取数据路径
  check_data = list.files(path = inputPath,
                          pattern = "*.csv",
                          all.files = FALSE,
                          full.names = TRUE)
  
  # 数据转化
  k = 1
  for (i in check_data) {
    
    my_data = read.csv(file = i, header = TRUE, sep = ",")
    my_shp = st_as_sf(my_data, coords = c("Lon","Lat"), crs = 4326)
    
    j = strsplit(check_data[1], "_")[[1]]
    if (k < 10) {
      
      k_tmp = paste("0", k, sep = "")
      o = paste(j[1], k_tmp, sep = "_")
      o = paste(o, ".shp", sep = "")
    } else {
      
      o = paste(j[1], k, sep = "_")
      o = paste(o, ".shp", sep = "")
    }
    st_write(my_shp, o, driver = "ESRI Shapefile")
    
    k = k + 1
  }
}
