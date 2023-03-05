# 环境因子点采样
pointSampling = function (shpPath, tifPath, outPath) {
  
  # 读取数据路径
  check_data_shp = list.files(path = shpPath,
                              pattern = "*.shp$",
                              all.files = FALSE,
                              full.names = TRUE)
  check_data_tif = list.files(path = tifPath,
                              pattern = "*.tif$",
                              all.files = FALSE,
                              full.names = TRUE)
  
  # 环境因子点采样
  len = length(check_data_shp)
  for (i in c(1:len)) {
    
    point = st_read(check_data_shp[i])
    ef = raster(check_data_tif[i])
    
    point$ef = raster::extract(ef, as(point,"Spatial"))
    point = point %>% filter(!is.na(point$ef))
    
    op = paste(outPath, "/", sep = "")
    op = paste(op, i, sep = "")
    op = paste(op, ".csv", sep = "")
    
    if (dir.exists(outPath)) {
      
      write_csv(point, op)
    } else {
      
      dir.create(outPath)
      write_csv(point, op)
    }
  }
}
