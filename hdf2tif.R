# 环境数据转化函数
hdf2tif = function (hdfPath, outYear, flag) {
  
  # 读取数据路径
  check_data = list.files(path = hdfPath,
                          pattern = "*.hdf$",
                          all.files = FALSE,
                          full.names = TRUE)
  
  # 目录检查
  d = paste("./output/", outYear, sep = "")
  d = paste(d, "/", sep = "")
  d = paste(d, flag, sep = "")
  if (!dir.exists(d)) {
    dir.create(d)
  }
  
  # 数据转化
  for (i in check_data) {
    
    j = strsplit(i, "/")[[1]][4]
    o = paste("./output/", outYear, sep = "")
    o = paste(o, "/", sep = "")
    o = paste(o, flag, sep = "")
    o = paste(o, "/", sep = "")
    o = paste(o, j, sep = "")
    o = paste(o, ".tif", sep = "")
    res = gdal_translate(i, dst_dataset = o, a_srs = "EPSG:4326", a_ullr = c(-180.0833, 90.08333, 179.9167, -89.91667), a_nodata = -9999.0)
  }
}
