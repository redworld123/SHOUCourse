library("rgdal")
library("ggplot2")

# 数据预处理
data_pre = function(inputPath, month, zlim_value) {
  
  # 读取数据路径
  hdf4 = list.files(path = inputPath,
                    pattern = "*.hdf$",
                    all.files = FALSE,
                    full.names = TRUE)
  
  # 读取hdf4文件
  hdf = hdf4[month]
  hdf_open = readGDAL(hdf)
  
  # 去除 -9999.0
  tmp_1 = which(hdf_open$band1 == -9999)
  hdf_open$band1[tmp_1] = NaN
  
  # 选择合理渲染区间
  tmp_2 = which(hdf_open$band1[!is.nan(hdf_open$band1)] >= zlim_value)
  percentage = length(tmp_2) / (length(hdf_open$band1) - length(tmp_1))
  res_str = paste("有效值的高值缺失率：", percentage, "| 月份：", month)
  
  # 构建矩阵
  hdf_open = matrix(hdf_open$band1, 2160, 1080)
  hdf_open = hdf_open[,1080:1]
  
  # 矩阵仿射变换
  lat = seq(-90, 90, length.out = ncol(hdf_open))
  lon = seq(-180, 180, length.out = nrow(hdf_open))
  
  # 构建数据容器
  result = list("res_str" = res_str,
                "hdf_open" = hdf_open,
                "lat" = lat,
                "lon" = lon)
  
  return(result)
}

# 预览hdf环境因子数据
draw_pictures = function(inputPath, month, zlim_value, draw_area) {
  
  # 数据预处理
  res = data_pre(inputPath, month, zlim_value)
  res_str = res$res_str
  hdf_open = res$hdf_open
  lat = res$lat
  lon = res$lon
  
  # 绘制预览图
  par(mar = c(12,3,1,6))
  ocean = readOGR("./country/country.shp")
  plot(ocean,
       col = "dark gray",
       axes = T,
       xlim = c(draw_area[3], draw_area[4]),
       ylim = c(draw_area[1], draw_area[2]))
  image.plot(lon,
             lat,
             hdf_open,
             add = TRUE,
             zlim = c(0, zlim_value),
             horizontal = TRUE
  )
  
  return(res_str)
}

# 主控函数
hdfDraw = function (inputPath, month, zlim_value, draw_area) {
  
  res_str = draw_pictures(inputPath, month, zlim_value, draw_area)
  print(res_str)
}
