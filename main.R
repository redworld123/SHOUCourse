#========== 加载包组 ===========#
library("sf")
library("rgdal")
library("readr")
library("raster")
library("readxl")
library("fields")
library("ggplot2")
library("tidyverse")
library("gdalUtils")
#========== 加载包组 ===========#



#========== 加载模块 ===========#
# 清空内存
rm(list = ls())
# 鱼捞网格数据预处理模块
source("./dataPre.R")
# 根据csv坐标数据生成采样点shp文件
source("./csv2shp.R")
# 环境因子数据hdf预览
source("./hdfDraw.R")
# 环境数据转化函数
source("./hdf2tif.R")
# 环境因子点采样
source("./pointSampling.R")
# CPUE
source("./correlationTest.R")
# SI
source("./SI.R")
#========== 加载模块 ===========#



#========== 代码执行 ===========#
#
dataPre("./input/WCPFC_S_PUBLIC_BY_1x1_MM.xlsx", 2020)
#
csv2shp("./output/2020")
#
hdfDraw("./input/sst.v.2019", 1, 40, c(-66.5, 30, 60, 110))
#
hdf2tif("./input/chl.v.2019", 2019, "chl")
hdf2tif("./input/sst.v.2019", 2019, "sst")
#
pointSampling("./output/2019", "./output/2019/chl", "./output/2019chl")
pointSampling("./output/2019", "./output/2019/sst", "./output/2019sst")
#
correlationTest_SST("./output/2019sst", 1, 0.3)
correlationTest_CHL("./output/2019chl", 12, 0.02)
#
SI_sst("./output/2019sst", 2019, 12, 0.20, TRUE)
SI_chl("./output/2019chl", 2019, 12, 0.02, TRUE)
#
result_sst = SI_sst("./output/2019sst", 2019, 12, 0.20, FALSE)
result_chl = SI_chl("./output/2019chl", 2019, 12, 0.02, FALSE)
