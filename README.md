
##读接口的文件

###实时监控中接口处理
* readStation.R
获取电站列表
* readDevCap.R
获取逆变器列表，包括206与201

###历史趋势中接口处理
* readDevice.R
* readPoint.R
* readHistory.R


##分析数据文件
* stationGen.R 
用于某电站某天数据电量数据确认。----待封装
* deviceGen.R
用于某电站某段时间所有设备的发电量。----待封装
* lossGen.R
计算某一天电站逆变器损失电量。