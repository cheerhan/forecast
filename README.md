#文件夹结构

##读接口的文件 src

###实时监控中接口处理
* readPVStation.R
获取光伏电站列表
* readWindStation.R
获取光风电站列表
* readDevCap.R
获取逆变器列表，包括206与201，主要包括装机容量。

###历史趋势中接口处理
* readDevice.R
获取设备列表，表头？？？
* readPoint.R
获取设备测点列表
* readHistory.R
* readData.R
获取历史数据。

##应用结果 App

###已实现应用
* pvStationGen.R 
用于某电站某天数据电量数据确认。----待封装

* checkAggregate.R
对比1分钟聚合数据与10分钟聚合数据结果。----待封装

###待实现应用
* 查看数测点数据接入质量！！！
现在用的是deviceGEN.R


* lossGen.R
计算某电站逆变器损失电量。
  * 场景一：部分设备故障。---待封装
  * 场景二：全部设备故障/限电。---待开发
  
* checkGen.R
计算某一个设备某一个测点一天数据质量。

