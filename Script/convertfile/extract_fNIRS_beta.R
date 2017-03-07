#
#                       _oo0oo_
#                      o8888888o
#                      88" . "88
#                      (| -_- |)
#                      0\  =  /0
#                    ___/`---'\___
#                  .' \\|     |# '.
#                 / \\|||  :  |||# \
#                / _||||| -:- |||||- \
#               |   | \\\  -  #/  |   |
#               | \_|  ''\---/''  |_/ |
#               \  .-\__  '-'  ___/-. /
#             ___'. .'  /--.--\  `. .'___
#          ."" '<  `.___\_<|>_/___.' >' "".
#         | | :  `- \`.;`\ _ /`;.`/ - ` : | |
#         \  \ `_.   \_ __\ /__ _/   .-` /  /
#     =====`-.____`.___ \_____/___.-`___.-'=====
#                       `=---='
#
#
#     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#               佛祖保佑         永无BUG
#
# 天津师范大学 心理与行为研究院 近红外组
# 夏骁凯 编写
# 有问题请联系 xia@xiaokai.me
# Ver. 0.1 2017年1月14日
# 本文件用于提取 spm_fnirs 分析后输出的 con_* 文件中的 beta 值

extract_fNIRS_beta <- function(filename = NA) {
  if(is.na(filename)){filename = file.choose()}
  if(stringr::str_detect(filename, pattern = "con_") == FALSE){
    print("Please choose the 'con_' file which spm_fnirs toolbox generated!")
  }

  beta_data <- R.matlab::readMat(filename)$S[1,,]$cbeta
  return(beta_data)
}
