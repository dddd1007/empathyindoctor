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
# Ver. 0.1 2016年12月19日

convert_spm_fnirs <- function(filename = NA) {

  options(warn = -1)

  cat("分析开始……", "\n")

  #选择数据文件
  if(is.na(filename) == TRUE){
  filename <- file.choose()
  }

  #读取数据
  datafile <- readr::read_delim(filename, delim = "\t", escape_double = FALSE, trim_ws = TRUE, skip = 34, col_names = TRUE)
  datafile <- datafile[-1,]
  probe_pair <- readr::read_lines(filename, skip = 32, n_max = 2)

  #提取血氧数据
  wavelength1 <- dplyr::select(datafile, dplyr::starts_with("Abs780"))
  wavelength2 <- dplyr::select(datafile, dplyr::starts_with("Abs805"))

  #提取onset点
  onset_num <- unique(datafile$Task)
  onset_num <- onset_num[onset_num != "00"]
  onset_table_scans <- data.frame(matrix(0, ncol = 2, nrow = length(onset_num)))
  onset_table_secs <- data.frame(matrix(0, ncol = 2, nrow = length(onset_num)))
  names(onset_table_secs) <- c("onsets", "secs")
  names(onset_table_scans) <- c("onsets", "scans")
  for(i in onset_num){
    row_onset_num <- which(onset_num == i)
    onset_table_secs[row_onset_num,1] <- i
    onset_table_secs[row_onset_num,2] <- stringr::str_c(as.character(datafile$`Time(sec)`[which(datafile$Task == i)]), collapse = " ")
  }
  for(i in onset_num){
    row_onset_num <- which(onset_num == i)
    onset_table_scans[row_onset_num,1] <- i
    onset_table_scans[row_onset_num,2] <- stringr::str_c(as.character(which(datafile$Task == i)), collapse = " ")
  }

  #计算采样率
  fs <- length(datafile$`Time(sec)`[datafile$`Time(sec)` < 1])

  #提取通道布局
  probe_position <- probe_pair[1]
  probe_name <- probe_pair[2]

  probe_name <- stringr::str_split(probe_name, pattern = "\t")[[1]]
  probe_name <- stringr::str_extract(probe_name, pattern = "ch-..")
  probe_name <- unique(probe_name[!is.na(probe_name)])
  probe_name <- stringr::str_replace(stringr::str_sub(probe_name, start = 4), pattern = " ", replacement = "")

  probe_position <- stringr::str_split(probe_position, pattern = "[()]")[[1]]
  probe_position <- probe_position[probe_position != ""]
  probe_position <- stringr::str_split(probe_position, pattern = ",")
  probe_position_x <- sapply(probe_position, "[[", 1)
  probe_position_y <- sapply(probe_position, "[[", 2)

  ch_config <- data.frame(Ch = probe_name, Source = probe_position_x, Detector = probe_position_y)

  #写入文件
  filename <- stringr::str_sub(filename, start = 1, end = -5)
  ch_config_name <- stringr::str_c(filename, "ch_config.csv", sep = "_")
  wavelength1_name <- stringr::str_c(filename, "wavelength1.csv", sep = "_")
  wavelength2_name <- stringr::str_c(filename, "wavelength2.csv", sep = "_")
  onset_name_scans <- stringr::str_c(filename, "onset_scans.csv", sep = "_")
  onset_name_secs <- stringr::str_c(filename, "onset_secs.csv", sep = "_")
  readr::write_csv(ch_config, path = ch_config_name, col_names = TRUE)
  readr::write_csv(onset_table_scans, path = onset_name_scans, col_names = TRUE)
  readr::write_csv(onset_table_secs, path = onset_name_secs, col_names = TRUE)
  readr::write_csv(wavelength1, path = wavelength1_name, col_names = FALSE)
  readr::write_csv(wavelength2, path = wavelength2_name, col_names = FALSE)

  #反馈采样率与频率
  cat("wavelength1为780, wavelength2为805", "\n")
  cat("采样率为", fs, "\n", sep = " ")
  cat("分析结束，怎么样，很快吧？\n")
}
