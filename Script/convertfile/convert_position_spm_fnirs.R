convert_position_spm_fnirs <- function(originname, othersname) {

  # 输入数据并校验文件名
  print("Please input origin file")
  originname <- file.choose()
  print("Please input others file")
  othersname <- file.choose()
  if (stringr::str_detect(originname, "origin") == FALSE) {
    warning("Please input origin file!")
    stop()
  }
  if (stringr::str_detect(othersname, "others") == FALSE) {
    warning("Please input others file!")
    stop()
  }

  OriginXYZ <- read.csv(originname, header = TRUE, na.strings = NA)[2:4]
  OthersXYZ <- read.csv(othersname, header = FALSE, na.strings = NA)[2:4]

  # 开始分析数据

  # 建立标准坐标系
  DMNIHAve <- matrix(a <- c(1.59969646229532, 79.9945373845711, -41.4170507904725,
                            5.95433425048611, -1.67872695095577, -120.546358497584, -21.48864971015,
                            14.2339564612865, 76.5889196320321, -28.1702904655389, -52.3077974178331,
                            5.53535214499577, -76.6175260909038, -27.3888225082851, -53.5281845359011,
                            6.07618805786147, -26.1176470588235, 83.5294117647059, -0.117647058823533,
                            5.9, 32.1176470588235, 81.2941176470588, -0.235294117647052, 5.3, -0.117647058823533,
                            53.2941176470588, 70.5882352941177, 10.3, -42.625, 58, 39.625, 11.5, 46.9411764705882,
                            56.7058823529412, 40.2352941176471, 9.5, -68.5, 37.875, -5.875, 6.6, 71.4117647058824,
                            35.6470588235294, -7.76470588235294, 6.2, 0.588235294117651, -12.7058823529412,
                            101.411764705882, 7.5, -62.7058823529412, -12.7058823529412, 69.7647058823529,
                            9.7, 64.2352941176471, -15.1764705882353, 69.8823529411765, 8.8, -84.5882352941177,
                            -20.7058823529412, -10.8235294117647, 8.5, 86, -25.5294117647059, -9.41176470588236,
                            7.4, -0.235294117647052, -76.5882352941177, 88.4705882352941, 9, -46.8235294117647,
                            -88.2352941176471, 58.8235294117647, 10.5, 44.8235294117647, -87.8823529411765,
                            59.6470588235294, 9.5, -72.2352941176471, -72.3529411764706, 0.588235294117651,
                            9.3, 70.8235294117647, -75.6470588235294, 4.11764705882353, 8.7, -31.8823529411765,
                            -112.588235294118, 17.2941176470588, 12.6, 28.1176470588235, -112.941176470588,
                            19.2941176470588, 13.5), ncol = 4, byrow = TRUE)

  # 读取参考坐标
  select_line <- which(is.na(OriginXYZ[, 2]) == FALSE)
  loadedData_Origin = OriginXYZ[select_line, ]
  loadedData_DMNIHAve = DMNIHAve[(select_line + 1), ]  # XYZ coordinates and SD
  loadedData_Others = OthersXYZ

  DMNI <- readRDS("dataset/DMNI.Rdata") #文件来源于nfri_mni_estimation_spm，详情请访问https://www.nitrc.org/projects/spm_fnirs/
  RefBList_Cur <- list()
  RefBList_WW <- list()
  ListOri <- cbind(loadedData_Origin, rep(1,4))

  for(i in 1:17){
    DM <- DMNI[[i]][(select_line+1),]
    RefDist <- cbind(DM, rep(1,4))
    WW <- solve(as.matrix(ListOri),as.matrix(RefDist))
    RefBListCur = as.matrix(ListOri) %*% as.matrix(WW)
    RefBList_Cur[[i]] <- RefBListCur
    RefBList_WW[[i]] <- WW
  }

  #建立转换后数据的矩阵
  result <- matrix(1, nrow = nrow(loadedData_Others), ncol = 4)
  result[,1:3] <- as.matrix(loadedData_Others)

  convert_other_list <- list()

  for(i in 1:17){
    OtherRef <- as.matrix(result) %*% as.matrix(RefBList_WW[[i]])
    convert_other_list[[i]] <- OtherRef
  }

  PointN <- nrow(result)
  PListOverPoint <- list()
  PList <- matrix(1, nrow = 17, ncol = 3)

  for(i in 1:PointN){
    for(j in 1:17){
      PList[j, ] <- convert_other_list[[j]][i, 1:3]
    }
    PListOverPoint[[i]] <- PList
  }

  OtherRefCList <- list()

  for(i in 1:PointN){
    for(j in 1:PointN){
      xallM <- CrtSrfMNISm[[i]][1]$xallM
      yallM <- CrtSrfMNISm[[i]][1]$yallM
      zallM <- CrtSrfMNISm[[i]][1]$zallM

      ProjectionListC[[j]] <- ProjectionBS_f(xallM, yallM, zallM)
    }

  OtherRefCList[[i]] <- ProjectionListC
  }

  CPListOverPoint <- list()

  for(i in 1:PointN){
    CPList = matrix(1, ncol = 3, nrow = 17)

    for(j in 1:17){
    CPList[j, ] = OtherRefCList[[i]][j, ]
    }

    CPListOverPoint[[i]] = CPList
  }


  CPListOverPoint{c2} = CPList;
  ShowProgress(c2, PointN, 'Restoring data across reference brains');
  end

  OtherC <- matrix(1, nrow = PointN, ncol = 3)

  for(i in 1:PointN){
    AA <- mean(CPListOverPoint[[i]][1,])
  }
}
