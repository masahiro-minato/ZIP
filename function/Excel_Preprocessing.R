Excel_Preprocessing <- function(
  Metis_sheet1 = Metis_sheet1, 
  Metis_sheet1_2 = Metis_sheet1_2, 
  Metis_sheet1_3 = Metis_sheet1_3,
  Metis_sheet2 = Metis_sheet2
){
  # Metis_MF3名のデータフレームを作成する
  # Excelシートの結合と列選定、data型への変換を行う
  
  Metis_sheet123 <- 
    bind_rows(Metis_sheet1, Metis_sheet1_2, Metis_sheet1_3)
  
  Metis_sheet1a <- 
    Metis_sheet123 %>% 
    mutate(
      機種略機番 = paste(機種略号, 機番, sep="")
    )
  
  # 結合
  Metis_sheet1a2join <- left_join(Metis_sheet1a, Metis_sheet2, by="機種略機番")
  
  # colnames(Metis_sheet1a)
  
  # 列の選定
  Metis_MF3 <- Metis_sheet1a2join %>% 
    select(機種略号.x,機種略機番,機種ｺｰﾄﾞ,訪問区分,
               年月度,製造年月,納入日.x,稼動月,保守実施日,
               CE作業時間,ｺｰﾙ,EM,現象,現象SC名称,処置場所,...17,周辺機名) %>% 
    filter(EM == 1)
  
  # 日付へ変換
  Metis_MF3$年月度 <- 
    paste(str_sub(Metis_MF3$年月度, start=1, end=4),
          str_sub(Metis_MF3$年月度, start=5, end=-1),("01"),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  Metis_MF3$製造年月 <- 
    paste(str_sub(Metis_MF3$製造年月, start=1, end=4),
          str_sub(Metis_MF3$製造年月, start=5, end=-1),("01"),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  Metis_MF3$納入日.x <- 
    paste(str_sub(Metis_MF3$納入日.x, start=1, end=4),
          str_sub(Metis_MF3$納入日.x, start=5, end=6),
          str_sub(Metis_MF3$納入日.x, start=7, end=-1),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  Metis_MF3$保守実施日 <- 
    paste(str_sub(Metis_MF3$保守実施日, start=1, end=4),
          str_sub(Metis_MF3$保守実施日, start=5, end=6),
          str_sub(Metis_MF3$保守実施日, start=7, end=-1),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  # 列名
  # colnames(Metis_MF3)
  Metis_MF3 <- 
    rename(Metis_MF3, 
           Model_abbreviation = 機種略号.x,
           Machine_numbers = 機種略機番,
           Model_code = 機種ｺｰﾄﾞ,
           Visit_classification = 訪問区分,
           Year_and_month = 年月度,
           Manufacturing_date = 製造年月,
           Due_date = 納入日.x,
           Working_month = 稼動月,
           Maintenance_date = 保守実施日,
           CE_Working_hours = CE作業時間,
           Call = ｺｰﾙ,
           Phenomenon = 現象,
           SC_name = 現象SC名称,
           Treatment_location = 処置場所,
           Peripheral_machine = ...17,
           Peripheral_name = 周辺機名
    )
  return(Metis_MF3)
  
}
