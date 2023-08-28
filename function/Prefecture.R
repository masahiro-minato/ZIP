Prefecture_tif <- function(xx){
  # 住所データをベクトルで引数として渡すと戻り値として県名・地方名のtibbleを返す
  # 空のtibble
  pre <- 
    tibble()
  # 住所の列データを読込み、県名を抽出しtibbleでの行結合を繰り返す
  for (x in xx){
    # print(x)
    pre <- 
      pre %>% 
      bind_rows(Prefecture(x))
  }
  # print(pre)
  return(pre)
}

Prefecture <- function(x){
  # 文字列の住所から都道府県名を抜き出し地方名とあわせて戻り値として返す
  # 但し、都道府県名は住所の最初にある必要がある
  # ｘ：住所の文字列　例　x = "大阪府大阪市都島区毛馬町三丁目"
  Prefecture_name <- c("北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県","茨城県","栃木県","群馬県",
                       "埼玉県","千葉県","東京都","神奈川県","新潟県","富山県","石川県","福井県","山梨県","長野県",
                       "岐阜県","静岡県","愛知県","三重県","滋賀県","京都府","大阪府","兵庫県","奈良県","和歌山県",
                       "鳥取県","島根県","岡山県","広島県","山口県","徳島県","香川県","愛媛県","高知県","福岡県",
                       "佐賀県","長崎県","熊本県","大分県","宮崎県","鹿児島県","沖縄県")
  Local_name <- c("北海道地方",rep("東北地方",6),rep("関東地方",7),rep("中部地方",9),
                  rep("近畿地方",7),rep("中国地方",5),rep("四国地方",4),rep("九州・沖縄地方",8))
  Local_level <- c("北海道地方","東北地方","関東地方","中部地方","近畿地方","中国地方","四国地方","九州・沖縄地方")
  
  tib <- 
    tibble(
      Prefecture=as.factor(Prefecture_name),
                Local=as.factor(Local_name)
      )
  
  tib$Prefecture <- factor(tib$Prefecture, levels = Prefecture_name)
  tib$Local <- factor(tib$Local, levels = Local_level)
  
  list <- c("都","道","府","県")
  d_NA <- 
    tibble(
      Prefecture=as.factor(NA),
      Local=as.factor(NA)
    )
  c <- NA
  for (L in list){
    for (Pn in tib$Prefecture){
      if (nchar(substr(x,1,regexpr(L, x))) > 5 | nchar(substr(x,1,regexpr(L, x))) < 2){
        break
      }
      if (!is.na(base::match(Pn, table=substr(x,1,regexpr(L, x))))){
        c <- substr(x,1,regexpr(L, x))
        break
      }
    }
    if (!is.na(base::match(Pn, table=substr(x,1,regexpr(L, x))))){
      break
    } 
  }
  if (is.na(c)){
    d <- d_NA
  }else{
    d <- 
      tib %>% 
      filter(Prefecture == c)
  }
  # print(d)
  return(d)
}
