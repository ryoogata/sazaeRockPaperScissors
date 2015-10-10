require(rvest)
require(stringr)

# Web 上から情報を取得
textdata <- html("http://www.asahi-net.or.jp/~tk7m-ari/sazae_ichiran.html")

# <B></B> 内の情報を抽出 
cast <- html_node(textdata, "b")

# \n で分割
vect <- str_split(string = html_text(cast), pattern = "\n")[[1]]

# \t を , に置換
vect <- str_replace_all(string = vect, pattern = "\t", replacement = ",")

# 1 列の , 区切りの data.frame を作成
theDF <- data.frame(vect, stringsAsFactors = FALSE)

# , で文字列を分割して、data.frame にまとめる
list <- str_split(string = theDF$vect, pattern = ",")
listMatrix <- data.frame(Reduce(rbind, list), stringsAsFactors = FALSE)

# 1 行目に不要なデータが入るため削除
x <- nrow(listMatrix) -1
listMatrix <- listMatrix[2:x,]

# 列目を設定
names(listMatrix) <- c("vol", "date", "hand")

# 第と回を削除
listMatrix$vol <- str_replace_all(string = listMatrix$vol, pattern = "第|回", replacement ="")

# 手以外の不要な情報を削除
listMatrix$hand <- str_replace(string = listMatrix$hand, pattern = "（観月ありさ）", replacement = "")
listMatrix$hand <- str_replace(string = listMatrix$hand, pattern = "（カツオ）", replacement = "")

# 休みの行を削除
listMatrix <- listMatrix[which(listMatrix$hand != "休み"),]

# 放送回を数値データに
listMatrix$vol <- as.numeric(listMatrix$vol)

# 放送年が 2 桁表記を 4 桁表記に変換
listMatrix$date <- str_replace(string = listMatrix$date, pattern = "^1", replacement = "201")
listMatrix$date <- str_replace(string = listMatrix$date, pattern = "^0", replacement = "200")
listMatrix$date <- str_replace(string = listMatrix$date, pattern = "^9", replacement = "199")

# 放送日を - 区切りに変換
listMatrix$date <- str_replace_all(string = listMatrix$date, pattern = "\\.", replacement = "-")

# 1991/11/31 は存在しない日なので変換
listMatrix$date <- str_replace(string = listMatrix$date, pattern = "1991-11-31", replacement = "1991-12-01")

# 行名を削除
rownames(listMatrix) <- NULL

# 1 手から 5 手前の情報を追記
listMatrix$"1" <- listMatrix$hand[as.numeric(rownames(listMatrix)) + 1]
listMatrix$"2" <- listMatrix$hand[as.numeric(rownames(listMatrix)) + 2]
listMatrix$"3" <- listMatrix$hand[as.numeric(rownames(listMatrix)) + 3]
listMatrix$"4" <- listMatrix$hand[as.numeric(rownames(listMatrix)) + 4]
listMatrix$"5" <- listMatrix$hand[as.numeric(rownames(listMatrix)) + 5]

# NA を含む行を削除
listMatrix <- na.omit(listMatrix)

# 古い放送日から順に並べる
listMatrix <- listMatrix[order(listMatrix$vol),]

# 過去 5 手の内、 2 回連続同じ手がでているか否かの情報を追加
listMatrix$cont2 <- listMatrix$"1" == listMatrix$"2" | listMatrix$"2" == listMatrix$"3" | listMatrix$"3" == listMatrix$"4" | listMatrix$"4" == listMatrix$"5"

# 過去 5 手の内、 3 回連続同じ手がでているか否かの情報を追加
listMatrix$cont3 <- (listMatrix$"1" == listMatrix$"2" & listMatrix$"2" == listMatrix$"3") | (listMatrix$"2" == listMatrix$"3" & listMatrix$"3" == listMatrix$"4") | (listMatrix$"3" == listMatrix$"4" & listMatrix$"4" == listMatrix$"5")

# 過去 5 手の内、 4 回連続同じ手がでているか否かの情報を追加
listMatrix$cont4 <- (listMatrix$"1" == listMatrix$"2" & listMatrix$"2" == listMatrix$"3" & listMatrix$"3" == listMatrix$"4" ) | (listMatrix$"2" == listMatrix$"3" & listMatrix$"3" == listMatrix$"4" & listMatrix$"4" == listMatrix$"5" )

# 予測の為の情報抽出
predictDF <- tail(listMatrix, 1)[,c("hand", "1","2","3","4")]
names(predictDF) <- c("1","2","3","4","5")

# 過去 5 手の内、 2/3/4 回連続同じ手がでているか否かの情報を追加
predictDF$cont2 <- predictDF$"1" == predictDF$"2" | predictDF$"2" == predictDF$"3" | predictDF$"3" == predictDF$"4" | predictDF$"4" == predictDF$"5"
predictDF$cont3 <- (predictDF$"1" == predictDF$"2" & predictDF$"2" == predictDF$"3") | (predictDF$"2" == predictDF$"3" & predictDF$"3" == predictDF$"4") | (predictDF$"3" == predictDF$"4" & predictDF$"4" == predictDF$"5")
predictDF$cont4 <- (predictDF$"1" == predictDF$"2" & predictDF$"2" == predictDF$"3" & predictDF$"3" == predictDF$"4" ) | (predictDF$"2" == predictDF$"3" & predictDF$"3" == predictDF$"4" & predictDF$"4" == predictDF$"5" )

# 予測の為の行を整形
predictDF$vol <- tail(listMatrix,1)[,"vol"] + 1
predictDF$date <- as.character(as.Date(tail(listMatrix,1)[,"date"]) + 7)
predictDF$hand <- "unknown"

# リストに予測の為の行を追加
listMatrix <- rbind(listMatrix, predictDF)

# ファイルの書き出し
write.table(listMatrix, file="past5hands.csv", sep=",", row.names = FALSE)