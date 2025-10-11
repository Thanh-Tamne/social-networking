## ==========================================
## Pha A — Chuẩn bị cho PhoBERT
## Đầu vào : description.csv  (có cột: category_name, description)
## Đầu ra  :
##   1) description_clean_pho.csv
##   2) auto_english_candidates.csv  (DF ≥ 50, len ≥ 3)
## ==========================================

## Cài & load thư viện
pkgs <- c("readr", "dplyr", "stringi", "hunspell")
for (p in pkgs) { if (!requireNamespace(p, quietly = TRUE)) install.packages(p); library(p, character.only = TRUE) }

IN_CSV      <- "description.csv"
OUT_CLEAN   <- "description_clean_pho.csv"
OUT_CAND    <- "auto_english_candidates.csv"
MIN_DF      <- 50   # token phải xuất hiện ở ≥ 50 mô tả
MIN_LEN     <- 3    # độ dài tối thiểu token (ASCII)

## Chuẩn hoá đơn vị/ký hiệu
normalize_units <- function(txt) {
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*(\"|”|“|''|inches|inch)\\b", "\\1inch", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*inch\\b", "\\1inch", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+)\\s*m\\s*a\\s*h\\b", "\\1mah", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*w\\b",  "\\1w",  txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*hz\\b", "\\1hz", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*mp\\b", "\\1mp", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*gb\\b", "\\1gb", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*tb\\b", "\\1tb", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*nm\\b", "\\1nm", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*mm\\b", "\\1mm", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+)\\s*fps\\b",               "\\1fps", txt, ignore.case = TRUE)
  txt <- gsub("\\b4\\s*[kK]\\b", "4k", txt)
  txt <- gsub("\\b8\\s*[kK]\\b", "8k", txt)
  txt <- gsub("\\b1080\\s*[pP]\\b", "1080p", txt)
  txt <- gsub("\\b720\\s*[pP]\\b",  "720p",  txt)
  txt <- gsub("(\\d{2,})\\s*(mah)\\b", "\\1\\2", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+),(\\d+)(inch|mm|nm|w|hz|mp|gb|tb)\\b", "\\1.\\2\\3", txt, ignore.case = TRUE)
  txt
}

## Làm sạch nền tảng
clean_base <- function(txt) {
  txt <- stringi::stri_replace_all_regex(txt, "<[^>]+>", " ")
  txt <- stringi::stri_replace_all_regex(txt, "(https?://|ftp://|www\\.)\\S+", " ")
  txt <- stringi::stri_replace_all_regex(txt, "[\\p{L}\\p{N}._%+-]+@[\\p{L}\\p{N}.-]+\\.[A-Za-z]{2,}", " ")
  txt <- stringi::stri_replace_all_regex(txt, "[\\r\\n\\t]+", " ")
  txt <- stringi::stri_trans_nfc(txt)
  txt <- normalize_units(txt)
  txt <- stringi::stri_trans_tolower(txt)
  txt <- stringi::stri_replace_all_regex(txt, "[\\p{P}\\p{S}]+", " ")
  txt <- stringi::stri_replace_all_regex(txt, "\\s+", " ")
  stringi::stri_trim_both(txt)
}

## Đọc & làm sạch mô tả, xuất file (1): description_clean_pho.csv
df <- readr::read_csv(IN_CSV, show_col_types = FALSE) %>%
  mutate(description = ifelse(is.na(description), "", description))
desc_clean <- vapply(df$description, clean_base, FUN.VALUE = character(1))
out_clean <- df %>% transmute(category_name, description = desc_clean)
readr::write_csv(out_clean, OUT_CLEAN)
cat(sprintf("[OK] Đã ghi: %s (rows=%d)\n", OUT_CLEAN, nrow(out_clean)))

## Trích token ASCII không dấu, unique theo dòng
line_tokens <- strsplit(desc_clean, "\\s+")
line_ascii_sets <- lapply(line_tokens, function(v) {
  u <- unique(tolower(v[grepl("^[a-z]+$", v)]))
  u[nchar(u) >= MIN_LEN]
})

## Tính DF, lọc theo DF & LEN
df_table <- table(unlist(line_ascii_sets))
cand <- data.frame(token = names(df_table), df = as.integer(df_table), row.names = NULL)
cand$len <- nchar(cand$token)
cand <- cand %>% dplyr::filter(df >= MIN_DF, len >= MIN_LEN)

## Check hunspell (tiếng Anh)
cand$is_english <- vapply(cand$token, function(tk){
  ok <- hunspell::hunspell_check(tk, dict = hunspell::dictionary("en_US"))
  if (length(ok) == 0) FALSE else ok
}, logical(1))

## Xuất file (2): auto_english_candidates.csv — để bạn mở ra lọc tay
cand <- cand[order(-cand$df, cand$token), c("token","df","len","is_english")]
readr::write_csv(cand, OUT_CAND)
cat(sprintf("[OK] Đã ghi: %s (tokens=%d, DF≥%d, len≥%d)\n", OUT_CAND, nrow(cand), MIN_DF, MIN_LEN))











###############################################################################


## ==========================================
## Pha B — Áp bộ lọc token cho PhoBERT
## Đầu vào :
##   - description_clean_pho.csv      (từ Pha A)
##   - auto_english_candidates.csv    (ĐÃ LỌC TAY: chỉ còn token CẦN LOẠI)
## Đầu ra  :
##   - description_phobert_ready.csv  (chuẩn để embed PhoBERT)
## ==========================================

## ---- Cài & load thư viện ----
pkgs <- c("readr", "dplyr", "stringi", "stopwords")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p)
  library(p, character.only = TRUE)
}

## ---- Cấu hình ----
IN_DESC <- "description_clean_pho.csv"
IN_DROP <- "auto_english_candidates.csv"   # file đã lọc tay
OUT_CSV <- "description_phobert_final.csv"

## ---- Đọc dữ liệu ----
desc <- readr::read_csv(IN_DESC, show_col_types = FALSE)
stopifnot(all(c("category_name","description") %in% names(desc)))
desc$description[is.na(desc$description)] <- ""

drop_df <- readr::read_csv(IN_DROP, show_col_types = FALSE)
stopifnot("token" %in% names(drop_df))

## ---- Danh sách cấm (lọc tay + stopwords EN) ----
en_sw <- stopwords::stopwords("en", source = "stopwords-iso")

banned <- unique(c(
  tolower(trimws(drop_df$token)),   # token bạn lọc tay
  en_sw                             # stopwords tiếng Anh phổ biến
))
banned <- banned[nchar(banned) > 0 & !is.na(banned)]

cat(sprintf("Tổng số token sẽ bị loại (EN only): %d\n", length(banned)))

## ---- Hàm loại token bị cấm ----
drop_banned_tokens <- function(text, banned) {
  if (!nzchar(text) || length(banned) == 0) return(text)
  toks <- unlist(strsplit(text, "\\s+"))
  keep <- !(tolower(toks) %in% banned)
  kept <- toks[keep]
  if (!length(kept)) return("")
  out <- paste(kept, collapse = " ")
  out <- gsub("\\s+", " ", out)
  trimws(out)
}

## ---- Áp bộ lọc ----
desc$description <- vapply(desc$description, drop_banned_tokens, FUN.VALUE = character(1), banned = banned)

## ---- Ghi kết quả ----
out <- desc %>% dplyr::select(category_name, description)
readr::write_csv(out, OUT_CSV)

print(head(out, 20))


#####################################

pkgs <- c("readr","dplyr","stringi")
for (p in pkgs) { if (!requireNamespace(p, quietly=TRUE)) install.packages(p); library(p, character.only=TRUE) }

IN_FINAL <- "description_phobert_final.csv"
OUT_DF5  <- "auto_english_candidates_df5.csv"

df <- readr::read_csv(IN_FINAL, show_col_types = FALSE)
df$description[is.na(df$description)] <- ""
line_tokens <- strsplit(df$description, "\\s+")
line_ascii_sets <- lapply(line_tokens, function(v){
  u <- unique(tolower(v[grepl("^[a-z]+$", v)])); u[nchar(u) >= 3]
})
tab <- table(unlist(line_ascii_sets))
cand5 <- data.frame(token = names(tab), df = as.integer(tab))
cand5 <- cand5[cand5$df >= 5 & nchar(cand5$token) >= 3, , drop=FALSE]
if (nrow(cand5) > 0) readr::write_csv(cand5[order(-cand5$df, cand5$token), ], OUT_DF5)


########################################
# Áp bộ lọc DF≥5 vào mô tả đã xử lý PhoBERT
pkgs <- c("readr","dplyr","stringi")
for (p in pkgs) { if (!requireNamespace(p, quietly=TRUE)) install.packages(p); library(p, character.only=TRUE) }

IN_FINAL <- "description_phobert_final.csv"   
IN_DF5   <- "auto_english_candidates_df5.csv" 
OUT_CSV  <- "description_phobert(1).csv"

stopifnot(file.exists(IN_FINAL), file.exists(IN_DF5))

desc <- readr::read_csv(IN_FINAL, show_col_types = FALSE)
stopifnot(all(c("category_name","description") %in% names(desc)))
desc$description[is.na(desc$description)] <- ""

df5 <- readr::read_csv(IN_DF5, show_col_types = FALSE)
stopifnot("token" %in% names(df5))
banned <- unique(tolower(trimws(df5$token)))
banned <- banned[nchar(banned) > 0 & !is.na(banned)]

drop_banned_tokens <- function(text, banned) {
  if (!nzchar(text) || length(banned) == 0) return(text)
  toks <- unlist(strsplit(text, "\\s+"))
  keep <- !(tolower(toks) %in% banned)
  kept <- toks[keep]
  if (!length(kept)) return("")
  out <- paste(kept, collapse = " ")
  out <- gsub("\\s+", " ", out)
  stringi::stri_trim_both(out)
}

desc$description <- vapply(desc$description, drop_banned_tokens, FUN.VALUE = character(1), banned = banned)

out <- desc %>% dplyr::select(category_name, description)
readr::write_csv(out, OUT_CSV)


print(head(out, 20))
