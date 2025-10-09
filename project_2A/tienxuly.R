## setup thư viện
pkgs <- c("readr","dplyr","purrr")
for (p in pkgs) { if (!requireNamespace(p, quietly=TRUE)) install.packages(p); library(p, character.only=TRUE) }

## 1) Đếm dòng từng file 
files <- sprintf("mota%d.csv", 1:14)
stopifnot(all(file.exists(files)))
rows_info <- do.call(rbind, lapply(files, function(fp) {
  data.frame(file = basename(fp), n_rows = nrow(readr::read_csv(fp, show_col_types = FALSE)))
}))
print(rows_info)

## 2) Gộp & chuẩn cột 
needed <- c("category_name","product_name","product_url","description")
read_one <- function(fp){
  df <- readr::read_csv(fp, show_col_types = FALSE)
  miss <- setdiff(needed, names(df)); if (length(miss)) df[, miss] <- NA_character_
  dplyr::select(df, dplyr::any_of(needed)) |>
    dplyr::mutate(source_file = basename(fp))
}
mota_all <- purrr::map_dfr(files, read_one)
readr::write_csv(mota_all, "mota_all.csv")

##  3) Lấy 2 cột & thống kê thiếu 
df_all   <- readr::read_csv("mota_all.csv", show_col_types = FALSE)
df_small <- dplyr::select(df_all, category_name, description)
readr::write_csv(df_small, "description.csv")

n_missing <- sum(is.na(df_small$description) | trimws(df_small$description) == "")
cat("Tổng dòng:", nrow(df_small), " | Thiếu mô tả:", n_missing, "\n")

missing_by_label <- df_small |>
  dplyr::mutate(is_missing = is.na(description) | trimws(description) == "") |>
  dplyr::group_by(category_name) |>
  dplyr::summarise(total = dplyr::n(), missing = sum(is_missing),
                   pct_missing = round(100*missing/total,2), .groups="drop") |>
  dplyr::arrange(dplyr::desc(missing))
print(missing_by_label, n = 50)

## 4) Xóa dòng thiếu
df_clean <- df_small |>
  dplyr::filter(!(is.na(description) | trimws(description) == ""))
readr::write_csv(df_clean, "description.csv")
cat("Sau khi xóa thiếu, còn:", nrow(df_clean), "dòng\n")

# (tuỳ chọn) thống kê sau khi xóa
clean_by_label <- df_clean |>
  dplyr::group_by(category_name) |>
  dplyr::summarise(total = dplyr::n(), .groups="drop") |>
  dplyr::arrange(dplyr::desc(total))
print(clean_by_label, n = 50)

## =========================
## Tiền xử lý CHO PHOBERT (tiếng Việt)
## Đầu vào:  description.csv  (cột: category_name, description)
## Đầu ra :  description_phobert.csv
## =========================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringi)
})

## 1) Đọc dữ liệu (đã xoá dòng thiếu ở bước trước)
df <- readr::read_csv("description.csv", show_col_types = FALSE) %>%
  dplyr::mutate(
    description   = ifelse(is.na(description), "", description),
    category_name = ifelse(is.na(category_name), "unknown", category_name)
  )

## 2) Chuẩn hoá đơn vị/ký hiệu: 6.7" -> 6.7inch, 5000 mAh -> 5000mah, 120 Hz -> 120hz, 1080 p -> 1080p, 4k/8k...
normalize_units <- function(txt) {
  # inch (6.7", 6.7"", inches, inch) -> 6.7inch
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*(\"|”|“|''|inches|inch)\\b", "\\1inch", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*inch\\b", "\\1inch", txt, ignore.case = TRUE)
  
  # mAh, W, Hz, MP, GB/TB, nm, mm, fps
  txt <- gsub("(\\d+)\\s*m\\s*a\\s*h\\b", "\\1mah", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*w\\b",  "\\1w",  txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*hz\\b", "\\1hz", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*mp\\b", "\\1mp", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*gb\\b", "\\1gb", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*tb\\b", "\\1tb", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*nm\\b", "\\1nm", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+(?:[\\.,]\\d+)?)\\s*mm\\b", "\\1mm", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+)\\s*fps\\b",               "\\1fps", txt, ignore.case = TRUE)
  
  # 4K/8K/1080p/720p
  txt <- gsub("\\b4\\s*[kK]\\b", "4k", txt)
  txt <- gsub("\\b8\\s*[kK]\\b", "8k", txt)
  txt <- gsub("\\b1080\\s*[pP]\\b", "1080p", txt)
  txt <- gsub("\\b720\\s*[pP]\\b",  "720p",  txt)
  
  # "5000 mAh" -> "5000mah", "6,7inch" -> "6.7inch"
  txt <- gsub("(\\d{2,})\\s*(mah)\\b", "\\1\\2", txt, ignore.case = TRUE)
  txt <- gsub("(\\d+),(\\d+)(inch|mm|nm|w|hz|mp|gb|tb)\\b", "\\1.\\2\\3", txt, ignore.case = TRUE)
  txt
}

## 3) Làm sạch nền tảng (an toàn Unicode, không làm “vỡ chữ”)
clean_base <- function(txt) {
  # bỏ HTML/URL/email/xuống dòng-tab
  txt <- stri_replace_all_regex(txt, "<[^>]+>", " ")
  txt <- gsub("https?://\\S+|www\\.\\S+|ftp://\\S+", " ", txt)
  txt <- gsub("[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,}", " ", txt)
  txt <- gsub("[\\r\\n\\t]+", " ", txt)
  
  # chuẩn Unicode
  txt <- stri_trans_nfc(txt)
  txt <- stri_trans_nfkc(txt)
  
  # gỡ sạch ký tự vô hình/điều khiển (LIỆT KÊ RÕ, không dùng range Unicode)
  invisible_pattern <- "\u200B|\u200C|\u200D|\uFEFF|\u2060|\u00AD|\u202A|\u202B|\u202C|\u202D|\u202E|\u2066|\u2067|\u2068|\u2069"
  txt <- stri_replace_all_regex(txt, invisible_pattern, "")
  
  # chuẩn hoá đơn vị/ký hiệu
  txt <- normalize_units(txt)
  
  # hạ chữ thường
  txt <- tolower(txt)
  
  # chỉ giữ chữ cái (vi & en), số, khoảng trắng
  keep <- "[^a-z0-9àáảãạăắằẳẵặâầấẩẫậèéẻẽẹêềếểễệìíỉĩịòóỏõọôồốổỗộơờớởỡợùúủũụưừứửữựỳýỷỹỵđ\\s]"
  txt <- gsub(keep, " ", txt)
  
  # gom space
  txt <- gsub("\\s+", " ", txt)
  trimws(txt)
}

## 4) Heuristic lọc tiếng Việt cho PhoBERT (giữ token VI + token kỹ thuật)
filter_vi_for_phobert <- function(text_line) {
  if (is.na(text_line) || nchar(text_line) == 0) return("")
  toks <- unlist(strsplit(text_line, "\\s+"))
  
  # token kỹ thuật: số + đơn vị / độ phân giải / fps
  is_tech <- grepl("^\\d+(?:\\.\\d+)?(mah|hz|mp|gb|tb|inch|mm|nm|fps)$", toks) |
    grepl("^(4k|8k|1080p|720p)$", toks)
  
  # token chứa ít nhất một ký tự tiếng Việt có dấu/đặc trưng (đ, â, ê, ô, ă, ơ, ư…)
  vi_char_class <- "àáảãạăắằẳẵặâầấẩẫậèéẻẽẹêềếểễệìíỉĩịòóỏõọôồốổỗộơờớởỡợùúủũụưừứửữựỳýỷỹỵđ"
  is_vi <- grepl(sprintf("[%s]", vi_char_class), toks)
  
  # token ASCII thuần: giữ lại nếu là số hoặc token kỹ thuật; token chữ ASCII sẽ bị loại (hạn chế tiếng Anh)
  keep <- is_tech | is_vi | grepl("^\\d+$", toks)
  
  kept <- toks[keep]
  if (!length(kept)) return("")
  paste(kept, collapse = " ")
}

## 5) Tạo & ghi file PhoBERT
df_phobert <- df %>%
  mutate(
    description = clean_base(description),
    description = vapply(description, filter_vi_for_phobert, FUN.VALUE = character(1))
  ) %>%
  select(category_name, description)

readr::write_csv(df_phobert, "description_phobert.csv")
cat("[OK] Đã ghi: description_phobert.csv (chỉ tiếng Việt + token kỹ thuật)\n")

## 6) In mẫu nhanh để kiểm tra
head_idx <- 1:min(10, nrow(df_phobert))
print(df_phobert[head_idx, ])