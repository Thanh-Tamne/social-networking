# =============================================================================
# XỬ LÝ CHUỖI VÀ BIỂU THỨC CHÍNH QUY TRONG R
# =============================================================================

# Tải các thư viện cần thiết
library(tidyverse)
library(stringr)

# =============================================================================
# 1. BIẾN KIỂU KÝ TỰ (CHARACTER VARIABLE)
# =============================================================================

# --- Dấu ngoặc kép ---
# Trong R, dấu ngoặc kép đơn '' và kép đôi "" hoạt động giống nhau

text <- "String"
text_nested <- 'Single quotation mark with "nested" double quotation'


# --- Cách thêm dấu ngoặc vào trong chuỗi ---

# Cách 1: Lồng dấu ngoặc khác loại
one_quote_single <- "'"
one_quote_double <- '"'

# Cách 2: Sử dụng dấu gạch chéo ngược (backslash)
one_quote_single <- '\''
one_quote_double <- "\""

one_quote_double

# Để xem nội dung thực của chuỗi, dùng hàm writeLines()
writeLines(one_quote_single)  # Hiển thị: '
writeLines(one_quote_double)  # Hiển thị: "

# --- Cách thêm dấu gạch chéo ngược vào chuỗi ---
# Phải viết hai dấu gạch chéo ngược \\

text_backslash1 <- "Short \ long"      # SAI - dấu \ bị hiểu là ký tự escape
text_backslash2 <- "Short \\ long"     # ĐÚNG - \\ tạo ra một dấu \

text_backslash1  # Kết quả không như mong đợi
text_backslash2  # Hiển thị: [1] "Short \\ long"

writeLines(text_backslash1)  # Hiển thị: Short  long
writeLines(text_backslash2)  # Hiển thị: Short \ long

# =============================================================================
# 2. VECTOR KÝ TỰ (CHARACTER VECTOR)
# =============================================================================

# Tạo vector chứa các ký tự đặc biệt
character_vector <- c("\"", "\\")
character_vector
writeLines(character_vector)

# --- Các ký tự đặc biệt thường gặp ---
# "\n"   - Xuống dòng (newline)
# "\t"   - Tab ngang
# "\uXXXX" - Ký tự Unicode

character_vector2 <- c("separation\ttabulation and jump\ninto a newline")
character_vector2
writeLines(character_vector2)

# --- Ví dụ về ký tự Unicode ---
# "\u00b6" - Ký hiệu đoạn văn (PILCROW SIGN ¶)
# "\u00b5" - Ký hiệu micro (MICRO SIGN µ)
# "\u00bb" - Mũi tên phải (RIGHT POINTING »)

character_vector3 <- c("\u00b6", "\u00b5", "\u00bb")
character_vector3
writeLines(character_vector3)

# --- Ví dụ về emoji (ký tự biểu tượng) ---
# "\u266c" - Nốt nhạc (♬)
# "\u2605" - Ngôi sao đen (★)
# "\u260f" - Điện thoại (☏)

character_vector4 <- c("\u266c", "\u2605", "\u260f")
character_vector4
writeLines(character_vector4)

# =============================================================================
# 3. BIỂU THỨC CHÍNH QUY (REGULAR EXPRESSIONS) - CƠ BẢN
# =============================================================================
# Tạo vector ví dụ
expression <- c("international", "associations", "intra-organisational", 
                "foundations", "technical", "institutions")

# --- Tìm kiếm chuỗi văn bản đơn giản ---
# Biểu thức chính quy đơn giản nhất là một đoạn văn bản
str_view(expression, "national")  # Tìm từ "national"
str_view(expression, "al")        # Tìm "al"
str_view(expression, "int")       # Tìm "int"
str_view(expression, "tions")     # Tìm "tions"

# --- Dấu chấm (.) khớp với bất kỳ ký tự nào (trừ xuống dòng) ---
str_view(expression, ".a.")  # Tìm: ký tự bất kỳ + "a" + ký tự bất kỳ
str_view(expression, "t.")   # Tìm: "t" + ký tự bất kỳ
str_view(expression, ".o")   # Tìm: ký tự bất kỳ + "o"

# --- Cách tìm dấu chấm thực sự ---
# Nếu dấu chấm "." khớp với bất kỳ ký tự nào, làm sao để tìm dấu "." thực?
#
# Trong regex: viết \.
# Nhưng vì dấu \ cũng là ký tự escape trong chuỗi R
# Nên để tạo regex \.
# Trong R phải viết "\\."
#
# Viết regex là: \.
# Viết chuỗi biểu diễn regex trong R là: "\\."

dot <- "\\."
writeLines(dot)  # Hiển thị: \.

expression_short <- c("int.", "assoc.", "intra-org.", 
                      "found.", "tech.", "instit.")
str_view(expression_short, "\\.")  # Tìm dấu chấm thực

# --- Cách tìm dấu gạch chéo ngược thực sự ---
# Nếu dấu gạch chéo ngược "\" được dùng làm ký tự escape, làm sao tìm "\" thực?
#
# Trong regex: viết \\
# Trong R: phải viết "\\\\" (bốn dấu gạch chéo!)

text_backslash2 <- "Short \\ long"
text_backslash2
writeLines(text_backslash2)

str_view(text_backslash2, "\\\\")  # Tìm dấu \ thực

x <- "a \\ b"
writeLines(x)
str_view(x, "\\\\")

# --- Neo đầu và cuối chuỗi ---
# ^ - Khớp với đầu chuỗi
# $ - Khớp với cuối chuỗi

expression <- c("international", "associations", "intra-organisational", 
                "foundations", "technical", "institutions")

str_view(expression, "^i")     # Tìm chuỗi bắt đầu bằng "i"
str_view(expression, "^f")     # Tìm chuỗi bắt đầu bằng "f"

str_view(expression, "al$")    # Tìm chuỗi kết thúc bằng "al"
str_view(expression, "tions$") # Tìm chuỗi kết thúc bằng "tions"

# Kết hợp ^ và $ để khớp chính xác toàn bộ chuỗi
cake <- c("donut", "custard donut", "donut with plum", "pudding donut")
str_view(cake, "donut")    # Tìm "donut" ở bất kỳ đâu trong chuỗi
str_view(cake, "^donut$")  # Chỉ khớp chuỗi "donut" chính xác

# --- Tập hợp ký tự và toán tử OR ---
# [abc]  - Khớp với a HOẶC b HOẶC c
# [^abc] - Khớp với bất kỳ ký tự nào NGOẠI TRỪ a, b, c
# |      - Toán tử "hoặc" (alternation)

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), ".[*]c")   
# Tìm: ký tự bất kỳ + * + c

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), ".[^*]c")  
# Tìm: ký tự bất kỳ + (không phải *) + c

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), "a[\\.]c") 
# Tìm: a + . + c

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), ".(\\.|8)c") 
# Tìm: ký tự bất kỳ + (. hoặc 8) + c

str_view(c("grey", "gray"), "gr(e|a)y")  
# Tìm: gr + (e hoặc a) + y

# --- Lớp ký tự đặc biệt ---
# \d - Khớp với bất kỳ chữ số nào (0-9)
# \s - Khớp với khoảng trắng (space, tab, newline)

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), "\\d")          
# Tìm chữ số

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), "\\s")          
# Tìm khoảng trắng

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), ".(\\.|\\d)c")  
# Tìm: ký tự bất kỳ + (. hoặc chữ số) + c

str_view(c("abc", "a.c", "a*c", "a c", "a8c"), ".(\\d|\\s)c")  
# Tìm: ký tự bất kỳ + (chữ số hoặc khoảng trắng) + c

# =============================================================================
# BÀI TẬP THỰC HÀNH
# =============================================================================

# -------------------------------------------------#
# Bài tập 1 ----
# Cho vector chuỗi:

vector <- c("emoticon", ":)", "symbol", "$^$")
writeLines((vector))

# Sử dụng hàm str_view() để tìm:
# a) Chuỗi có 3 ký tự với chữ "o" ở giữa
str_view(vector, ".o.")
# b) Từ "emoticon"
str_view(vector, "emoticon")

# c) Biểu tượng ":)"
str_view(vector, "[:][)]")
# d) Biểu tượng "$^$"
str_view(vector, "[$][\\^][$]")


# -------------------------------------------------#
# Bài tập 2 ----
# Cho bộ từ vựng 980 từ:
stringr::words

# Sử dụng hàm str_view() để tìm:
# a) Tất cả từ chứa từ "yes" (thêm tham số match=T)
# b) Tất cả từ bắt đầu bằng "w"
# c) Tất cả từ kết thúc bằng "x"


# -------------------------------------------------#
# Bài tập 3 ----
# Cho bộ từ vựng 980 từ:
stringr::words

# Sử dụng hàm str_view() để tìm:
# a) Tất cả từ bắt đầu bằng nguyên âm
# b) Tất cả từ chỉ bắt đầu bằng phụ âm
# c) Tất cả từ kết thúc bằng "ing" hoặc "ise"
# d) Tất cả từ kết thúc bằng "ed" nhưng không phải "eed"

# -------------------------------------------------#
