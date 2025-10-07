from selenium import webdriver
from selenium.webdriver.common.by import By
import pandas as pd
import time

# 1. Mở Chrome
options = webdriver.ChromeOptions()
options.add_argument("--start-maximized")
driver = webdriver.Chrome(options=options)

# 2. Mở trang Wikipedia
url = "https://vi.wikipedia.org/wiki/Danh_s%C3%A1ch_phim_%C4%91i%E1%BB%87n_%E1%BA%A3nh_Vi%E1%BB%87t_Nam#Th%E1%BA%ADp_ni%C3%AAn_2020"
driver.get(url)
time.sleep(3)

# 3. Scroll tới mục "Thập niên 2020"
section = driver.find_element(By.ID, "Thập_niên_2020")
driver.execute_script("arguments[0].scrollIntoView();", section)
time.sleep(2)

# 4. Lấy bảng ngay sau heading
table = section.find_element(By.XPATH, "./following::table[contains(@class,'wikitable')][1]")

# 5. Lấy tất cả các hàng
rows = table.find_elements(By.TAG_NAME, "tr")

data = []
for row in rows[1:]:  # bỏ hàng tiêu đề
    cols = row.find_elements(By.TAG_NAME, "td")
    if len(cols) == 5:
        ten_phim = cols[0].text.strip()
        dao_dien = cols[1].text.strip()
        dien_vien = cols[2].text.strip()
        the_loai = cols[3].text.strip()
        khoi_chieu = cols[4].text.strip()
        
        data.append([ten_phim, dao_dien, dien_vien, the_loai, khoi_chieu])

# 6. Lưu thành CSV
df = pd.DataFrame(data, columns=["Tên phim", "Đạo diễn", "Diễn viên", "Thể loại", "Chính thức khởi chiếu"])
df.to_csv("phim_viet_thap_nien_2020.csv", index=False, encoding="utf-8-sig")

print("Đã lưu phim_viet_thap_nien_2020.csv với", len(df), "dòng")

driver.quit()
