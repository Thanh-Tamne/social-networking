import requests
from bs4 import BeautifulSoup, Tag
import csv

URL = "https://vi.wikipedia.org/wiki/Danh_s%C3%A1ch_phim_%C4%91i%E1%BB%87n_%E1%BA%A3nh_Vi%E1%BB%87t_Nam#Th%E1%BA%ADp_ni%C3%AAn_2020"
TARGET_ID = "Thập_niên_2020"

headers = {"User-Agent": "Mozilla/5.0"}
res = requests.get(URL, headers=headers, timeout=30)
res.raise_for_status()
res.encoding = "utf-8"
soup = BeautifulSoup(res.text, "lxml")

# B1: tìm <h3 id="Thập_niên_2020">
anchor = soup.find(id=TARGET_ID)
h3 = anchor if (anchor and anchor.name == "h3") else (anchor.find_parent("h3") if anchor else None)
if not h3:
    raise RuntimeError("Không tìm thấy <h3 id='Thập_niên_2020'>")

# B2: cha trực tiếp của h3
parent = h3.parent

# B3: tìm bảng wikitable cùng cấp với cha
table = None
for sib in parent.next_siblings:
    if isinstance(sib, Tag) and sib.name == "table" and "wikitable" in (sib.get("class") or []):
        table = sib
        break

if not table:
    raise RuntimeError("Không tìm thấy table.wikitable sau heading")

print("Đã tìm thấy table.wikitable")

# B4: lấy header (hàng th)
header_row = table.find("tr")
header = [th.get_text(strip=True) for th in header_row.find_all("th")]
print("Header:", header)

# B5: lấy data
rows = []
for tr in table.find_all("tr")[1:]:  # bỏ hàng header
    cols = [td.get_text(strip=True) for td in tr.find_all("td")]
    if cols:
        rows.append(cols)

print(f"Thu được {len(rows)} dòng dữ liệu")

# B6: ghi ra CSV
with open("movie.csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    if header:  # chỉ ghi nếu header có dữ liệu
        writer.writerow(header)
    writer.writerows(rows)

print("💾 Đã lưu dữ liệu vào movie.csv")