def main():
    year_list = {
    "January" : 1,
    "February": 2,
    "March" : 3,
    "April" : 4,
    "May" : 5,
    "June" : 6,
    "July" : 7,
    "August" :8,
    "September" : 9,
    "October" : 10,
    "November" : 11,
    "December" : 12,
    }
    while True:
            try:
                input_date = input("Date:")
                try:
                    month,day,year = input_date.split("/")
                    month = int(month)
                    day = int(day)
                    year = int(year)
                    if month < 1 or month > 12:
                        raise ValueError
                    if day > 31:
                         raise ValueError
                    print(f"{year}-{month:02d}-{day:02d}")
                    break
                except:
                    month,day,year = input_date.split(" ")
                    if not day.endswith(","):
                        raise ValueError
                    day = day.replace(",","")
                    day = int(day)
                    year = int(year)
                    if month in year_list:
                        mm = year_list[month]
                    if mm < 1 or mm > 12:
                        raise ValueError
                    if day > 31:
                        raise ValueError
                    print(f"{year}-{mm:02d}-{day:02d}")
                    break
            except:
                 pass

main()
