import csv

name = input("whats your name?")
home = input("wheres your home?")

with open("students.csv","a", newline="") as file:
    writer = csv.DictWriter(file, fieldnames =["name","home"])
    writer.writerow({"name":name,"home":home})
