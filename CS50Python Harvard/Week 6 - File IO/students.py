'''
with open("students.csv") as file:
    for line in file:
        row = line.rstrip().split(",")
        print(f"{row[0]} is in {row[1]}")

print("\n better way of printing:")
with open("students.csv") as file:
    for line in file:
        name, house = line.rstrip().split(",")
        print(f"{name} is in {house}")


print("\n sorting the file:")
students = []
with open("students.csv") as file:
    for line in file:
        name, house = line.rstrip().split(",")
        students.append(f"{name} is in {house}")

for student in sorted(students):
    print(student)



print("\n printing with dictionary sorted by name:")
students_l = []
with open("students.csv") as file:
    for line in file:
        name, house = line.rstrip().split(",")
        student_d = {"name":name, "house": house}
        students_l.append(student_d)

for student in sorted(students_l, key=lambda student:student["name"]):
    print(f"{student['name']} is in {student['house']}")

'''

print("\n printing with dictionary sorted by home:")

import csv
students = []

with open("students.csv") as file:
    reader = csv.DictReader(file)
    for row in reader:
        students.append({"name": row["name"], "home": row["home"]})

for student in sorted(students, key=lambda student:student["name"]):
    print(f"{student['name']} is from {student['home']}")



