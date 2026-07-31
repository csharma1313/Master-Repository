print("printing entire list as is")
students = ["Hermione", "Harry", "Ron"]
print(students)
#prints entire list just like it looks above

#prints item one by one
print("printing list items one by one")
for student in students:
    print(student)

#using len for iteration through list
print("printing using len")
for i in range(len(students)):
    print(i+1, students[i])

houses = ["Gryffindor", "Gryffindor", "Gryffindor", "Slytherin"]
students = ["Hermione", "Harry", "Ron", "Draco"]

#to make them related use python dictionary
print("printing using dictionary")
students = {"Hermione":"Gryffindor",
            "Harry":"Gryffindor", 
            "Ron":"Gryffindor",
            "Draco":"Slytherin"}

print("printing thorugh key in dictionary")
print(students["Hermione"])

print("printing dynamically in dictionary")
for student in students:
    print(student, students[student], sep = ", ")

