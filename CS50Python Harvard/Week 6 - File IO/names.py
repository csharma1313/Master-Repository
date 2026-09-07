
'''
names = []

for _ in range(3):
    names.append("What's your name?")

for name in sorted(names):
    print(f"hello, {name}")

'''

'''
#to append to a file
name = input("What's your name?")
with open("names.txt", "a") as file:
    file.write(f"{name}\n")

#to read from a file
with open("names.txt", "r") as file:
    lines = file.readlines()

for line in lines:
    print("hello,",line.rstrip())

'''

with open("names.txt", "r") as file:
    for line in file:
        print("hello,",line.rstrip())


#sorting names
'''
print("printing sorted")
names=[]

with open("names.txt") as file:
    for line in sorted(file):
        print("hello,",line.rstrip())
'''

print("printing sorted")
names=[]

with open("names.txt") as file:
    for line in file:
        names.append(line.rstrip())
        
for name in sorted(names):
    print(f"hello, {name}")