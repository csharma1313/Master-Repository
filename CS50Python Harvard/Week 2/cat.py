print("while loop decrementing")
i = 3
while i != 0:
    print("meow")
    i -= 1
print("------------------------------------")

print("while loop incrementing")
i = 0
while i < 3:
    print("meow")
    i += 1 
print("------------------------------------")

print("for loop")
for i in [0, 1, 2]: #i iterating over list
    print("meow")

print("for loop")
for _ in range(3): #loop using range goes upto 3
    print("meow")
print("------------------------------------")

print("iterating by multiplying")
print("meow" * 3) #leaves no space between each iteration print
print("meow \n" * 3, end="")
print("------------------------------------")

print("User requested iteration")
while True:
    n = int(input("What's n?"))
    if n > 0:
        break

for _ in range(n):
    print("meow")
print("------------------------------------")

print("iterating through function")
def main():
    number = get_number()
    meow(number)

def get_number():
    while True:
       n = int(input("What's n?"))
       if n > 0:
            break
    return n  

def meow(n):
    for _ in range(n):
        print("meow")
print("------------------------------------")