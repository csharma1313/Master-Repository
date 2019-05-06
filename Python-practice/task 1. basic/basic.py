#learning basic
height=1.79
weight=67.3
bmi= weight/ height**2
print(bmi)

#to find type of a var
#type(bmi)

#lists

fam= [ ["liz", 1.79],
       ["paul", 1.9],
       ["mom", 1.4],
       ["dad", 1.8]]
print (fam)
#pyton uses indexes to access elements in lists
#0 based indexing
#to select paul
print(fam[1])
#negative indexes also work. to select last element -1 will work
print(fam[-1]) #prints dads info
# list slicing allows selecting multiple elements from a list
# start : end
#start is inclusive and end is exclusive
#if you leave out first element in range, it assumes from index zero
print (" list splicing : " + str(fam[1:3]))
#changing list elements
fam[3][1]=1.88
print(fam)
#adding new elements to list
fam = fam + [["me", 1.5]]
print("added element: ")
print(fam)
#deleting elements
del(fam[-1])
print("deleted elemnts list :")
print(fam)

#copying elements
x=["a", "b","C"]
print("x:", x)
y=x
y[1]="p"
print("y:", y)
print("x:", x) #x also reflects the changes made to y coz a reference of x is provided to y, hence changes made are done to
#the same address location in memory
#to create a new list with same contents as x use  list splicing like x[:3] or list function
z=list(x)
print("z:", z)

