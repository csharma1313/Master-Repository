# Definition of radius
import math

# task 1
r = 0.43

# Import the math package
# import math
# in pycharm we use settings>project interpreter to add new packages

# Calculate C
C = 2 * math.pi * r

# Calculate A
A = math.pi * r * r

# Build printout
print("Circumference: " + str(C))
print("Area: " + str(A))

# task 2
# Definition of radius
r = 192500

# Import radians function of math package
from math import radians

# Travel distance of Moon over 12 degrees. Store in dist.
dist = r * radians(12)

# Print out dist
print(dist)
