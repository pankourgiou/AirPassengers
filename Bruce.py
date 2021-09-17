import time


t1 = time.time()

x = ("nature ignores sadness")

print(bool(x))

t2 = time.time()
t = t2 - t1
print("Elapsed time is : ", t, " seconds")
