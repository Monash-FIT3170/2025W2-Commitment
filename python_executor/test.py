import time

print("waiting 2 seconds")
for i in range(200):
  print(i/100)
  time.sleep(0.01)
print("done")
