with open("input.txt", "rb") as f:
    rucksacks = f.readlines()

sum_priority = 0
for i in range(0, len(rucksacks), 3):
    badge = {i for i in rucksacks[i][0:-1]}.intersection(rucksacks[i+1], rucksacks[i+2]).pop()
    if badge >= ord('a') and badge <= ord('z'):
        sum_priority += (badge - ord('a') + 1)
    elif badge >= ord('A') and badge <= ord('Z'):
        sum_priority += (badge - ord('A') + 27)
print(sum_priority)