with open("input.txt", "rb") as f:
    rucksacks = f.readlines()

sum_priority = 0
for rucksack in rucksacks:
    if rucksack[-1] == ord('\n') :
        rucksack = rucksack[:-1]
    size = len(rucksack)
    duplicate = {i for i in rucksack[0:size//2]}.intersection(rucksack[size//2:]).pop()
    if duplicate >= ord('a') and duplicate <= ord('z'):
        sum_priority += (duplicate - ord('a') + 1)
    elif duplicate >= ord('A') and duplicate <= ord('Z'):
        sum_priority += (duplicate - ord('A') + 27)
print(sum_priority)
    