// execute with groovy day23.groovy
filename = 'input.txt'

File fh = new File(filename)
def lines = fh.readLines()
def elfs = [:]
for (int y = 0; y < lines.size(); y++) {
    for (int x = 0; x < lines[y].length(); x++) {
        if (lines[y][x] == '#') {
            elfs[new Tuple(x,y)] = 1
        } 
    }
}

def elfMoved = true
def directions = ["N", "S", "W", "E"]
def first_direction = 0
def nb_round = 0
while (elfMoved) {
    elfMoved = false

    // First half: propose movements
    def proposed = [:]
    for (elf in elfs) {
        
        def x = elf.key.get(0)
        def y = elf.key.get(1)
        def nw = new Tuple(x-1, y-1)
        def n = new Tuple(x, y-1)
        def ne = new Tuple(x+1, y-1)
        def w = new Tuple(x-1, y)
        def e = new Tuple(x+1, y)
        def sw = new Tuple(x-1, y+1)
        def s = new Tuple(x, y+1)
        def se = new Tuple(x+1, y+1)

        // Move only when one adjacent
        if (elfs.containsKey(nw) || elfs.containsKey(n) || elfs.containsKey(ne) ||
            elfs.containsKey(w) || elfs.containsKey(e) ||
            elfs.containsKey(sw) || elfs.containsKey(s) || elfs.containsKey(se))
        {
            // Propose something
            def has_proposed = false
            for (int i = 0; i < directions.size() && !has_proposed; i++)
            {
                direction = directions[(first_direction + i) % directions.size()]
                if (direction == "N" && !elfs.containsKey(nw) && !elfs.containsKey(n) && !elfs.containsKey(ne)) {
                    if (!proposed.containsKey(n)) {
                        proposed[n] = []
                    }
                    proposed[n].add(elf.key)
                    has_proposed = true
                }
                if (direction == "S" && !elfs.containsKey(sw) && !elfs.containsKey(s) && !elfs.containsKey(se)) {
                    if (!proposed.containsKey(s)) {
                        proposed[s] = []
                    }
                    proposed[s].add(elf.key)
                    has_proposed = true
                }
                if (direction == "W" && !elfs.containsKey(w) && !elfs.containsKey(nw) && !elfs.containsKey(sw)) {
                    if (!proposed.containsKey(w)) {
                        proposed[w] = []
                    }
                    proposed[w].add(elf.key)
                    has_proposed = true
                }
                if (direction == "E" && !elfs.containsKey(e) && !elfs.containsKey(ne) && !elfs.containsKey(se)) {
                    if (!proposed.containsKey(e)) {
                        proposed[e] = []
                    }
                    proposed[e].add(elf.key)
                    has_proposed = true
                }
            }
        }
    }

    // Second half: move!
    for (p in proposed) {
        if (p.value.size() == 1) {
            elfs.remove(p.value[0])
            elfs[p.key] = 1
            elfMoved = true
        }
    }

    // Change first direction
    first_direction = (first_direction + 1) % directions.size()

    // Part 1 result
    nb_round += 1
    if (nb_round == 10) {
        def firstElf = elfs.entrySet().iterator().next()
        def minX = firstElf.getKey().get(0)
        def maxX = firstElf.getKey().get(0)
        def minY = firstElf.getKey().get(1)
        def maxY = firstElf.getKey().get(1)

        for (elf in elfs) {
            def x = elf.key.get(0)
            def y = elf.key.get(1)
            if (x < minX) {
                minX = x
            }
            if (x > maxX) {
                maxX = x
            }
            if (y < minY) {
                minY = y
            }
            if (y > maxY) {
                maxY = y
            }
        }

        def part1 = (maxX - minX + 1) * (maxY - minY + 1) - elfs.size()
        print "Part 1: "
        print part1
        print "\n"
    }
}

print "Part 2: "
print nb_round
print "\n"