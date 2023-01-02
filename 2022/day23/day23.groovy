// execute with groovy day23.groovy
filename = 'input_test_tiny.txt'

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

    print proposed
    print "\n"

    // Second half: move!

}