// execute with groovy day23.groovy
filename = 'input_test_tiny.txt'

File fh = new File(filename)
def lines = fh.readLines()
for (line in lines) {
    print line + "\n"
}