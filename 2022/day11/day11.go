package main

import (
	"os"
	"fmt"
	"log"
	"strings"
	"strconv"
	"sort"
)

type Monkey struct {
	Items []int
	Operation []string
	Test int
	IfTrue int
	IfFalse int
	NbInspected int
}

func parseOperant(operant string, old int) int {
	switch operant {
	case "old":
		return old
	default:
		op, err := strconv.Atoi(operant)
		if err != nil {
			log.Fatalf("unable to convert operant: %v", err)
		}
		return op
	}
}

func main() {
	body, err := os.ReadFile("input.txt")
    if err != nil {
        log.Fatalf("unable to read file: %v", err)
    }

	// Read the monkeys
	monkeys := make([]Monkey, 0)
	var currentMonkey Monkey
	lines := strings.Split(string(body), "\n")
	for i := 0; i < len(lines); i++ {
		if (strings.HasPrefix(lines[i], "Monkey")) {
			currentMonkey = Monkey{}
		}
		if (strings.HasPrefix(lines[i], "  Starting items: ")) {
			itemStr := strings.TrimPrefix(lines[i], "  Starting items: ")
			items := strings.Split(itemStr, ", ")
			for j := 0; j < len(items); j++ {
				item, err := strconv.Atoi(items[j])
				if err != nil {
					log.Fatalf("unable to convert Test value: %v", err)
				}
				currentMonkey.Items = append(currentMonkey.Items, item)
			}
		}
		if (strings.HasPrefix(lines[i], "  Operation: new = ")) {
			itemStr := strings.TrimPrefix(lines[i], "  Operation: new = ")
			currentMonkey.Operation = strings.Split(itemStr, " ")
		}
		if (strings.HasPrefix(lines[i], "  Test: divisible by ")) {
			test, err := strconv.Atoi(strings.TrimPrefix(lines[i], "  Test: divisible by "))
			if err != nil {
				log.Fatalf("unable to convert Test value: %v", err)
			}
			currentMonkey.Test = test
		}
		if (strings.HasPrefix(lines[i], "    If true: throw to monkey ")) {
			ifTrue, err := strconv.Atoi(strings.TrimPrefix(lines[i], "    If true: throw to monkey "))
			if err != nil {
				log.Fatalf("unable to convert IfTrue value: %v", err)
			}
			currentMonkey.IfTrue = ifTrue
		}
		if (strings.HasPrefix(lines[i], "    If false: throw to monkey ")) {
			ifFalse, err := strconv.Atoi(strings.TrimPrefix(lines[i], "    If false: throw to monkey "))
			if err != nil {
				log.Fatalf("unable to convert IfFalse value: %v", err)
			}
			currentMonkey.IfFalse = ifFalse
			monkeys = append(monkeys, currentMonkey)
		}
	}

	// part 1
	monkeys_part1 := make([]Monkey, len(monkeys))
	copy(monkeys_part1, monkeys)

	// play rounds for part 1
	for round := 0; round < 20; round++ {
		for m := 0; m < len(monkeys_part1); m++ {
			monkeys_part1[m].NbInspected += len(monkeys_part1[m].Items)
			for i := 0; i < len(monkeys_part1[m].Items); i++ {
				item := monkeys_part1[m].Items[i]

				// apply the operation
				operant1 := parseOperant(monkeys_part1[m].Operation[0], item)
				operant2 := parseOperant(monkeys_part1[m].Operation[2], item)
				switch op := monkeys_part1[m].Operation[1]; op {
				case "+":
					item = operant1 + operant2
				case "*":
					item = operant1 * operant2
				default:
					log.Fatalf("unsupported operation: %v", err)
				}

				// get bored
				item = item / 3

				// throws to a monkey
				if item % monkeys_part1[m].Test == 0 {
					monkeys_part1[monkeys_part1[m].IfTrue].Items = append(monkeys_part1[monkeys_part1[m].IfTrue].Items, item)
				} else {
					monkeys_part1[monkeys_part1[m].IfFalse].Items = append(monkeys_part1[monkeys_part1[m].IfFalse].Items, item)
				}
			}
			monkeys_part1[m].Items = make([]int, 0)
		}
	}

	sort.Slice(monkeys_part1, func(i, j int) bool {
		return monkeys_part1[i].NbInspected > monkeys_part1[j].NbInspected
	}) 

	fmt.Println(monkeys_part1[0].NbInspected * monkeys_part1[1].NbInspected)

	// part2
	monkeys_part2 := make([]Monkey, len(monkeys))
	copy(monkeys_part2, monkeys)

	// manage my anxiety level because monkeys are so stressful they are causing me an overflow of stress
	max_anxiety := 1
	for m := 0; m < len(monkeys_part2); m++ {
		max_anxiety *= monkeys_part2[m].Test
	}

	// play rounds for part 2
	for round := 0; round < 10000; round++ {
		for m := 0; m < len(monkeys_part2); m++ {
			monkeys_part2[m].NbInspected += len(monkeys_part2[m].Items)
			for i := 0; i < len(monkeys_part2[m].Items); i++ {
				item := monkeys_part2[m].Items[i]

				// apply the operation
				operant1 := parseOperant(monkeys_part2[m].Operation[0], item)
				operant2 := parseOperant(monkeys_part2[m].Operation[2], item)
				switch op := monkeys_part2[m].Operation[1]; op {
				case "+":
					item = operant1 + operant2
				case "*":
					item = operant1 * operant2
				default:
					log.Fatalf("unsupported operation: %v", err)
				}

				// anxiety management
				item = item % max_anxiety

				// throws to a monkey
				if item % monkeys_part2[m].Test == 0 {
					monkeys_part2[monkeys_part2[m].IfTrue].Items = append(monkeys_part2[monkeys_part2[m].IfTrue].Items, item)
				} else {
					monkeys_part2[monkeys_part2[m].IfFalse].Items = append(monkeys_part2[monkeys_part2[m].IfFalse].Items, item)
				}
			}
			monkeys_part2[m].Items = make([]int, 0)
		}
	}

	sort.Slice(monkeys_part2, func(i, j int) bool {
		return monkeys_part2[i].NbInspected > monkeys_part2[j].NbInspected
	}) 

	fmt.Println(monkeys_part2[0].NbInspected * monkeys_part2[1].NbInspected)
}