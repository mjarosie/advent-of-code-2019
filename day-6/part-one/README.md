[--- Day 6: Universal Orbit Map: Part One ---](https://adventofcode.com/2019/day/6)

```
You've landed at the Universal Orbit Map facility on Mercury.
the orbit maps here are useful for finding efficient routes between, for example, you and Santa.

Except for the universal Center of Mass (COM), every object in space is in orbit around exactly one other object.

                  \
                   \
                    |
                    |
AAA--> o            o <--BBB
                    |
                    |
                   /
                  /

In this diagram, the object BBB is in orbit around AAA.

Before you use your map data to plot a course, you need to make sure it wasn't corrupted during the download.
(like the one shown above) and indirect orbits.

Whenever A orbits B and B orbits C, then A indirectly orbits C.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L

Visually, the above map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I

In this visual representation, when two objects are connected by a line,
the one on the right directly orbits the one on the left.

Here, we can count the total number of orbits as follows:

    D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
    L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
    COM orbits nothing.

The total number of direct and indirect orbits in this example is 42.

What is the total number of direct and indirect orbits in your map data?
```

To build:

```bash
stack build
```

To get a result:

```bash
stack exec count-orbits <path to input file>
```