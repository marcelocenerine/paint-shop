Problem Description *(details omitted)*
---------------------------------------
Given a list of orders and a set of available paints (colors x sheens), find the optimal mix so that:
1. all the customers are satisfied (hard-constraint)
2. has the lowest cost (soft-constraint)


Solution
--------
This project provides 2 strategies to tackle the problem: brute force and local search (Tabu Search).
These strategies can be chosen via command line argument (details below).

Before even entering the search space exploration phase, the `Mixer` attempts to reduce the search space by eliminating
values (paints) that are fixed (customer selection with single paint). This reduction is performed recursively so that
other multi-value selections can become singleton during the reduction process. Unfeasible combinations derived from
singleton selections are also identified at this stage, which prevents pointless execution of the subsequent phase.  

#### Brute Force
Explores the search space via exhaustive search and guarantees to find the optimal solution (if there is any) as long
as potential long running times are acceptable. Once a feasible solution is found, it turns itself into a 
[backtracking](https://en.wikipedia.org/wiki/Backtracking) algorithm to avoid further exploration of more expensive 
partial solutions that can't improve the best solution found so far. This optimization can greatly reduce the remaining
search space and, consequently, the running time.
The time complexity of this algorithm is `O(n^m)` in the worst case scenario, where `n` = sheens (2) and `m` = colors. 
This strategy is indicated for relatively small data sets (up to 20 colors). 

#### Tabu Search
Explores the search space using a simplified implementation of the [Tabu Search](https://en.wikipedia.org/wiki/Tabu_search)
meta-heuristic. This strategy is suitable for large inputs where the search space is too big to be explored via exhaustive
search. Given that the algorithm may not visit all possible combinations in the search space, **it does not guarantee to find 
an optimal nor a feasible solution**. The more time is given to the algorithm, the better the solution will likely be.
Moreover, it has to be given a condition to stop the search. Currently it only supports a time-based condition but others
(e.g. max iterations, max iterations without improvement, etc) can be easily introduced. 

### Usage
#### Requirements
- Scala 2.12
- SBT

On `sbt`:

* Brute force strategy
```
run <path to file>
```

* Tabu search strategy
```
run <path to file> --tabu-search [<timeout in secs>]
```
**timeout defaults to 5 seconds if not provided*

>*Note:* There are some sample input files available in the classpath under `/samples/` that can be used for testing.
The `40x10000_feasible.txt` file in particular can be used to see the difference in running time between both algorithms.
 
