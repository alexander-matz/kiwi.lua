# Kiwi.lua

This is a lua port of the [kiwi](https://github.com/nucleic/kiwi) project, a cleaner
and faster implementation of the original cassowary project.
It allows to solve a system of equality and inequality constraints and find valid
values for the variables that have been used in these constraints.

"
Cassowary and other hierarchial constraint toolkits add a unique mechanism for deciding between sets of rules that might conflict in determining which of a set of possible solutions are "better".
By allowing constraint authors to specify weights for the constraints, the toolkit can decide in terms of stronger constraints over weaker ones, allowing for more optimal solutions.
These sorts of situations arise all the time in UI programming; e.g.: "I'd like this to be it's natural width, but only if that's smaller than 600px, and never let it get smaller than 200px".
Constraint solvers offer a way out of the primordial mess of nasty conditionals and brittle invalidations.
"

Kiwi.lua is a pretty straightforward port from the original C++ code which mapped surprisingly well to lua 5.1.
It comes without any dependencies and is contained in a single file.
Where appropriate, the code was translated to more idiomatic lua code (e.g. handling of shared data and underscore as word seperator instead of camelCase).
Some of the nice functionality from the C++ project is not available, most notably the overloaded operators
which allowed to embed the constraints directly in C++.
For this purpose, there is a basic frontend available that parses a string into a constraint.

## Status

The code seems to be working fine, but I did write any unit tests, so it is entirely possible that there
is a bug somewhere in there.
If you found any, let me know!

## Usage

Getting started is rather easy. Just drop the file into a directory in your lua path and require it.
Here is a code example:

```lua
local kiwi = require("kiwi")

local solver = kiwi.solver()
local x1 = kiwi.variable('x1')
local x2 = kiwi.variable('x2')

local vars = {x1 = x1, x2 = x2}

solver:add_constraint(kiwi.parse_constraint(vars, 'x1 >= 0'))
solver:add_constraint(kiwi.parse_constraint(vars, 'x2 >= 0'))
solver:add_constraint(kiwi.parse_constraint(vars, 'x1 == 3 x2'))

solver:add_edit_variable(x1, kiwi.strength.medium)
solver:suggest_value(x1, 10)

solver:update_variables()

print(x1.value, x2.value)
```

The general idea is always the same:
1. Setup
    - create a solver object
    - create variables
    - create constraints
    - add constraints
    - add edit variables
2. Rest of the runtime
    - Suggest values for your variables
    - Update variables
    - Get variable values with variable.value

## API

In the spirit of the original Kiwi project, the API is very lightweight.
Intermediate objects (expressions, terms, ...) are exported, but not necessary
in order to make full use of the library.
This documentation only covers the required parts.
If you feel like directly using the internal objects, take a look at the source
code and use the original project (link at the top) as reference.

### kiwi.variable

```lua
kiwi.variable(name) -- creates new variable instance with corresponding name

variable.value -- holds the value of the variable, writes don't influence the
  -- solver but are not recommended anyway
```

### kiwi.parse\_constraint

```lua
kiwi.parse_constraint(vars, string) -- parses a string into a constraint
 -- vars: map of {variablename = variable}
 --   ONLY variables in this map can be used in the constraint
 -- string: string containing the constraint, it follows the following format
 --   Each term can be either: a constant, a variable, or
 --     a constant followed by a variable (no multiplication sign)
 --   Terms are combined with either + or -
 --   The relationship between left and right side can be ==, <=, or >=
 --   The constraint can be followed by one of these:
 --     #required, #string, #medium, #weak (none defaults to required)
 --   Every is seperated via WHITESPACE
 -- examples:
 --   1 + x1 >= 2 x3 - x5 #strong
 --   x >= 0 - x1 - 3 x4
 -- returns a constraint that can be added to a solver
```

### kiwi.solver

```lua
kiwi.solver() -- creates new solver instance

solver:add_constraint(cn) -- adds constraint to solver
solver:remove_constraint(cn) -- removes previously added constraint
solver:add_edit_variable(variable, strength) -- adds an editable variable
 -- the strength determines the priority of suggested values
solver:remove_edit_variable(variable) -- removes previously added editable var
solver:has_edit_variable(variable) -- check if variable is editable
solver:suggest_value(variable, value) -- suggest a value for editable variable
solver:update_variables() -- write the .value field for all variables used in any
 -- constraint. Solutions are NOT visible before this operation.
solver:reset() -- delete all constraints and edit variables, object should be the
 -- same as it was right after creation
```

## License

This project is licensed under zlib license.

## Links

- Original Cassowary C++ implementation: [](https://constraints.cs.washington.edu/cassowary/)
- Cassowary.lua: [](https://github.com/simoncozens/cassowary.lua)
- Kiwi C++: [](https://github.com/simoncozens/cassowary.lua)
