# Life-HS

My implementation of Conway's Game of Life in Haskell

## To Use

```
stack build
stack exec Life <program/path>
```

Two example programs are included in the `programs/` folder. You can also
append a custom paint delay (in microseconds) to the run command. The default
paint delay is 100000.

## How It Works

### Test Board

-------------------------  
| . | . | . | . | . | . |  
-------------------------  
| . | . | x | . | . | . |  
-------------------------  
| . | . | x | . | . | . |  
-------------------------  
| . | . | x | . | . | . |  
-------------------------  
| . | . | . | . | . | . |  
-------------------------  
| . | . | . | . | . | . |  
-------------------------  

### Step One

Insert the rightmost column to the left

-------------------------------------------  
| `.` |  .  |  .  |  .  |  .  |  .  |  .  |  
-------------------------------------------  
| `.` |  .  |  .  |  x  |  .  |  .  |  .  |  
-------------------------------------------  
| `.` |  .  |  .  |  x  |  .  |  .  |  .  |  
-------------------------------------------  
| `.` |  .  |  .  |  x  |  .  |  .  |  .  |  
-------------------------------------------  
| `.` |  .  |  .  |  .  |  .  |  .  |  .  |  
-------------------------------------------  
| `.` |  .  |  .  |  .  |  .  |  .  |  .  |  
-------------------------------------------  
   ^                                   |  
   |                                   |  
   -------------------------------------  


### Step Two

Insert the bottommost column at the top

-------------------------------------------  
| `.` | `.` | `.` | `.` | `.` | `.` | `.` | <-|  
-------------------------------------------   |  
| `.` |  .  |  .  |  .  |  .  |  .  |  .  |   |  
-------------------------------------------   |  
| `.` |  .  |  .  |  x  |  .  |  .  |  .  |   |  
-------------------------------------------   |  
| `.` |  .  |  .  |  x  |  .  |  .  |  .  |   |  
-------------------------------------------   |  
| `.` |  .  |  .  |  x  |  .  |  .  |  .  |   |  
-------------------------------------------   |  
| `.` |  .  |  .  |  .  |  .  |  .  |  .  |   |  
-------------------------------------------   |  
| `.` |  .  |  .  |  .  |  .  |  .  |  .  | --|  
-------------------------------------------  

### Step Three

For every row except the last, get chunks of two, and count the amount of 'x'
characters in the two characters.

For a row that looks like this:

-------------------------------------------  
| `.` |  .  |  .  |  x  |  .  |  .  |  .  |  
-------------------------------------------  

We obtain this new list:

---------------------------------  
| 0 | 0 | 1 | 1 | 0 | 0 | 0 | 0 |  
---------------------------------  

What we produce is actually the number of the 'x' characters occurring in the
cells above and to the top left of a cell.

We end up with a grid representing every cell's top and top-left neighbour count.

### Step Four

Rotate the original grid 90 degrees, and perform steps one to three on the
rotated grid. When you rotate the top-left neighbour grid back, you can add it
to the last rotations'. This will cover all of a cell's neighbours, even though
we've only coded a pattern for the top and top-left neighbours.

### Step Five

Now with the original grid and the grid representing the cells' neighbour
counts, apply the rules of the game to each cell to produce the next state.
