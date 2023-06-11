# Important note:

This implementation fails `cbn` tests only because I did not pass them during Forest Flame either and could not fix it since the tests were not released. It also fails a couple of other tests rightly so, which run out of memory due to the implementation of Nursery Garbage collection. Please look at my 8th submission [on Gradescope](https://www.gradescope.com/courses/528266/assignments/2937288/submissions/183113349) for a primitive implementation that passes all tests, but only does basic optimizations.

# Green Snake

Two additional optimizations are implemented for a total of four, viz.,  Constant Folding, Constant Propagation, Dead Code Elimination and proper tail calls/TCO.

## Constant Folding and Constant Propagation

These are implemented in `src/optimize.rs` as an AST -> AST transformation. Variables that are `set!` on are never optimized. The rest of the folding and propagation happens until saturation.

Overflow conditions are tracked and optimizations are undone, leaving it to the compiler to generate runtime errors.

## Dead Code Elimination

This happens in two parts. The `src/optimize.rs` does as much as possible on the AST, such as eliminating if-then-else branches as necessary. The original compiler already tracks type information and eliminates type checks as much as possible outside blocks.

## Notes on performance

This compiler implements **all** the extensions from all assignments, which means that there is additional code that the reference implementation might not generate, like the write barriers for nursery GC and uses a different stack layout with stack frames which means there will be two copies of arguments generated. This means that the baseline might not be an accurate reflection of the baseline of this version of the compiler.

## Tests that do well

### Tail calls

Since this compiler also implements proper tail calls, we can execute recursions in tail positions in fixed space which will trigger a stack overflow on the reference compiler. This is showcased in `tests/gs_even_odd.snek`. Speedup is mostly due to eliminating call/ret.

### Constant complex expressions (Extreme Version)

A number crunching loop with constant expressions, making use of constant folding and propagation showcased in `tests/gs_complex_expr.snek`. The loop is just to make the benchmark more even, since each run would be instantaneous otherwise.

### Variable complex expressions

A number crunching loop that uses `input` in `tests/gs_simple_calc.snek`. This showcases dead code elimination for type checks since `x` is set as the result of multiplication.

## Performance Comparison

Relative performance data for the above-mentioned tests is shown in the table below.
|          **Tests**         | **Primary Optimization**                 | **Loops** | **Code generated reduction** | **Code executed reduction** | **Runtime Speedup** |
|:--------------------------:|------------------------------------------|-----------|:-----------------------:|:----------------------:|:-------------------:|
| tests/gs_even_odd.snek     | Proper Tail Calls                        |       1e2 |                    1.17 |                   2.01 |                 1.6 |
| tests/gs_complex_expr.snek | Constant Folding<br>Constant Propogation |       1e9 |                    3.04 |                   3.51 |                 2.8 |
| tests/gs_simple_calc.snek  | Dead Code Elimination                    |       1e9 |                    1.46 |                   1.42 |                1.95 |
