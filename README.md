# Green Snake

Two additional optimizations are implemented for a total of three, viz.,  Constant Folding, Constant Propagation and Dead Code Elimination.

## Constant Folding and Constant Propagation

These are implemented in `src/optimize.rs` as an AST -> AST transformation. Variables that are `set!` on are never optimized. The rest of the folding and propagation happens until saturation.

Overflow conditions are tracked and optimizations are undone, leaving it to the compiler to generate runtime errors.

## Dead Code Elimination

This happens in two parts. The `src/optimize.rs` does as much as possible on the AST, such as eliminating if-then-else branches as necessary. The original compiler already tracks type information and eliminates type checks as much as possible outside blocks.

## Notes on performance

This compiler implements all the extensions from all assignments, which means that there is additional code that the reference implementation might not generate, like the write barriers for nursery GC and uses a different stack layout with stack frames which means there will be two copies of arguments generated. This means that the baseline might not be an accurate reflection of the baseline of this version of the compiler.

## Tests that do very well

### Tail calls

Since this compiler also implements proper tail calls, we can execute recursions in tail positions in fixed space which will trigger a stack overflow on the reference compiler.
