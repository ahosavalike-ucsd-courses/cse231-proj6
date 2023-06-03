# Egg Eater / Forest Flame

```
+-------------+->; For func2/After stack shift
|  live size  |  [rsp-48]/[rsp] ; Live data in stack
| func 1 args |  [rsp-48]/[rsp+8]
|      2      |  [rsp-40]/[rsp+16]
+-------------+
|      1      |  [rsp-32]/[rsp+24]
| func 2 temp |  [rsp-24]/[rsp+32]
|      3      |  [rsp-16]/[rsp+40]
|   /align/   |  [rsp-8]
+-------------+
|   ret ptr   |  [rsp] ; At call to func2
+-------------+->; For func1/After stack shift
|  live size  |  [rsp-72]/[rsp]
|      1      |  [rsp-64]/[rsp+8]
| func 2 args |  [rsp-56]/[rsp+16]
|      3      |  [rsp-48]/[rsp+24]
+-------------+
|      1      |  [rsp-40]/[rsp+32]
|      2      |  [rsp-32]/[rsp+40]
| func 3 temp |  [rsp-24]/[rsp+48]
|      4      |  [rsp-16]/[rsp+56]
|      5      |  [rsp-8]/[rsp+64]
+-------------+
|   ret ptr   |  [rsp] ; At call to func1
+-------------+->; For main/After stack shift
|  live size  |  [rsp-56]/[rsp]
|    input    |  [rsp-48]/[rsp+8]
|      1      |  [rsp-40]/[rsp+16]
| main 2 temp |  [rsp-32]/[rsp+24]
|      3      |  [rsp-24]/[rsp+32]
|   /align/   |  [rsp-16]/[rsp+40]
+-------------+
|     ---     |  [rsp-8]
|   ret ptr   |  [rsp] ; At call to main
+-------------+
```

## Heap layout

The first word tracks starting of free heap space. The second word denotes the end of the heap. The third word tracks the Rsp base.
