# Egg Eater

## Calling Convention
Move stack up by the depth of the function body + number of arguments to the function at the start of each call. The stack is aligned to 16 bytes along with the move. Arguments are at the very top. For the main function, the input is at the top.


```
+-------------+->; For func2/After stack shift
| func 1 args |  [rsp-48]/[rsp]
|      2      |  [rsp-40]/[rsp+8]
+-------------+
|      1      |  [rsp-32]/[rsp+16]
| func 2 temp |  [rsp-24]/[rsp+24]
|      3      |  [rsp-16]/[rsp+32]
|   /align/   |  [rsp-8]
+-------------+
|   ret ptr   |  [rsp] ; At call to func2
+-------------+->; For func1/After stack shift
|      1      |  [rsp-64]/[rsp]
| func 2 args |  [rsp-56]/[rsp+8]
|      3      |  [rsp-48]/[rsp+16]
+-------------+
|      1      |  [rsp-40]/[rsp+24]
|      2      |  [rsp-32]/[rsp+32]
| func 3 temp |  [rsp-24]/[rsp+40]
|      4      |  [rsp-16]/[rsp+48]
|      5      |  [rsp-8]/[rsp+56]
+-------------+
|   ret ptr   |  [rsp] ; At call to func1
+-------------+->; For main/After stack shift
|    input    |  [rsp-48]/[rsp]
|      1      |  [rsp-40]/[rsp+8]
| main 2 temp |  [rsp-32]/[rsp+16]
|      3      |  [rsp-24]/[rsp+24]
|   /align/   |  [rsp-16]/[rsp+32]
+-------------+
|     ---     |  [rsp-8]
|   ret ptr   |  [rsp] ; At call to main
+-------------+
```
