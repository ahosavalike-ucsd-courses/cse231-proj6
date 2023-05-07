# Diamondback

## Calling Convention
Move stack up by the depth of the function body + number of arguments to the function at the start of each. Arguments are at the very bottom. For the main function, the input is at the bottom.


```
+-------------+  ; For func/after stack shift
|      1      |  [rsp-64]/[rsp]
|      2      |  [rsp-56]/[rsp+8]
| func 3 temp |  [rsp-48]/[rsp+16]
|      4      |  [rsp-40]/[rsp+24]
|      5      |  [rsp-32]/[rsp+32]
+-------------+
|      3      |  [rsp-24]/[rsp+40]
| func 2 args |  [rsp-16]/[rsp+48]
|      1      |  [rsp-8]/[rsp+56]
+-------------+
|   ret ptr   |  [rsp] ; At call to func
+-------------+  ; For main/After stack shift
|      1      |  [rsp-48]/[rsp]
| main 2 temp |  [rsp-40]/[rsp+8]
|      3      |  [rsp-32]/[rsp+16]
+-------------+
|    input    |  [rsp-16]/[rsp+32]
|     ---     |  [rsp-8]
|   ret ptr   |  [rsp] ; At call to main
+-------------+
```
