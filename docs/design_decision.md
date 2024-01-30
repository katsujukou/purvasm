# The structure of each object code file

```
+---------------------------+
| header section            |
+===========================+
| text section              |
+---------------------------+
| constant data section     |
+---------------------------+
| global reference section  |
+---------------------------+
| Symbol lookup tbl section |
+---------------------------+
```

```purs
f :: Int -> Int
f = (_ + 1)

g :: Int
g = f 42
```

This single module will compiled into:

```
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Section
;;;;;;;;;;;;;;;;;;;;;;;;;;
; code block for function `f`
@f_0: start_fun;
      grab;
      quote 1;
      push;
      access 0;
      add_i32;
      return;

; code block for function `g`
@g_0: pushmark;
      quote 42;
      push;
      call_global "Sample.f";
```
