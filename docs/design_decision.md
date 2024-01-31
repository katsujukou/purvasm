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

## How toplevel phrases will be represented?

Immediate values are directly stored in data section

```purs
ultimateAnswer :: Int
ultimateAnswer = 42
```

would be compiled into the object code file like this:

```
;;==================
;; DATA SECTION
;;==================
;; ultimateAnswer
quote 42;
```

Function value will be compiled into two part:
one for entry point and other for code block which would be referred by some closures.
The former will located in the data section, and the later in text section.

```purs
add :: Int -> Int -> Int
add x y = x + y

-- partially applied function
inc :: Int -> Int
inc = add 1
```

```
;;==================
;; TEXT SECTION
;;==================
;; add
;;------------------
@0: start_fun;
    grab;
    access 0;
    push;
    access 1;
    p_add_i32;
    return;
;;==================
;; DATA SECTION
;;==================
;; add
;;------------------
    closure (Label 0);
    set_global "Sample.add";
    stop;
;;------------------
;; inc
;;------------------
    pushmark;
    quote 1;
    push;
    get_global "Sample.add";
    apply;
    set_global "Sample.inc";
    stop:
;;==================
;; entry point
;;------------------
    pushmark;
    quote 42:
    push;
    get_global "Sample.inc";
    apply;
    stop;
;;==================
```
