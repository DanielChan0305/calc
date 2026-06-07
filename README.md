# calc

A terminal-based interactive calculator written in **Scala 3**, inspired by [eva](https://github.com/oppiliappan/eva) by oppiliappan — a beautifully simple calculator that motivated this project.

## Quick start

```bash
sbt compile
sbt run
```

```
> 1 + 2 * 3
7.0
> sin(pi / 2) + log10(100)
3.0
> exit
Thanks for using calc. Have a nice day.
```

## Operators

| Operator | Description | Associativity |
|----------|-------------|---------------|
| `+` | Addition / unary plus | Left |
| `-` | Subtraction / unary minus | Left |
| `*` | Multiplication | Left |
| `/` | Division | Left |
| `^` | Exponentiation | **Right** |
| `=` | Variable assignment | **Right** |

### Precedence (highest to lowest)

1. `^` (right‑associative: `2^3^2` → `2^(3^2)`)
2. Unary `+`, `-`
3. Implicit multiplication
4. `*`, `/`
5. `+`, `-`

## Constants

| Name | Value |
|------|-------|
| `pi` | π ≈ 3.14159… |
| `e` | Euler's number ≈ 2.71828… |

## Mathematical functions

### Trigonometric

| Function | Definition |
|----------|------------|
| `sin(x)` | sine |
| `cos(x)` | cosine |
| `tan(x)` | tangent |
| `csc(x)` | cosecant = 1 / sin(x) |
| `sec(x)` | secant = 1 / cos(x) |
| `cot(x)` | cotangent = cos(x) / sin(x) |

### Hyperbolic

| Function | Definition |
|----------|------------|
| `sinh(x)` | hyperbolic sine |
| `cosh(x)` | hyperbolic cosine |
| `tanh(x)` | hyperbolic tangent |

### Inverse trigonometric

| Function | Definition |
|----------|------------|
| `asin(x)` | arc sine |
| `acos(x)` | arc cosine |
| `atan(x)` | arc tangent |
| `acsc(x)` | arc cosecant = asin(1/x) |
| `asec(x)` | arc secant = acos(1/x) |
| `acot(x)` | arc cotangent = atan(1/x) |

### Logarithms & powers

| Function | Definition |
|----------|------------|
| `ln(x)` | natural logarithm |
| `log2(x)` | base‑2 logarithm |
| `log10(x)` | base‑10 logarithm |
| `sqrt(x)` | square root |

### Rounding & absolute

| Function | Definition |
|----------|------------|
| `ceil(x)` | ceiling (round up) |
| `floor(x)` | floor (round down) |
| `abs(x)` | absolute value |

## Features

### Auto‑balancing parentheses

Unmatched opening parentheses are automatically closed for you:

```
> (1 + 2) * 3     →  9.0         ← well‑formed, no change
> (1 + 2 * 3      →  7.0         ← missing `)` appended
> ((2 + 3) * (4 + 1  →  25.0     ← two missing `)` appended
```

Invalid bracket sequences still produce an error:

```
> 1 + 2)          →  Error: invalid bracket sequence
```

### Variable assignment

Store values in variables and reuse them:

```
> x = 5
5.0
> x + 3
8.0
> y = x * 2
10.0
> x ^ y
9765625.0
```

Assignments can use full expressions:

```
> radius = 2.5
2.5
> pi * radius ^ 2
19.634954…
```

Built‑in function names and constants are protected:

```
> sin = 5         →  Error: invalid use of function 'sin'
```

Variables persist within a session but reset on restart.

### Implicit multiplication

You can omit `*` between:

- A number and a parenthesised expression: `2(3 + 1)` → `8.0`
- Two parenthesised expressions: `(2)(3)` → `6.0`
- A number/constant and a variable: `2x`, `2 pi`
- A number/constant and a function: `2 sin(pi / 2)` → `2.0`

```
> 2(3)
6.0
> (2 + 1)(3 + 4)
21.0
> 2 pi e
17.07946…
```

### Terminal experience (JLine)

The REPL uses [JLine](https://github.com/jline/jline3) for a proper terminal feel:

| Action | Key |
|--------|-----|
| Previous input | **↑** |
| Next input | **↓** |
| Tab completion | **Tab** |
| Move cursor | **← →** |
| Beginning / end of line | **Home / End** |

Tab completion covers all 26 built‑in function and constant names. Start typing and press **Tab** — for example, `si` → `sin`, `lo` → cycles `log2` → `log10`.

## Running tests

```bash
sbt test
```

68 tests covering arithmetic, precedence, all functions, implicit multiplication, variable assignment, auto‑balancing, and error cases.

## Requirements

- **JDK 21+**
- **sbt** (the build tool — `sbt run` downloads Scala and all dependencies automatically)

## License

MIT