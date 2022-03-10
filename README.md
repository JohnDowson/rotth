# Rotth
### A stack-cringe language inspired by [Porth](https://gitlab.com/tsoding/porth)

## Syntax
```rotth
include "std.rh"

proc main : u64 do
    "Hello, world!\n" puts
    0
end
```

Rotth has following keywords:
- `include`
- `if`
- `else`
- `proc`
- `while`
- `do`
- `bind`
- `const`
- `end`
- `return`
- `cond`

### `proc`
Keyword `proc` declares a procedure. It is followed by procedure name, then it's inputs and outputs separated by the `:` signature separator.
Body of the procedure is terminated by `end` keyword.
### `if` and `else`
`if` keyword is a primary conditional construct of the language. It must be preceded by an expression of type `bool` and followed by true branch, then by optional `else` branch and finally by `end` terminator.
### `while do`
`while` is the looping construct. It is followed by loop condition, then `do` keyword, then loop body, then `end`.
### `const`
`const` followed by name and type, separated by `:`, declares a compile-time constant. It supports limited compile-time evaluation, syscalls and user-defined proc calls are not allowed.
### `bind`
`bind` is similliar to destructuring in traditional functional languages, it iakes elements from the stack and allows using them as local constants. For example, this is how you can implement `Forth` `rot` word using it:
```rotth
bind a : u64 b : ptr c : char do
    b c a
end
```
### `cond`
Despite it's name `cond` is more similliar to `Rust`'s `match` than to `Lisp`'s `cond`, taking only constants and literal values as patterns to compare against.
```rotth
some-char cond
    'a' do
        "hello\n"
    else
    'b' do
        "bye\n"
    else
    'c' do
        "get out\n"
  end
```