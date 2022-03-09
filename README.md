# Rotth
### A stack-cringe language inspired by [Porth](https://gitlab.com/tsoding/porth)

## Syntax
```rotth
proc main : uint do
    "Hello, world!\n" puts
    0
end

const SYS_write : uint do 1 end
const STDOUT : uint do 1 end

proc puts uint ptr do
    STDOUT SYS_write syscall3 drop
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
`bind` is similliar to destructuring in traditional functional languages, it iakes elements from the stack and allows using them as local constants. For example this is how you can implement `Forth` `rot` word using it:
```rotth
bind a b c do
    b c a
end
```