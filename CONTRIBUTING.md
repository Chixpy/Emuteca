## Code formating

### Beautifier

Use internal JEDI Code Formater in Lazarus (`Ctrl+D`), with next settings:

  - Width length = 80
  - Tabs to spaces = 2

### For / While / Repeat

`Repeat` esentially is different from `For` and `While`, if it's required at least one iterarion you can use it.

Between `For` and `While`... maybe `While` in most cases:

  - `For` is better when we need to skip values inside a fixed length iteration with `Continue`, making less anidated conditionals and clearer code.
  - `While` is better when whole iteration must be aborted in the middle and it's a must when iteration length is variable. For example, removing items from a list (the counter value must not be incremented, unless it goes backwards...).

### String Constants

Ideally, all string constants must be cleared as constants in `const` section (or as resource string in `resourcestring` section if translatable).

This include `'' = EmptyStr`


### .ToXXXX helpers

As FPC 3.0 (and 2.6 for records and classes), FPC has [Helper_types](https://wiki.freepascal.org/Helper_types. It's clearer in code to use them.

```fpc
var
  a: integer
  s: string
begin
  a := 2;
  
  s := IntToStr(a); // Classic
  
  s := a.ToString;  // Helper Type
end;
```

In Classic method; if `a` is changed to `real`, `IntToStr` must be change too.