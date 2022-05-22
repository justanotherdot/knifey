# Design Notes

## Features

### Rich value from evaluation

It would be nice to know the results of dice rolls as well as the final
resulting value. That way we could know that the, e.g., first, second, and third
roll such and such a value.

Something along the lines of

```
pub struct Value {
  result: i64,
  rolls: Vec<i64>,
}
```

### Pretty printer

A pretty printer to take a generated AST that produces a final variant would be
helpful for debugging state.

### Parentheses

Support for parenthesis.

## Testing

Fuzzing against the grammar would be good.
