## data-cmd
Parser and renderer for any(generic) type. Suitable for command line, textual represenation of the type

### Development

Enter nix shell with hls using
```
nix-shell --arg withHLS true
```

### Utility



### Usage

### Examples

### How it works

```mermaid
graph LR
A[Raw text] e1@-- Lexer --> B[Tree]
B -- Delexer --> A

B -- Former --> C[Form]
C -- Deformer --> B

C -- Parser --> D[Type]
D -- Deparser --> C
```
