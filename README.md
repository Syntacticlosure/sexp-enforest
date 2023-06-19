# sexp-enforest
inspired by rhombus
What's the difference with tranditional racket extensible macros(match,require,provide ... expanders)?
1. use one set of friendly apis to implement all extensible macros
2. macro binding spaces to avoid name conflict
3. parsed structures to represent internal format and avoid further expansion
4. automatically capture the implicit forms