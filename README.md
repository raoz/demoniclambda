# demoniclambda 0.1.0.0

Demoniclambda (dλ) is a research-level, currently extremely unstable lambda-calculus based programming language. The development is in currently in early stages. Do not consider using demoniclambda.

## Syntax
The syntax is highly inspired by lambda calculus. The basic building block is a term (t), which is either:

### A literal
Demoniclambda currently only has standard natural number literals and boolean values ⊥ and ⊤.

### A Variable
Demoniclambda as of now has immutable untyped variables. A variable can currently be anything in `[a-z]+`.

### An Abstraction
An abstraction is an anonymous function. It binds a variable x in the term t.

Form:

```
λx.t
```

### Application
Function application sets a bound variable to a definite value.

Form:
t t

### An Expression
Applications are composed of binary operators between terms.

Form:
t ★ t

Current supported operators are:
 | Operator | Semantics |
 | -------- | --------- |
 |    +     | Integer addition |
 |    -     | Integer substraction |
 |    /     | Interger division |
 |    ·     | Integer multiplication |
 |    ∧     | Logical and |
 |    ∨     | Logical or | 
 |-----------------------|

Operator prescedence is that of standard mathematics.

## Editor support

There is current ongoing work to support Atom and VSCode by plugins.

## Usage

Currently compiler does not have a convenient interface.

To use the compiler, one can type in valid terms and the compiler will output debug information and bytecode which results in evaluating that term and exiting with the value as exit code. The same bytecode will also be written in the file `a.ll`.

This can be compiled to binary with clang
```
clang a.ll -O3
```



## Design principles

There are currently two overarching design principles:
1. We like Unicode
2. We like lambda calculus

Otherwise, this is a testbed for compiler/programming language development. Radical things should be behind feature switches.

## Building
### Requirements
The most important requirement is LLVM-8, which the compiler is currently built on top of. This can generally be installed from binaries or can be compiled oneself. It is useful to enable assertions, in case the compiler produces invalid LLVM IR. LLVM should be compiled to be dynamically linked (currently we haven't figured out how to do this on Windows or how to work around this).

It should also be ensured that `libtinfo-dev` or similar is installed.

Other requirements should be fulfilled by stack.

### Building

```
stack setup
stack build
```
Then can run by
```
stack exec demoniclambda
```
Optionally:
```
stack install
```

## Contributing
Pull requests are in principle welcome but futile at this stage of development. It is recommended to discuss the changes beforehand by opening an issue.

## Licence

The compiler is currently available under the BSD 3 clause license, see [LICENSE](LICENSE).
