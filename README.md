ocaml-tailrec
=============

A .cmt parser to assert whether functions are tail-recursive

## Usage
```
make
./tailrec <module.cmt>
```

### Example output
There are a couple of tests provided:

Consider the following module `fail.ml`:
```ocaml
let rec length_nottailrec = function [] -> 0 | x::xs -> 1 + length_nottailrec xs
[@@tailrec]
```

Compiling this and running this utility on it produces the following error (piping stdout to /dev/null to avoid debug):

```
$ ocamlc -bin-annot fail.ml
$ ./tailrec fail.cmt > /dev/null
File "fail.ml", line 1, characters 60-80:
Error: this call to length_nottailrec is not a tail-call!
```

A correctly annotated function produces no such output. Consider this module, `pass.ml`:
```ocaml
let rec length_tailrec acc = function [] -> acc | x::xs -> length_tailrec (acc+1) xs
[@@tailrec]
```
```
$ ocamlc -bin-annot pass.ml
$ ./tailrec pass.cmt > /dev/null
```

### Implementation
Currenltly there is debugging output to `stdout` which gives a hint to what's going on under the hood:
```
$ ./tailrec pass.cmt
Found implementation annotation in cmt.
  Found value length_tailrec at [location] marked as tail-recursive...
  Compiling length_tailrec_1008 into Lletrec lambda term...
  Checking all calls to length_tailrec_1008 in lambda body are tail calls...
    Generating annotations for lambda term...
    Checking if the lambda term Lapply(length_tailrec_1008, _, _) is a tail call...OK!
```

Basically it checks that all applications of the function that is annotated as tail-recursives are tail-calls.

### Considerations
The use of ppx extensions proved difficult since they only give you an opportunity to provide a mapping from an untyped AST to another untyped AST. I couldn't find enough information to check all the callsites of a particular expression since they had not yet been uniquely tagged. If there was a chance to operate on the `TypedTree` values, this might have been possible.

The current compiler takes the code through these first three stages:

```
Parse-tree ---typing---> TypedTree ---transl---> Lambda
```

The annotations are part of the parse tree and the TypedTree but are thrown away during the translation to the Lambda expressions. However, the tail-call analysis is done on the Lambda types (in `Simplif.emit_tail_infos`). So one option would be to thread through the annotations to this layer. This looked like it would be a large interface change so I decided against it.





