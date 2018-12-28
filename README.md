# What's this?

An attempt to solve the following problem: define a record, allow it to be read
from JSON, and in addition allow mere *subsets* of its fields to be read from
JSON.

# Goals

- Non-invasive solution. The original record should be a plain one, without
  wrapping the fields in functors or a type family or something like that.
- Solution should not require specifying multiple "base records" or
  specifying the type of a given field more than once.
- Allow extracting individual fields from the parsed record, instead of
  having to process all the fields at once.
- The user should not be obliged to handle the possibility of `Nothing`s
  that he knows won't be there.
- No TH

# Non-goals

- Avoiding
  [unsafeCoerce](http://hackage.haskell.org/package/base-4.12.0.0/docs/Unsafe-Coerce.html).
  Judicious uses of unsafeCoerce discreetly hidden in module internals should
  be ok.
- Optimizing compilation time.

# How does it work?

The fields of the `Subrec` are stored in a plain term-level map and their
different types are existencialized away. The constructor of the `Subrec` is
hidden, so this is an internal detail. Only fields present in the type-level
list have entries in the map.

When accessing a field of the `Subrec`, we require a `HasField` instance
(provided by generic-lens) on the original record to ensure that the required
field exists on the record, and also to know what its type should be (thanks to
the functional dependency). We also check that the field name is present in the
list of selected fields. These conditions allow us to call `unsafeCoerce`
internally to get the field's value without fear of blowing up.

# Any examples?

Check out the `Subrec.Examples` module.

# Related / potentially useful

- [How to deal with the "records problem" when writing REST API clients?](https://www.reddit.com/r/haskell/comments/a7asi8/how_to_deal_with_the_records_problem_when_writing/)
- [Rich Hickey - Maybe Not](https://www.youtube.com/watch?v=YR5WdGrpoug&feature=youtu.be&t=2355) The
- part at 23:55 about separating schema from selection.
- [exinst](http://hackage.haskell.org/package/exinst) could be useful
- [surgery for data types](https://blog.poisson.chat/posts/2018-11-26-type-surgery.html). [reddit](https://www.reddit.com/r/haskell/comments/a0gi4z/surgery_for_data_types/).
- [justified-containers](http://hackage.haskell.org/package/justified-containers)
- [ghosts of departed proofs](https://www.reddit.com/r/haskell/comments/8qn0wr/safe_api_design_with_ghosts_of_departed_proofs/)
- [fastsum](http://hackage.haskell.org/package/fastsum) Membership queries are constant-time, compiling to a single type-level natural lookup in a closed type family, 
- [type-map](http://hackage.haskell.org/package/type-map) Maps where keys are types and values can have types depending on their keys.
- [type-level-sets](http://hackage.haskell.org/package/type-level-sets)
- [type-level-bst](http://hackage.haskell.org/package/type-level-bst)
- [Parsing type-level strings in Haskell](https://kcsongor.github.io/symbol-parsing-haskell/)
- [generics-sop, interaction with OverloadedRecordFields](https://github.com/well-typed/generics-sop/issues/18). [Generic HasField](https://gist.github.com/adamgundry/2eea6ca04fd6e5b6e76ce9bfee454a6b). 


