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

The constructor of `Subrec` is hidden, the only way of obtaining a value is the
`FromJSON` instance. Only fields present in the type-level list of selected
fields will be parsed.

The fields of the `Subrec` are stored in a plain term-level map and their
different types are existencialized away.

When accessing a field of the `Subrec`, we require a `HasField` instance
(provided by generic-lens) on the original record to ensure that the required
field exists on the record, and also to know what its type should be (thanks to
the functional dependency). We also check that the field name is present in the
list of selected fields. These conditions allow us to call `unsafeCoerce`
internally to get the field's value without fear of blowing up.

# Related

[How to deal with the "records problem" when writing REST API
clients?](https://www.reddit.com/r/haskell/comments/a7asi8/how_to_deal_with_the_records_problem_when_writing/)

[Rich Hickey - Maybe
Not](https://www.youtube.com/watch?v=YR5WdGrpoug&feature=youtu.be&t=2355) The
part at 23:55 about separating schema from selection.

[exinst](http://hackage.haskell.org/package/exinst) could be useful

[surgery for data types](https://blog.poisson.chat/posts/2018-11-26-type-surgery.html). [reddit](https://www.reddit.com/r/haskell/comments/a0gi4z/surgery_for_data_types/).

[justified-containers](http://hackage.haskell.org/package/justified-containers)

[ghosts of departed proofs](https://www.reddit.com/r/haskell/comments/8qn0wr/safe_api_design_with_ghosts_of_departed_proofs/)

