# What's this?

An attempt to solve the following problem: define a record, allow it to be read
from JSON, and in addition allow mere *subsets* of its fields to be read from
JSON.

# Goals

- non-invasive solution. The original record should be a plain one, without
  wrapping the fields in functors or a type family or something like that.
- solution should not require specifying multiple "base records" or
  specifying the type of a given field more than once.
- allow extracting individual fields from the parsed record, instead of
  having to process all the fields at once.
- the user should not be obliged to handle the possibility of `Nothing`s
  that he knows won't be there.
- no TH

# Non-goals

- avoiding unsafeCoerce. Judicious uses of unsafeCoerce discreetly hidden in
  module internals should be ok.
- optimizing compilation time

# Related

[How to deal with the "records problem" when writing REST API
clients?](https://www.reddit.com/r/haskell/comments/a7asi8/how_to_deal_with_the_records_problem_when_writing/)

[Rich Hickey - Maybe
Not](https://www.youtube.com/watch?v=YR5WdGrpoug&feature=youtu.be&t=2355) The
part about separating schema from selection.

[exinst](http://hackage.haskell.org/package/exinst) could be useful

[surgery for data types](https://blog.poisson.chat/posts/2018-11-26-type-surgery.html). [reddit](https://www.reddit.com/r/haskell/comments/a0gi4z/surgery_for_data_types/).

[justified-containers](http://hackage.haskell.org/package/justified-containers)

[ghosts of departed proofs](https://www.reddit.com/r/haskell/comments/8qn0wr/safe_api_design_with_ghosts_of_departed_proofs/)

