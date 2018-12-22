# What's this?

An attempt to solve the following problem: define a record, allow it to be read
from JSON, and in addition allow *subsets* of its fields to be read from JSON,
withour forcing. 

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

    - avoiding judicious uses of unsafeCoerce
    - optimizing compilation time

# Related

[How to deal with the "records problem" when writing REST API
clients?](https://www.reddit.com/r/haskell/comments/a7asi8/how_to_deal_with_the_records_problem_when_writing/)


[Rich Hickey - Maybe
Not](https://www.youtube.com/watch?v=YR5WdGrpoug&feature=youtu.be&t=2355) The
part about separating schema from selection.

