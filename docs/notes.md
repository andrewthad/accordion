# Notes

These are just some random thoughts.

So, SoA-style records are extremely important for performance. For
a number of algorithms, they provide asymptotically better performance.
This is because you can just pluck out the vectors upfront. Each one
you need costs `O(log n)` to pluck out. But then all accesses are `O(1)`.

The big question is how we want to integrate this into the types.
Roughly, the starting point is:

   data Record :: (Type -> Type) -> Set Field -> Type

This is a simple record backed by a balanced binary tree, polymorphic
in the wrapper for the elements. Using this one type, we can produce
several other interesting types:

* `Record Identity rs`: Standard record.
* `Record Maybe rs`: All fields are optional.
* `Record (Either ErrMsg) rs`: Validation that preserves all
  error messages.
* `Record (Vec n) rs`: SoA-style vector of records. Note that
  this is considerably more useful than `Record [] rs` or
  `Record Array rs` since it requires the length of each array
  to be the same.

The last example, `Record (Vec n) rs`, is tricky. We want to have
efficient random access, and we want unboxing. If `Vec` is the defined
inductively as:

    data Vec :: Nat -> Type -> Type where
      VecCons :: a -> Vec n a -> Vec (Succ n) a
      VecNil :: Vec Nil a

Then we end up with terrible performance when then arrays get bigger.
But first, let us contemplate some more complicated things that we
could express before revisiting the question of vectors. Let's make
`Record` handle some more sofisticated things:

    data Fields = Fields
      { fields :: Set Field
      , 
      }
    data Record :: Set Type -> 
