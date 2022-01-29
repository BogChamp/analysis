module type EQ = {
  type t;
  let (==): (t, t) => bool;
};

module type PARTIAL_ORDER = {
  type t;
  let (<=): (t, t) => bool;
};

module type JOIN_SEMILATTICE = {
  type t;
  let join: (t, t) => t;
};

module type MEET_SEMILATTICE = {
  type t;
  let meet: (t, t) => t;
};

module type BOUNDED_JOIN_SEMILATTICE = {
  include JOIN_SEMILATTICE;
  let bottom: t;
};

module type BOUNDED_MEET_SEMILATTICE = {
  include MEET_SEMILATTICE;
  let top: t;
};

module type LATTICE = {
  type t;
  include JOIN_SEMILATTICE with type t := t;
  include MEET_SEMILATTICE with type t := t;
};

module type BOUNDED_LATTICE = {
  type t;
  include BOUNDED_JOIN_SEMILATTICE with type t := t;
  include BOUNDED_MEET_SEMILATTICE with type t := t;
};

module type EQ_JOIN_SEMILATTICE = {
  type t;
  include EQ with type t := t;
  include JOIN_SEMILATTICE with type t := t;
};

module type EQ_BOUNDED_LATTICE = {
  type t;
  include EQ with type t := t;
  include BOUNDED_LATTICE with type t := t;
};

module LevitatedLattice =
       (L: LATTICE)
       : {
         type t;
         include BOUNDED_LATTICE with type t := t;
         let lift: L.t => t;
       } => {
  type t =
    | Top
    | Levitate(L.t)
    | Bottom;

  let top = Top;
  let lift = x => Levitate(x);
  let bottom = Bottom;

  let meet = (x, y) =>
    switch (x, y) {
    | (Top, _)
    | (_, Top) => Top
    | (x, Bottom)
    | (Bottom, x) => x
    | (Levitate(x), Levitate(y)) => Levitate(L.meet(x, y))
    };

  let join = (x, y) =>
    switch (x, y) {
    | (Bottom, _)
    | (_, Bottom) => Bottom
    | (x, Top)
    | (Top, x) => x
    | (Levitate(x), Levitate(y)) => Levitate(L.join(x, y))
    };
};

module Constant = (T: EQ): {
  type t =
    | Any
    | Some(T.t)
    | Non;
  include EQ_BOUNDED_LATTICE with type t := t;

  let of_option: option(T.t) => t;
} => {
  type t =
    | Any
    | Some(T.t)
    | Non;

  let top = Any;

  let bottom = Non;

  let join = (x, y) =>
    switch (x, y) {
    | (Any, _)
    | (_, Any) => Any
    | (x, Non)
    | (Non, x) => x
    | _ => Any
    };

  let meet = (x, y) =>
    switch (x, y) {
    | (Non, _)
    | (_, Non) => Non
    | (x, Any)
    | (Any, x) => x
    | _ => Non
    };
  
  let (==) = (x, y) =>
  switch (x, y) {
  | (Any, Any)
  | (Non, Non) => true
  | (Some(x), Some(y)) => x == y
  | _ => false
  };
  
  let of_option = x =>
    switch (x) {
    | None => Non
    | Some(v) => Some(v)
    };
};

module EQ_CONST: EQ = {
  type t = Runtime.Value.t;

  let (==) = (==);
};



