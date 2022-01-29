open Base;

module Vertex = {
    type statement = 
    | Assignment(Syntax.Identifier.t)
    | If 
    | While 
    | Print 

    type t =
    | Begin 
    | Node({statement: statement, expr: Syntax.Expression.t, id: int})
    | AfterIf(int)
    | End
    
    let hash = (vertix: t): int => {
      switch vertix {
      | Node(x) => x.id
      | End => -1
      | Begin => -2
      | AfterIf(x) => x
      };
    }
    let compare = (x: t, y: t): int => {
        compare(hash(x), hash(y));
    }

    let equal = (x: t, y: t): bool => {
        switch (x, y) {
        | (AfterIf(x), AfterIf(y)) => x == y
        | (Node(x), Node(y)) => x.id == y.id
        | (End, End) => true
        | (Begin, Begin) => true
        | _ => false
        };
    }
};

module Edge = {
    type t =
    | True
    | False
    | Simple
    
    let default = Simple;

    let hash = (x: t) => {
      switch x {
      | True => 1
      | False => -1
      | Simple => 0
      };
    }
    let compare = (x: t, y: t): int => {
        compare(hash(x), hash(y));
    }
};

module G = Graph.Persistent.Digraph.ConcreteLabeled(Vertex)(Edge)

let graph = G.empty;

module Data = {
  type t = {g: G.t, id: int, last: Vertex.t, edge: Edge.t};
}

let rec make_CFG = (program: Syntax.Program.t, ~default: Data.t): Data.t => {
  List.fold(program, ~init=default, ~f=(init: Data.t, statement: Syntax.Statement.t) => {
    switch (statement) {
      | Syntax.Statement.AssignmentStatement({identifier, value}) => {
        let vertex = Vertex.Node({statement: Vertex.Assignment(identifier), expr: value, id: init.id});
        let cfg = G.add_vertex(init.g, vertex);
        let edge = G.E.create(init.last, init.edge, vertex);
        let cfg = G.add_edge_e(cfg, edge);
        {g: cfg, id: init.id + 1, last: vertex, edge: Edge.Simple};
      }
      | Syntax.Statement.PrintStatement(expression) => {
        let vertex = Vertex.Node({statement: Vertex.Print, expr: expression, id: init.id});
        let cfg = G.add_vertex(init.g, vertex);
        let edge = G.E.create(init.last, init.edge, vertex);
        let cfg = G.add_edge_e(cfg, edge);
        {g: cfg, id: init.id + 1, last: vertex, edge: Edge.Simple};
      }
      | Syntax.Statement.If({statement, actions, elseactions}) => {
        let vertex = Vertex.Node({statement: Vertex.If, expr: statement, id: init.id});
        let cfg = G.add_vertex(init.g, vertex);
        let edge = G.E.create(init.last, Edge.True, vertex);
        let cfg = G.add_edge_e(cfg, edge);
        let tmp = make_CFG(actions,~default={g: cfg, id: init.id + 1, last: vertex, edge: Edge.True});
        let tmp2 = switch (elseactions) {
          | Some(x) => {
            make_CFG(x,~default={g: tmp.g, id: tmp.id, last: vertex, edge: Edge.False});
          }
          | None => {g: tmp.g, id: tmp.id , last: vertex, edge: Edge.False};
        };
        let vertex = Vertex.AfterIf(tmp2.id);
        let cfg = G.add_vertex(tmp2.g, vertex);
        let edge = G.E.create(tmp.last, tmp.edge , vertex);
        let cfg = G.add_edge_e(cfg, edge);
        let edge = G.E.create(tmp2.last, tmp2.edge, vertex);
        let cfg = G.add_edge_e(cfg, edge);
        {g: cfg, id: tmp2.id + 1, last: vertex, edge: Edge.Simple};
      }
      | Syntax.Statement.WhileStatement({statement, actions}) => {
        let vertex = Vertex.Node({statement: Vertex.While, expr: statement, id: init.id});
        let cfg = G.add_vertex(init.g, vertex);
        let edge = G.E.create(init.last, init.edge, vertex);
        let cfg = G.add_edge_e(cfg, edge);
        let tmp = make_CFG(actions,~default={g: cfg, id: init.id + 1, last: vertex, edge: Edge.True});
        let edge2 = G.E.create(tmp.last, Edge.Simple, vertex);
        let cfg = G.add_edge_e(tmp.g, edge2);
        {g: cfg, id: tmp.id, last: vertex, edge: Edge.False};
      }
    };
  });
};

module GraphPrinter = {
	include G;
	open Graph
	let get_subgraph = (_: G.V.t): option(Graphviz.DotAttributes.subgraph) => None;
 
	let graph_attributes = (_: G.t): list(Graphviz.DotAttributes.graph) => [];
 
	let default_vertex_attributes = (_: G.t): list(Graphviz.DotAttributes.vertex) => [];
	let vertex_attributes = (vertex: G.V.t): list(Graphviz.DotAttributes.vertex) => {
		switch(vertex) {
			| Node({statement: _, expr: expression, id: _}) => [`Label((Syntax.Expression.sexp_of_t %> Sexp.to_string_hum)(expression))]
			| _ => []
		};
	};
 
	let default_edge_attributes = (_: G.t): list(Graphviz.DotAttributes.edge) => [];
	let edge_attributes = (edge: G.E.t): list(Graphviz.DotAttributes.edge) => {
		switch(G.E.label(edge)) {
			| True => [`Color(0x00FF00)];
			| False => [`Color(0xFF0000)];
      | Simple => [`Color(0x000000)];
		};
	};
 
 
	let vertex_name = (vertex: G.V.t): string => {
		switch(vertex) {		
			| Begin => "begin"
			| End => "end"
			| Node({statement: _, expr: _, id: id}) => Printf.sprintf("node_%d", id);
      | AfterIf(id) => Printf.sprintf("node_%d", id);
		}
	}
};
 
module DotExport = Graph.Graphviz.Dot(GraphPrinter)
 
let export_graph = (graph: G.t) => {
  DotExport.output_graph(Stdio.Out_channel.stdout, graph);
}

let cfg_make = (program: Syntax.Program.t) => {
  let vertex = Vertex.Begin;
  let graph = G.add_vertex(graph, vertex);
  let tmp = make_CFG(program, ~default= {g: graph, id: 0, last: vertex, edge: Edge.Simple});
  let vertex = Vertex.End;
  let cfg = G.add_vertex(tmp.g, vertex);
  let edge = G.E.create(tmp.last, Edge.Simple, vertex);
  let cfg = G.add_edge_e(cfg, edge);
  cfg;
}

module Boolean = {
  type t = 
    | Any
    | Boolean(bool)
    | Never
  
  let join = (x, y) => {
    switch (x, y) {
      | (Any, _)
      | (_, Any) => Any
      | (Never, x)
      | (x, Never) => x
      | (Boolean(x), Boolean(y)) => {
        switch Bool.(x == y) {
        | true => Boolean(x)
        | false => Any
        };
      }
    };
  };

  let equal = (x:t, y:t) => {
    switch (x, y) {
      | (Never, _)
      | (_, Never) => Never
      | (Any, _)
      | (_, Any) => Any
      | (Boolean(x), Boolean(y)) => Boolean(Bool.(x == y))
    };
  };

  let notequal = (x:t, y:t) => {
    switch (x, y) {
      | (Never, _)
      | (_, Never) => Never
      | (Any, _)
      | (_, Any) => Any
      | (Boolean(x), Boolean(y)) => Boolean(Bool.(x != y))
    };
  };

  let (==) = (x, y) => {
    switch (x, y) {
    | (Any, Any)
    | (Never, Never) => true
    | (Boolean(x), Boolean(y)) => Bool.(x == y)
    | _ => false
    };
  };
};

module Nil = {
  type t = 
    | Any
    | Never
  
  let join = (x, y) => {
    switch (x, y) {
    | (Any, _)
    | (_, Any) => Any
    | _ => Never
    };
  };

  let equal = (x:t, y:t) => {
    switch (x, y) {
      | (Any, Any) => Boolean.Boolean(true)
      | _ => Boolean.Never
    };
  };

  let notequal = (x:t, y:t) => {
    switch (x, y) {
      | (Any, Any) => Boolean.Boolean(false)
      | _ => Boolean.Never
    };
  };

  let (==) = (x, y) => {
    switch (x, y) {
    | (Any, Any)
    | (Never, Never) => true
    | _ => false
    };
  };
};

module Integer = {
  type t = 
    | Any
    | Integer(int)
    | Never
  
  let join = (x, y) => {
    switch (x, y) {
      | (Any, _)
      | (_, Any) => Any
      | (Never, x)
      | (x, Never) => x
      | (Integer(x), Integer(y)) => {
        switch (x == y) {
          | true => Integer(x)
          | false => Any
        };
      }
    };
  };

  let add = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Never
    | (Any, _)
    | (_, Any) => Any
    | (Integer(x), Integer(y)) => Integer(x + y)
    };
  };

  let subtruct = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Never
    | (Any, _)
    | (_, Any) => Any
    | (Integer(x), Integer(y)) => Integer(x - y)
    };
  };

  let multiply = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Never
    | (Any, _)
    | (_, Any) => Any
    | (Integer(x), Integer(y)) => Integer(x * y)
    };
  };

  let divide = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Never
    | (_, Any) => Any
    | (Any, Integer(x)) => {
      switch (x == 0) {
      | true => Never
      | false => Any
      };
    }
    | (Integer(x), Integer(y)) => {
      switch (y == 0) {
      | true => Never
      | false => Integer(x / y)
      };
    }
    };
  };

  let equal = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Boolean.Never
    | (Any, _)
    | (_, Any) => Boolean.Any
    | (Integer(x), Integer(y)) => Boolean.Boolean(x == y)
    };
  };

  let notequal = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Boolean.Never
    | (Any, _)
    | (_, Any) => Boolean.Any
    | (Integer(x), Integer(y)) => Boolean.Boolean(x != y)
    };
  };

  let less = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Boolean.Never
    | (Any, _)
    | (_, Any) => Boolean.Any
    | (Integer(x), Integer(y)) => Boolean.Boolean(x < y)
    };
  };

  let lessorequal = (x:t, y:t) => {
    switch (x, y) {
    | (Never, _)
    | (_, Never) => Boolean.Never
    | (Any, _)
    | (_, Any) => Boolean.Any
    | (Integer(x), Integer(y)) => Boolean.Boolean(x <= y)
    };
  };

  let greater = (x:t, y:t) => {
    less(y, x);
  };

  let greaterorequal = (x:t, y:t) => {
    lessorequal(y, x);
  };

  let (==) = (x, y) => {
    switch (x, y) {
    | (Any, Any)
    | (Never, Never) => true
    | (Integer(x), Integer(y)) => x == y
    | _ => false
    };
  };
};

module Value = {
  type t = {
    int: Integer.t, 
    bool: Boolean.t, 
    nil: Nil.t
  };

  let join = (x: t, y: t): t => {
    {int: Integer.join(x.int, y.int), bool: Boolean.join(x.bool, y.bool), nil: Nil.join(x.nil, y.nil)};
  };

  let (==) = (x: t, y: t): bool => {
    switch (Integer.(x.int == y.int), Boolean.(x.bool == y.bool), Nil.(x.nil == y.nil)) {
    | (true, true, true) => true
    | _ => false
    };
  }

  let empty: t = {int: Integer.Never, bool: Boolean.Never, nil: Nil.Any};
};

module Environment = {
  type t = Map.M(Syntax.Identifier).t(Value.t);

  let empty: t = Map.empty((module Syntax.Identifier));

  let to_string = (e: t): string => {
    "[" ++ Map.fold(
         e,
         ~init="",
         ~f=(~key, ~data, s) => {
            let sep = switch (String.(s == "")) {
            | true => ""
            | false => ", "
            };

            let n = switch (data.nil) {
              | Nil.Any => "nil"
              | Nil.Never => ""
            };

            let b = switch (data.bool) {
              | Boolean.Any => "Boolean"
              | Boolean.Boolean(x) => Bool.to_string(x)
              | Boolean.Never => ""
            };

            let i = switch (data.int) {
              | Integer.Any => "Integer"
              | Integer.Integer(x) => Int.to_string(x)
              | Integer.Never => ""
            };

            let n = switch (String.(n == ""), String.(b == "")) {
              | (true, true) => ""
              | (false, true) => n
              | (true, false) => b
              | _ => n ++ " | " ++ b
            };
            
            let n = switch (String.(n == ""), String.(i == "")) {
              | (true, true) => ""
              | (false, true) => n
              | (true, false) => i
              | _ => n ++ " | " ++ i
            };
            
            let n = switch n {
            | "nil" => ""
            | _ => n
            };

            switch String.(n == "") {
            | true => s
            | false => s ++ sep ++ key ++ " → " ++ n
            };
          }
    ) ++ "]";
  };

  let print = (e: t) => {
    let tmp = to_string(e);
    Stdio.printf("%s\n", tmp);
  };
};

let rec calculate_expression = (expression: Syntax.Expression.t, environment: Environment.t): Value.t => {
  switch (expression) {
    | Syntax.Expression.Literal(literal) => {
      switch (literal) {
        | Syntax.Expression.Literal.Nil => {int: Integer.Never, bool: Boolean.Never, nil: Nil.Any}
        | Syntax.Expression.Literal.Boolean(literal) => {int: Integer.Never, bool: Boolean.Boolean(literal), nil: Nil.Never}
        | Syntax.Expression.Literal.Integer(literal) => {int: Integer.Integer(literal), bool: Boolean.Never, nil: Nil.Never}
      };
    }
    | Syntax.Expression.BinaryExpression({left, operator, right}) => {
      switch (operator) {
        | Syntax.Expression.BinaryOperator.Add => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          let tmp: Value.t = {int: Integer.add(left_expression.int, right_expression.int), bool: Boolean.Never, nil: Nil.Never};
          switch (Integer.(tmp.int == Integer.Never)) {
          | true => Value.empty
          | false => tmp
          };
        }
        | Syntax.Expression.BinaryOperator.Subtract => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          let tmp: Value.t = {int: Integer.subtruct(left_expression.int, right_expression.int), bool: Boolean.Never, nil: Nil.Never};
          switch (Integer.(tmp.int == Integer.Never)) {
          | true => Value.empty
          | false => tmp
          };
        }
        | Syntax.Expression.BinaryOperator.Multiply => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          let tmp: Value.t = {int: Integer.multiply(left_expression.int, right_expression.int), bool: Boolean.Never, nil: Nil.Never};
          switch (Integer.(tmp.int == Integer.Never)) {
          | true => Value.empty
          | false => tmp
          };
        }
        | Syntax.Expression.BinaryOperator.Divide => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          let tmp: Value.t = {int: Integer.divide(left_expression.int, right_expression.int), bool: Boolean.Never, nil: Nil.Never};
          switch (Integer.(tmp.int == Integer.Never)) {
          | true => Value.empty
          | false => tmp
          };
        }

        | Syntax.Expression.BinaryOperator.Equal => {
          let left = calculate_expression(left, environment);
          let right = calculate_expression(right, environment);
          switch (left, right) {
          | ({int: x, bool: Boolean.Never, nil: Nil.Never}, {int: y, bool: Boolean.Never, nil: Nil.Never}) => {
            let tmp: Value.t = {int: Integer.Never, bool: Integer.equal(x, y), nil: Nil.Never};
            switch (Boolean.(tmp.bool == Boolean.Never)) {
            | true => Value.empty
            | false => tmp
            };
          }
          | ({int: Integer.Never, bool: x, nil: Nil.Never}, {int: Integer.Never, bool: y, nil: Nil.Never}) => {
            let tmp: Value.t = {int: Integer.Never, bool: Boolean.equal(x, y), nil: Nil.Never};
            switch (Boolean.(tmp.bool == Boolean.Never)) {
            | true => Value.empty
            | false => tmp
            };
          }
          | ({int: Integer.Never, bool: Boolean.Never, nil: x}, {int: Integer.Never, bool: Boolean.Never, nil: y}) => {
            let tmp: Value.t = {int: Integer.Never, bool: Nil.equal(x, y), nil: Nil.Never};
            switch (Boolean.(tmp.bool == Boolean.Never)) {
            | true => Value.empty
            | false => tmp
            };
          }
          | (left, right) => {
            {int: Integer.Never, bool: Boolean.join(Boolean.join(Integer.equal(left.int, right.int), Boolean.equal(left.bool, right.bool)), Nil.equal(left.nil, right.nil)), nil: Nil.Any};
          }
          };
        }
        | Syntax.Expression.BinaryOperator.NotEqual => {
          let left = calculate_expression(left, environment);
          let right = calculate_expression(right, environment);
          switch (left, right) {
          | ({int: x, bool: Boolean.Never, nil: Nil.Never}, {int: y, bool: Boolean.Never, nil: Nil.Never}) => {
            let tmp: Value.t = {int: Integer.Never, bool: Integer.notequal(x, y), nil: Nil.Never};
            switch (Boolean.(tmp.bool == Boolean.Never)) {
            | true => Value.empty
            | false => tmp
            };
          }
          | ({int: Integer.Never, bool: x, nil: Nil.Never}, {int: Integer.Never, bool: y, nil: Nil.Never}) => {
            let tmp: Value.t = {int: Integer.Never, bool: Boolean.notequal(x, y), nil: Nil.Never};
            switch (Boolean.(tmp.bool == Boolean.Never)) {
            | true => Value.empty
            | false => tmp
            };
          }
          | ({int: Integer.Never, bool: Boolean.Never, nil: x}, {int: Integer.Never, bool: Boolean.Never, nil: y}) => {
            let tmp: Value.t = {int: Integer.Never, bool: Nil.notequal(x, y), nil: Nil.Never};
            switch (Boolean.(tmp.bool == Boolean.Never)) {
            | true => Value.empty
            | false => tmp
            };
          }
          | (left, right) => {
            {int: Integer.Never, bool: Boolean.join(Boolean.join(Integer.notequal(left.int, right.int), Boolean.notequal(left.bool, right.bool)), Nil.notequal(left.nil, right.nil)), nil: Nil.Any};
          }
          };
        }
        | Syntax.Expression.BinaryOperator.Less => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | ({int: x, bool: Boolean.Never, nil: Nil.Never}, {int: y, bool: Boolean.Never, nil: Nil.Never}) => {
              let tmp: Value.t = {int: Integer.Never, bool: Integer.less(x, y), nil: Nil.Never};
              switch (Boolean.(tmp.bool == Boolean.Never)) {
              | true => Value.empty
              | false => tmp
              };
            }
            | ({int: x, bool: _, nil: _}, {int: y, bool: _, nil: _}) => {int: Integer.Never, bool: Integer.less(x, y), nil: Nil.Any}
          };
        }
        | Syntax.Expression.BinaryOperator.LessOrEqual => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | ({int: x, bool: Boolean.Never, nil: Nil.Never}, {int: y, bool: Boolean.Never, nil: Nil.Never}) => {
              let tmp: Value.t = {int: Integer.Never, bool: Integer.lessorequal(x, y), nil: Nil.Never};
              switch (Boolean.(tmp.bool == Boolean.Never)) {
              | true => Value.empty
              | false => tmp
              };
            }
            | ({int: x, bool: _, nil: _}, {int: y, bool: _, nil: _}) => {int: Integer.Never, bool: Integer.lessorequal(x, y), nil: Nil.Any}
          };
        }
        | Syntax.Expression.BinaryOperator.Greater => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | ({int: x, bool: Boolean.Never, nil: Nil.Never}, {int: y, bool: Boolean.Never, nil: Nil.Never}) => {
              let tmp: Value.t = {int: Integer.Never, bool: Integer.greater(x, y), nil: Nil.Never};
              switch (Boolean.(tmp.bool == Boolean.Never)) {
              | true => Value.empty
              | false => tmp
              };
            }
            | ({int: x, bool: _, nil: _}, {int: y, bool: _, nil: _}) => {int: Integer.Never, bool: Integer.greater(x, y), nil: Nil.Any}
          };
        }
        | Syntax.Expression.BinaryOperator.GreaterOrEqual => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | ({int: x, bool: Boolean.Never, nil: Nil.Never}, {int: y, bool: Boolean.Never, nil: Nil.Never}) => {
              let tmp: Value.t = {int: Integer.Never, bool: Integer.greaterorequal(x, y), nil: Nil.Never};
              switch (Boolean.(tmp.bool == Boolean.Never)) {
              | true => Value.empty
              | false => tmp
              };
            }
            | ({int: x, bool: _, nil: _}, {int: y, bool: _, nil: _}) => {int: Integer.Never, bool: Integer.greaterorequal(x, y), nil: Nil.Any}
          };
        }
      };
    }
    | Syntax.Expression.Identifier(id) => {
      let value = Map.find(environment, id);
      switch (value) {
        | None => {int: Integer.Never, bool: Boolean.Never, nil: Nil.Any}
        | Some(value) => value
      };
    }
    | Syntax.Expression.ParenthesizedExpression(expression) => calculate_expression(expression, environment)
    | Syntax.Expression.InputExpression => {int: Integer.Any, bool: Boolean.Never, nil: Nil.Never}
  };
};

module A = {
  type vertex = G.vertex;
  type edge = G.edge;
  type g = G.t;
  type data = option(Environment.t);
  let direction = Graph.Fixpoint.Forward;

  /*let join = (x: data, y: data): Environment.t => {
    Map.merge(x, y, ~f= (~key as _, both): option(Value.t) => {
      switch (both) {
      | `Both(v1, v2) => Some({int: Integer.join(v1.int, v2.int), bool: Boolean.join(v1.bool, v2.bool), nil: Nil.join(v1.nil, v2.nil)});
      | `Left(v1) => {
        let tmp: Value.t = {int: Integer.Never, bool: Boolean.Never, nil: Nil.Any};
        Some({int: Integer.join(v1.int, tmp.int), bool: Boolean.join(v1.bool, tmp.bool), nil: Nil.join(v1.nil, tmp.nil)});
      }
      | `Right(v2) => {
        let tmp: Value.t = {int: Integer.Never, bool: Boolean.Never, nil: Nil.Any};
        Some({int: Integer.join(v2.int, tmp.int), bool: Boolean.join(v2.bool, tmp.bool), nil: Nil.join(v2.nil, tmp.nil)});
      }
    };
    });
  };*/

  let join = (x: data, y: data): data => {
    switch (x, y) {
    | (None, None) => None
    | (None, Some(x))
    | (Some(x), None) => Some(x)
    | (Some(x), Some(y)) => {
      Some(Map.merge(x, y, ~f= (~key as _, both): option(Value.t) => {
        switch (both) {
        | `Both(v1, v2) => Some({int: Integer.join(v1.int, v2.int), bool: Boolean.join(v1.bool, v2.bool), nil: Nil.join(v1.nil, v2.nil)});
        | `Left(v1) => {
          let tmp: Value.t = {int: Integer.Never, bool: Boolean.Never, nil: Nil.Any};
          Some({int: Integer.join(v1.int, tmp.int), bool: Boolean.join(v1.bool, tmp.bool), nil: Nil.join(v1.nil, tmp.nil)});
        }
        | `Right(v2) => {
          let tmp: Value.t = {int: Integer.Never, bool: Boolean.Never, nil: Nil.Any};
          Some({int: Integer.join(v2.int, tmp.int), bool: Boolean.join(v2.bool, tmp.bool), nil: Nil.join(v2.nil, tmp.nil)});
        }
        };
      }))
    }
  };
}

  /*let join = (x: data, y: data): data => {
    switch (x, y) {
    | (None, None) => None
    | (None, Some(x))
    | (Some(x), None) => Some(x)
    | (Some(x), Some(y)) => {
      Some(Map.merge_skewed(x, y, ~combine=(~key as _, v1, v2)=> {
        Value.join(v1, v2);
      }))
    }
    };
  };*/

  let equal = (x: data, y: data) => {
    switch (x, y) {
    | (Some(x), Some(y)) => Map.equal(Value.(==), x, y)
    | (None, None) => true
    | _ => false
    }
    
  };

  let analyze = (edge: edge, data: data): data => {
    let vertex = G.E.dst(edge);
    switch data {
    | None => None
    | Some(data) => {
      switch(vertex) {
        | Vertex.Node({statement: statement, expr: expr, id: _}) => {
            switch statement {
            | Vertex.Assignment(var) => Some(Map.set(data, ~key=var, ~data=calculate_expression(expr, data)));
            | _ => Some(data)
            };
          };  
        | _ => Some(data);
      };

    }
    };
  };
};

module Analyzer = Graph.Fixpoint.Make(G)(A);

let make_analysis = (program: Syntax.Program.t) => {
  let cfg = cfg_make(program);
  let init = (v: A.vertex): A.data => {
    switch(v) {
    | Vertex.Begin => Some(Environment.empty)
    | _ => None
    }
  };
  let result = Analyzer.analyze(init, cfg);
  switch (result(Vertex.End)) {
  | Some(x) => Environment.print(x);
  | None => Environment.print(Environment.empty)
  };
};

