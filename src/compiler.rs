use crate::ast::{
    ArrayLiteral, BlockStatement, Boolean, ConstStatement, Expression, ExpressionStatement,
    FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
    IntegerLiteral, LetStatement, Node, PrefixExpression, RootNode, StringLiteral,
};
use crate::builtins::BUILTINS;
use crate::bytecode::make_instruction;
use crate::bytecode::{self, Instructions, Opcode};
use crate::object::{CompiledFunc, Integer, Object};
use crate::symbol_table::{Symbol, SymbolScope, SymbolTable};

/// Bytecode contains the Instructions our compiler
/// generated and the constants the compiler evaluated.
struct Bytecode {
    instructions: bytecode::Instructions,
    constants: Vec<Box<dyn Object>>,
}

#[derive(Copy, Clone)]
/// Represents an instruction through an opcode and it's position.
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

impl EmittedInstruction {
    fn new(opcode: Opcode, position: usize) -> Self {
        EmittedInstruction { opcode, position }
    }
}

/// Before we start compiling a function's body (enter a new scope) we push
/// a new CompilationScope on to the scopes stack. While compiling inside
/// this scope, the emit method of the compiler will modify only the fields
/// of the current CompilationScope. Once we're done compiling the function,
/// we leave the scope by popping it off the scopes stack and putting the
/// instructions in a new object::CompiledFunction
struct CompilationScope {
    instructions: Instructions,
    last_instruction: EmittedInstruction,
    prev_instruction: EmittedInstruction,
}

/// defines our compiler with instructions which hold the generated bytecode,
/// constants which serve as our constant pool, and the last and previous
/// instructions
struct Compiler {
    constants: Vec<Box<dyn Object>>,
    symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_index: i64,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope {
            instructions: Instructions::new(vec![]),
            last_instruction: EmittedInstruction::new(Opcode::OpNull, 0),
            prev_instruction: EmittedInstruction::new(Opcode::OpNull, 0),
        };
        let mut symbol_table = SymbolTable::new();

        for (i, v) in BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i as i64, v.0.clone());
        }

        Compiler {
            constants: vec![],
            symbol_table,
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions(),
            constants: self.constants,
        }
    }

    fn current_instructions(&self) -> Instructions {
        self.scopes[self.scope_index as usize].instructions.clone()
    }

    fn current_instructions_mut(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index as usize].instructions
    }

    pub fn emit(&mut self, op: Opcode, operands: Vec<i32>) -> i64 {
        let ins = make_instruction(op, operands);
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos as usize);

        pos
    }

    fn add_instruction(&mut self, instruction: Vec<u8>) -> i64 {
        let new_instruction_pos = self.current_instructions().len();
        self.scopes[self.scope_index as usize]
            .instructions
            .extend(instruction);

        new_instruction_pos
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.scopes[self.scope_index as usize].last_instruction;
        let last = EmittedInstruction::new(op, pos);

        self.scopes[self.scope_index as usize].prev_instruction = previous;
        self.scopes[self.scope_index as usize].last_instruction = last;
    }

    pub fn add_constant(&mut self, obj: Box<dyn Object>) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            SymbolScope::Global => {
                self.emit(Opcode::OpGetGlobal, vec![symbol.index as i32]);
            }
            SymbolScope::Local => {
                self.emit(Opcode::OpGetLocal, vec![symbol.index as i32]);
            }
            SymbolScope::Builtin => {
                self.emit(Opcode::OpGetBuiltin, vec![symbol.index as i32]);
            }
            SymbolScope::Free => {
                self.emit(Opcode::OpGetFree, vec![symbol.index as i32]);
            }
            SymbolScope::Function => {
                self.emit(Opcode::OpCurrentClosure, vec![]);
            }
        }
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: Instructions::new(vec![]),
            last_instruction: EmittedInstruction::new(Opcode::OpNull, 0),
            prev_instruction: EmittedInstruction::new(Opcode::OpNull, 0),
        };
        self.scopes.push(scope);
        self.scope_index += 1;
        self.symbol_table = SymbolTable::new_enclosed(Some(Box::new(self.symbol_table.clone())));
    }

    pub fn leave_scope(&mut self) -> Instructions {
        let instructions = self.current_instructions();
        self.scopes.pop();
        self.scope_index -= 1;

        if let Some(outer_symbol_table) = self.symbol_table.get_outer() {
            self.symbol_table = *outer_symbol_table;
        }

        instructions
    }

    fn change_operand(&mut self, op_pos: i64, operand: i64) {
        let op = self.current_instructions()[op_pos.try_into().unwrap()];
        let new_instruction = make_instruction(Opcode::from(op), vec![operand as i32]);
        self.replace_instruction(op_pos, new_instruction);
    }

    fn replace_instruction(&mut self, position: i64, new_instruction: Vec<u8>) {
        let ins = self.current_instructions_mut();
        for (i, byte) in new_instruction.iter().enumerate() {
            ins[position as usize + i] = *byte;
        }
    }

    fn last_instruction_is(&self, op: Opcode) -> bool {
        if self.current_instructions().len() == 0 {
            return false;
        }
        self.scopes
            .get(self.scope_index as usize)
            .map_or(false, |scope| match &scope.last_instruction {
                last_instr => last_instr.opcode == op,
            })
    }

    fn remove_last_pop(&mut self) {
        if let Some(scope) = self.scopes.get_mut(self.scope_index as usize) {
            let last_instr = scope.last_instruction;
            let new_instructions_slice = &scope.instructions.as_vec_u8()[..last_instr.position];
            let new_instructions = Instructions::new(new_instructions_slice.to_vec());
            scope.instructions = new_instructions;
            scope.last_instruction = scope.prev_instruction.clone();
        }
    }

    fn replace_last_pop_with_return(&mut self) {
        // Clone the necessary data to avoid mutable borrow conflict
        let last_pos = if let Some(scope) = self.scopes.get(self.scope_index as usize) {
            scope.last_instruction.position
        } else {
            return; // TODO: what happens in this situation?
        };

        let return_instruction = make_instruction(Opcode::OpReturnValue, vec![]);
        self.replace_instruction(last_pos as i64, return_instruction);

        if let Some(scope) = self.scopes.get_mut(self.scope_index as usize) {
            scope.last_instruction.opcode = Opcode::OpReturnValue;
        }
    }

    // TODO: PostfixExpression after implementing them.
    pub fn compile(&mut self, node: &dyn Node) -> Result<(), String> {
        if let Some(root_node) = node.as_any().downcast_ref::<RootNode>() {
            for statement in &root_node.statements {
                self.compile(statement.as_node())?;
            }
        } else if let Some(expression_stmt) = node.as_any().downcast_ref::<ExpressionStatement>() {
            self.compile(expression_stmt.expression.as_node())?;
            self.emit(Opcode::OpPop, vec![]);
        } else if let Some(infix_expr) = node.as_any().downcast_ref::<InfixExpression>() {
            if infix_expr.operator == "<" || infix_expr.operator == "<=" {
                self.compile(infix_expr.right.as_node())?;
                self.compile(infix_expr.left.as_node())?;
                match infix_expr.operator.as_str() {
                    "<" => {
                        self.emit(Opcode::OpGreater, vec![]);
                    }
                    _ => {
                        self.emit(Opcode::OpGreaterEqual, vec![]);
                    }
                }
            } else {
                self.compile(infix_expr.left.as_node())?;
                self.compile(infix_expr.right.as_node())?;

                match infix_expr.operator.as_str() {
                    "+" => {
                        self.emit(Opcode::OpAdd, vec![]);
                    }
                    "-" => {
                        self.emit(Opcode::OpSub, vec![]);
                    }
                    "*" => {
                        self.emit(Opcode::OpMul, vec![]);
                    }
                    "/" => {
                        self.emit(Opcode::OpDiv, vec![]);
                    }
                    "%" => {
                        self.emit(Opcode::OpMod, vec![]);
                    }
                    ">" => {
                        self.emit(Opcode::OpGreater, vec![]);
                    }
                    ">=" => {
                        self.emit(Opcode::OpGreaterEqual, vec![]);
                    }
                    "==" => {
                        self.emit(Opcode::OpEqualEqual, vec![]);
                    }
                    "!=" => {
                        self.emit(Opcode::OpNotEqual, vec![]);
                    }
                    "&&" => {
                        self.emit(Opcode::OpAnd, vec![]);
                    }
                    "||" => {
                        self.emit(Opcode::OpOr, vec![]);
                    }
                    _ => return Err(format!("unknown operator {}", infix_expr.operator)),
                }
            }
        } else if let Some(int_literal) = node.as_any().downcast_ref::<IntegerLiteral>() {
            let integer = Integer {
                value: int_literal.value as i64,
            };
            let constant_index = self.add_constant(Box::new(integer));
            self.emit(Opcode::OpConstant, vec![constant_index as i32]);
        } else if let Some(boolean) = node.as_any().downcast_ref::<Boolean>() {
            if boolean.value {
                self.emit(Opcode::OpTrue, vec![]);
            } else {
                self.emit(Opcode::OpFalse, vec![]);
            }
        } else if let Some(prefix_expr) = node.as_any().downcast_ref::<PrefixExpression>() {
            self.compile(prefix_expr.right.as_node())?;

            match prefix_expr.operator.as_str() {
                "!" => {
                    self.emit(Opcode::OpBang, vec![]);
                }
                "-" => {
                    self.emit(Opcode::OpMinus, vec![]);
                }
                _ => return Err(format!("unknown operator {}", prefix_expr.operator)),
            }
        } else if let Some(if_expr) = node.as_any().downcast_ref::<IfExpression>() {
            self.compile(if_expr.condition.as_node())?;

            // Emit an `OpJumpNotTruthy` with a bogus value. After compiling the consequence, we will know
            // how far to jump and can come back and change it - "back-patching". Because we are creating
            // a single pass compiler this is the solution, however more complex compilers may not come
            // back to change this on first pass and instead fill it in on another pass
            let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, vec![9999]);
            self.compile(if_expr.consequence.as_node())?;

            if self.last_instruction_is(Opcode::OpPop) {
                self.remove_last_pop();
            }

            // Emit an `OpJump` with bogus value - see similar explanation above `jumpNotTruthyPos`
            // variable declaration.
            let jump_position = self.emit(Opcode::OpJump, vec![9999]);
            let after_consequence_pos = self.current_instructions().len();

            // Change the jump-to position in the OpJumpNotTruthy emission earlier
            self.change_operand(jump_not_truthy_pos, after_consequence_pos);
            if if_expr.alternative.statements.len() > 0 {
                for stmt in &if_expr.alternative.statements {
                    self.compile(stmt.as_node())?;
                }

                if self.last_instruction_is(Opcode::OpPop) {
                    self.remove_last_pop();
                }
            } else {
                self.emit(Opcode::OpNull, vec![]);
            }

            let after_alt_pos = self.current_instructions().len();
            self.change_operand(jump_position, after_alt_pos);
        } else if let Some(block_stmt) = node.as_any().downcast_ref::<BlockStatement>() {
            for statement in &block_stmt.statements {
                self.compile(statement.as_node())?;
            }
        } else if let Some(let_stmt) = node.as_any().downcast_ref::<LetStatement>() {
            let symbol = self.symbol_table.define(let_stmt.name.value.clone());

            self.compile(let_stmt.value.as_node())?;

            match symbol.scope {
                SymbolScope::Global => {
                    self.emit(Opcode::OpSetGlobal, vec![symbol.index as i32]);
                }
                _ => {
                    self.emit(Opcode::OpSetLocal, vec![symbol.index as i32]);
                }
            }
        } else if let Some(const_stmt) = node.as_any().downcast_ref::<ConstStatement>() {
            let symbol = self.symbol_table.define(const_stmt.name.value.clone());

            self.compile(const_stmt.value.as_node())?;

            match symbol.scope {
                SymbolScope::Global => {
                    self.emit(Opcode::OpSetGlobal, vec![symbol.index as i32]);
                }
                _ => {
                    self.emit(Opcode::OpSetLocal, vec![symbol.index as i32]);
                }
            }
        } else if let Some(identifier) = node.as_any().downcast_ref::<Identifier>() {
            match self.symbol_table.resolve(&identifier.value) {
                Some(symbol) => self.load_symbol(symbol),
                None => return Err(format!("undefined variable {}", identifier.value)),
            }
        } else if let Some(identifier) = node.as_any().downcast_ref::<StringLiteral>() {
            let string_obj = crate::object::Str {
                value: identifier.value.clone(),
            };
            let constant_index = self.add_constant(Box::new(string_obj));
            self.emit(Opcode::OpConstant, vec![constant_index as i32]);
        } else if let Some(array_literal) = node.as_any().downcast_ref::<ArrayLiteral>() {
            for el in &array_literal.elements {
                self.compile(el.as_node())?;
            }
            self.emit(Opcode::OpArray, vec![array_literal.elements.len() as i32]);
        } else if let Some(hash_literal) = node.as_any().downcast_ref::<HashLiteral>() {
            let mut keys: Vec<_> = hash_literal.pairs.keys().collect();
            keys.sort();

            for key in keys {
                self.compile(hash_literal.pairs.get(key).unwrap().as_node())?;
            }

            self.emit(Opcode::OpHash, vec![hash_literal.pairs.len() as i32 * 2]);
        } else if let Some(index_expr) = node.as_any().downcast_ref::<IndexExpression>() {
            self.compile(index_expr.left.as_node())?;
            self.compile(index_expr.index.as_node())?;
            self.emit(Opcode::OpIndex, vec![]);
        } else if let Some(func_literal) = node.as_any().downcast_ref::<FunctionLiteral>() {
            self.enter_scope();

            let func_name = func_literal.name.borrow();
            if !func_name.to_string().is_empty() {
                self.symbol_table.define_function(func_name.to_string());
            }

            for param in &func_literal.parameters {
                self.symbol_table.define(param.value.clone());
            }

            self.compile(func_literal.body.as_node())?;

            if self.last_instruction_is(Opcode::OpPop) {
                self.replace_last_pop_with_return();
            }
            if !self.last_instruction_is(Opcode::OpReturnValue) {
                self.emit(Opcode::OpReturn, vec![]);
            }

            let free_symbols = self.symbol_table.free_symbols.clone();
            let free_symbols_len = free_symbols.len();
            let num_locals = self.symbol_table.num_definitions;
            let instructions = self.leave_scope();

            for symbol in free_symbols {
                self.load_symbol(symbol);
            }

            let compiled_func = CompiledFunc {
                instructions,
                num_locals,
                num_params: func_literal.parameters.len(),
            };

            let func_index = self.add_constant(Box::new(compiled_func));
            self.emit(
                Opcode::OpClosure,
                vec![func_index as i32, free_symbols_len as i32],
            );
        } else {
            return Err(String::from("unknown node type"));
        }

        Ok(())
    }
}
