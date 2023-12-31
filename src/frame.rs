use crate::{bytecode::Instructions, object};

// Frame - Data structure that holds execution-relevant information. Short for "call frame" or "stack frame"
// and sometimes "activation record". On real machines a frame is not something separate from, but a designated
// part of "the stack". It's where the return address, the arguments to the current function, and it's local
// variables are stored. In VM land we don't have to use the stack. We're not constrained by standardized
// calling conventions and other much too real things, like real memory addresses and locations. Since we can
// store frames anywhere we like. What's kept on the stack and what's not differs from VM to VM. Some keep
// everything on the stack, others only the return address, some only the local variables, some the local
// variables and the arguments of the function call. The implementation depends on the language being
// implemented, the requirements in regards to concurrency and performance, the host language, and more.
// We are choosing the way that is easiest to build, understand, extend, etc.
pub struct Frame {
    closure: object::Closure,
    ip: i64,
    base_pointer: i64,
}

impl Frame {
    pub fn new(closure: object::Closure, base_pointer: i64) -> Self {
        Frame {
            closure,
            ip: -1,
            base_pointer,
        }
    }

    fn instructions(&self) -> &Instructions {
        &self.closure.func.instructions
    }
}
