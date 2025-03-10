
use crate::psx::processor::instruction::Instruction;
use crate::psx::bus::Bus;

// Macro to generate an enum with a custom name and associate functions with it
macro_rules! create_function_enum {
    // Takes the enum name, followed by a list of function names and their IDs
    ( $enum_name:ident, $( ($fn_name:ident, $id:expr) ),* ) => {
        // Define the enum with variants corresponding to the function names
        pub enum $enum_name {
            $(
                $fn_name = $id,
            )*
        }

        // Define a function to call the correct function based on the enum variant
        pub fn call_function_by_id(id: u32, arg1: &mut Bus, arg2: Instruction) {
            match id {
                $(
                    $id => $fn_name(arg1, arg2),
                )*
                _ => println!("Function not found!"),
            }
        }
    };
}

create_function_enum!(
    PrimaryOpcodes,
    (sll, 0)
);

pub fn run_instruction(psx: &mut Bus, i: Instruction) {
    call_function_by_id(i.primary(), psx, i);
}

fn illegal(psx: &mut Bus, i: Instruction) {

}

fn sll(psx: &mut Bus, i: Instruction) {
    todo!();
}