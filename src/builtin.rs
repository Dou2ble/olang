use crate::{
    location::Region,
    value::{ControlFlowValue, Exception, ExceptionKind, Value},
};
use std::{io, thread, time::Duration};

fn expect_num_of_argumets(
    arguments: &Vec<Value>,
    num: usize,
    region: &Region,
) -> Result<(), ControlFlowValue> {
    if arguments.len() != num {
        Err(ControlFlowValue::Exception(Exception {
            kind: ExceptionKind::WrongNumberOfArguments,
            region: region.clone(),
        }))
    } else {
        Ok(())
    }
}

pub fn print_ln(arguments: Vec<Value>, _region: &Region) -> Result<Value, ControlFlowValue> {
    let mut result = String::new();
    for arg in arguments.iter() {
        result.push_str(format!("{}", arg).as_str())
    }

    println!("{}", result);
    Ok(Value::Null)
}

pub fn to_string(arguments: Vec<Value>, region: &Region) -> Result<Value, ControlFlowValue> {
    expect_num_of_argumets(&arguments, 1, region)?;
    Ok(Value::String(format!("{}", arguments.first().unwrap())))
}

pub fn read_ln(arguments: Vec<Value>, region: &Region) -> Result<Value, ControlFlowValue> {
    expect_num_of_argumets(&arguments, 0, region)?;
    let mut input = String::new();
    io::stdin().read_line(&mut input).map_err(|err| {
        ControlFlowValue::Exception(Exception {
            kind: ExceptionKind::Custom(err.to_string()),
            region: region.clone(),
        })
    })?;

    Ok(Value::String(input.trim().to_string()))
}

pub fn len(arguments: Vec<Value>, region: &Region) -> Result<Value, ControlFlowValue> {
    expect_num_of_argumets(&arguments, 1, region)?;

    Ok(Value::Int(
        arguments.first().unwrap().into_list(region)?.len() as i64,
    ))
}

pub fn sleep(arguments: Vec<Value>, region: &Region) -> Result<Value, ControlFlowValue> {
    expect_num_of_argumets(&arguments, 1, region)?;

    thread::sleep(Duration::from_millis(*arguments[0].into_int(region)? as u64));

    Ok(Value::Null)
}
