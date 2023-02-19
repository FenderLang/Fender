use fender::{stdlib::{print_func, if_func}, FenderTypeSystem, FenderValue, FenderReference, FenderBinaryOperator};
use freight_vm::{expression::Expression, function::FunctionWriter, vm_writer::VMWriter};

#[allow(unused_variables)]
fn main() {
    let mut writer: VMWriter<FenderTypeSystem> = VMWriter::new();
    let mut main = FunctionWriter::new(0);

    let fib_ref = main.create_variable();

    let mut run_if_true = FunctionWriter::new_capturing(0, vec![1]);
    run_if_true
        .return_expression(Expression::BinaryOpEval(
            FenderBinaryOperator::Add,
            Expression::DynamicFunctionCall(
                Expression::Global(fib_ref).into(),
                vec![Expression::BinaryOpEval(
                    FenderBinaryOperator::Sub,
                    Expression::Variable(run_if_true.captured_stack_offset(0)).into(),
                    Expression::RawValue(FenderReference::FRaw(FenderValue::Int(2))).into(),
                )],
            )
            .into(),
            Expression::DynamicFunctionCall(
                Expression::Global(fib_ref).into(),
                vec![Expression::BinaryOpEval(
                    FenderBinaryOperator::Sub,
                    Expression::Variable(run_if_true.captured_stack_offset(0)).into(),
                    Expression::RawValue(FenderReference::FRaw(FenderValue::Int(1))).into(),
                )],
            )
            .into(),
        ))
        .unwrap();
    let run_if_true = writer.include_function(run_if_true);

    let mut run_if_false = FunctionWriter::new_capturing(0, vec![1]);
    run_if_false
        .return_expression(Expression::Variable(run_if_false.captured_stack_offset(0)))
        .unwrap();
    let run_if_false = writer.include_function(run_if_false);

    let if_func = writer.include_native_function(if_func);
    let print_func = writer.include_native_function(print_func);

    let mut fib = FunctionWriter::new(1);
    let n = fib.argument_stack_offset(0);
    fib.return_expression(Expression::StaticFunctionCall(
        if_func,
        vec![
            Expression::BinaryOpEval(
                FenderBinaryOperator::Gt,
                Expression::Variable(n).into(),
                Expression::RawValue(FenderReference::FRaw(FenderValue::Int(1))).into(),
            )
            .into(),
            Expression::FunctionCapture(run_if_true),
            Expression::FunctionCapture(run_if_false),
        ],
    ))
    .unwrap();
    let fib = writer.include_function(fib);
    main.assign_value(
        fib_ref,
        Expression::RawValue(FenderReference::FRaw(FenderValue::Function(fib.clone()))),
    )
    .unwrap();
    main.evaluate_expression(Expression::StaticFunctionCall(
        print_func,
        vec![Expression::StaticFunctionCall(
            fib,
            vec![Expression::RawValue(FenderReference::FRaw(
                FenderValue::Int(7),
            ))],
        )],
    ))
    .unwrap();
    let main = writer.include_function(main);
    let mut vm = writer.finish(main);
    vm.run().unwrap();
}
