use fender::{
    stdlib::{if_func, print_func},
    FenderBinaryOperator, FenderReference, FenderTypeSystem, FenderValue,
};
use freight_vm::{
    expression::{Expression, NativeFunction},
    function::FunctionWriter,
    vm_writer::VMWriter,
};

fn main() {
    let mut writer: VMWriter<FenderTypeSystem> = VMWriter::new();
    let mut main = FunctionWriter::new(0);

    let fib_ref = writer.create_global();

    let mut run_if_true = FunctionWriter::new_capturing(0, vec![0]);
    run_if_true.return_expression(Expression::BinaryOpEval(
        FenderBinaryOperator::Add,
        [
            Expression::DynamicFunctionCall(
                Expression::Global(fib_ref).into(),
                vec![Expression::BinaryOpEval(
                    FenderBinaryOperator::Sub,
                    [
                        Expression::CapturedValue(0),
                        Expression::RawValue(FenderReference::FRaw(FenderValue::Int(2))),
                    ]
                    .into(),
                )],
            ),
            Expression::DynamicFunctionCall(
                Expression::Global(fib_ref).into(),
                vec![Expression::BinaryOpEval(
                    FenderBinaryOperator::Sub,
                    [
                        Expression::CapturedValue(0),
                        Expression::RawValue(FenderReference::FRaw(FenderValue::Int(1))),
                    ]
                    .into(),
                )],
            ),
        ]
        .into(),
    ));
    let run_if_true = writer.include_function(run_if_true);

    let mut run_if_false = FunctionWriter::new_capturing(0, vec![0]);
    run_if_false.return_expression(Expression::CapturedValue(0));
    let run_if_false = writer.include_function(run_if_false);

    let mut fib = FunctionWriter::new(1);
    let n = fib.argument_stack_offset(0);
    fib.return_expression(Expression::NativeFunctionCall(
        NativeFunction::new(if_func),
        vec![
            Expression::BinaryOpEval(
                FenderBinaryOperator::Gt,
                [
                    Expression::Variable(n),
                    Expression::RawValue(FenderReference::FRaw(FenderValue::Int(1))),
                ]
                .into(),
            )
            .into(),
            Expression::FunctionCapture(run_if_true),
            Expression::FunctionCapture(run_if_false),
        ],
    ));
    let fib = writer.include_function(fib);
    main.evaluate_expression(Expression::AssignGlobal(
        fib_ref,
        Box::new(FenderValue::Function(fib.clone()).into()),
    ));
    main.evaluate_expression(Expression::NativeFunctionCall(
        NativeFunction::new(print_func),
        vec![Expression::StaticFunctionCall(
            fib,
            vec![Expression::RawValue(FenderReference::FRaw(
                FenderValue::Int(30),
            ))],
        )],
    ));
    let main = writer.include_function(main);
    let mut vm = writer.finish(main);
    vm.run().unwrap();
}
