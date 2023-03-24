use crate::{
    fender_reference::FenderReference, fender_value::FenderValue, interpreter::create_vm,
    type_sys::type_id::FenderTypeId,
};

fn run(source: &str) -> FenderReference {
    create_vm(source).unwrap().run().unwrap()
}

#[test]
fn test_simple_values() {
    assert_eq!(*run("1"), FenderValue::Int(1));
    assert_eq!(*run("1.0"), FenderValue::Float(1.0));
    assert_eq!(
        *run("\"Hello, world!\""),
        FenderValue::String("Hello, world!".to_string().into())
    );
    assert_eq!(*run("true"), FenderValue::Bool(true));
    assert_eq!(*run("false"), FenderValue::Bool(false));
    assert_eq!(*run("null"), FenderValue::Null);
    assert_eq!(run("{}").get_type_id(), FenderTypeId::Function);
}

#[test]
fn test_algebraic_expressions() {
    assert_eq!(*run("3 + 2"), FenderValue::Int(5));
    assert_eq!(*run("3 + 2 + 2"), FenderValue::Int(7));
    assert_eq!(*run("3 + 2 / 2"), FenderValue::Int(4));
    assert_eq!(*run("(3 + 7) / 2"), FenderValue::Int(5));
    assert_eq!(*run("-2"), FenderValue::Int(-2));
}

#[test]
fn test_closures() {
    assert_eq!(*run("{1}()"), FenderValue::Int(1));
    assert_eq!(*run("{$}(1)"), FenderValue::Int(1));
    assert_eq!(*run("{{$}(1)}()"), FenderValue::Int(1));
    assert_eq!(*run("(a, b) {a + b}(1, 2)"), FenderValue::Int(3));
}

#[test]
fn test_variables() {
    assert_eq!(*run("$x = 4; x"), FenderValue::Int(4));
    assert_eq!(*run("$x = 4; $y = x / 2; y"), FenderValue::Int(2));
    assert_eq!(*run("$add = (a, b) {a + b}; 4.add(5)"), FenderValue::Int(9));
    assert_eq!(*run("$abc = 4; abc = abc + 1; abc"), FenderValue::Int(5));
}

#[test]
fn test_capture() {
    assert_eq!(*run("{$x = 10; {x}()}()"), FenderValue::Int(10));
    assert_eq!(*run("{$x = 10; {x = x / 2}(); x}()"), FenderValue::Int(5));
}

#[test]
fn test_return() {
    assert_eq!(*run("return 2; 1"), FenderValue::Int(2));
    assert_eq!(*run("{return 2; 1}()"), FenderValue::Int(2));
    assert_eq!(*run("{{return 2; 1}()}()"), FenderValue::Int(2));
    assert_eq!(
        *run("{$x = `test {return@test 2; 1}(); x + 1}()"),
        FenderValue::Int(3)
    );
}

#[test]
fn test_lists() {
    assert_eq!(
        *run("[1, 2, 3]"),
        FenderValue::List(
            vec![
                FenderValue::Int(1).into(),
                FenderValue::Int(2).into(),
                FenderValue::Int(3).into(),
            ]
            .into()
        )
    );
    assert_eq!(
        *run("$x = 4; [1, 2, 3, x]"),
        FenderValue::List(
            vec![
                FenderValue::Int(1).into(),
                FenderValue::Int(2).into(),
                FenderValue::Int(3).into(),
                FenderValue::Int(4).into(),
            ]
            .into()
        )
    );
    assert_eq!(
        *run("$x = [1, 2, 3]; x[0] = 100; x[0]"),
        FenderValue::Int(100)
    );
}

#[test]
fn test_pass_by_reference() {
    assert_eq!(*run(include_str!("passByRef.fndr")), FenderValue::Int(8));
    assert_eq!(
        *run("$a = 4; $f = (n) {n = 0}; f(a); a"),
        FenderValue::Int(4)
    );
    assert_eq!(
        *run("$x = []; $f =(n) {n = 4}; f(x); x"),
        FenderValue::List(vec![].into())
    );
    assert_eq!(
        *run("$x = []; $y = x; x = 4; y"),
        FenderValue::List(vec![].into())
    );
    assert_eq!(*run("{$y = 1; {y = 5}(); y}()"), FenderValue::Int(5));
    assert_eq!(
        *run("$x = [1];$f = (n){ n[0] = 2 };f(x);x"),
        FenderValue::List(vec![FenderValue::Int(2).into()].into())
    );
}

#[test]
fn test_format_strings() {
    assert_eq!(
        *run("$x = 4; \"x is equal to {x}\""),
        FenderValue::String("x is equal to 4".to_string().into())
    );
    assert_eq!(
        *run("$x = 4; \"one more than x is {x + 1}\""),
        FenderValue::String("one more than x is 5".to_string().into())
    );
    assert_eq!(
        *run("\"\\n\\r\""),
        FenderValue::String("\n\r".to_string().into())
    );
}

#[test]
fn run_quicksort_test() {
    use FenderValue::Int;
    assert_eq!(
        *run(include_str!("quicksort.fndr")),
        FenderValue::List(
            vec![
                Int(1).into(),
                Int(2).into(),
                Int(7).into(),
                Int(8).into(),
                Int(9).into(),
                Int(10).into(),
                Int(54).into(),
                Int(57).into(),
                Int(68).into(),
                Int(670).into(),
                Int(1113).into()
            ]
            .into()
        )
    );
}

#[test]
fn test_shuffle() {
    let input_list = (0..100).collect::<Vec<_>>();
    let test_prog = format!(
        "$ordered = {:?}; $new = ordered.shuffle(); return [ordered, new]",
        input_list
    );

    let results = match (*run(&test_prog)).clone() {
        FenderValue::List(l) => ((*l[0]).to_string(), (*l[1]).to_string()),
        _ => unreachable!(),
    };
    assert_ne!(results.0, format!("{:?}", input_list));
    assert_ne!(results.1, format!("{:?}", input_list));
    assert_eq!(results.0, results.1);

    // -_- I hate this
}

#[test]
fn test_shuffled() {
    let input_list = (0..100).collect::<Vec<_>>();
    let test_prog = format!(
        "$ordered = {:?}; $new = ordered.getShuffled(); return [ordered, new]",
        input_list
    );

    let results = match (*run(&test_prog)).clone() {
        FenderValue::List(l) => ((*l[0]).to_string(), (*l[1]).to_string()),
        _ => unreachable!(),
    };
    assert_eq!(results.0, format!("{:?}", input_list));
    assert_ne!(results.1, format!("{:?}", input_list));
    assert_ne!(results.0, results.1);
}
