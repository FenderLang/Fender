use crate::{interpreter::create_vm, FenderReference, FenderTypeId, FenderValue};

fn run(source: &str) -> FenderReference {
    create_vm(source).unwrap().run().unwrap()
}

#[test]
fn test_simple_values() {
    assert_eq!(*run("1"), FenderValue::Int(1));
    assert_eq!(*run("1.0"), FenderValue::Float(1.0));
    assert_eq!(
        *run("\"Hello, world!\""),
        FenderValue::String("Hello, world!".to_string())
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
        FenderValue::List(vec![
            FenderValue::Int(1).into(),
            FenderValue::Int(2).into(),
            FenderValue::Int(3).into(),
        ])
    );
    assert_eq!(
        *run("$x = 4; [1, 2, 3, x]"),
        FenderValue::List(vec![
            FenderValue::Int(1).into(),
            FenderValue::Int(2).into(),
            FenderValue::Int(3).into(),
            FenderValue::Int(4).into(),
        ])
    );
}

#[test]
fn test_pass_by_reference() {
    assert_eq!(*run(include_str!("passByRef.fndr")), FenderValue::Int(8));
    assert_eq!(*run("$a = 4; $f = (n) {n = 0}; f(a); a"), FenderValue::Int(4));
}

#[test]
fn test_format_strings() {
    assert_eq!(*run("$x = 4; \"x is equal to {x}\""), FenderValue::String("x is equal to 4".to_string()));
    assert_eq!(*run("$x = 4; \"one more than x is {x + 1}\""), FenderValue::String("one more than x is 5".to_string()));
    assert_eq!(*run("\"\\n\\r\""), FenderValue::String("\n\r".to_string()));
}