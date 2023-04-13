use crate::{
    fender_reference::{FenderReference, InternalReference},
    fender_value::FenderValue,
    interpreter::create_engine_main,
    type_sys::type_id::FenderTypeId,
};
use freight_vm::error::FreightError;

fn run(source: &str) -> FenderReference {
    let (mut engine, main) = create_engine_main(source).unwrap();
    engine.call(&main, Vec::new()).unwrap()
}

fn run_error(source: &str) -> FreightError {
    let (mut engine, main) = create_engine_main(source).unwrap();
    engine.call(&main, Vec::new()).unwrap_err()
}

#[test]
fn simple_values() {
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
fn imports() {
    assert_eq!(
        *run("@import src/test/exports; exports::x"),
        FenderValue::Int(1)
    );
    assert_eq!(*run("@import src/test/exports::x; x"), FenderValue::Int(1));
    assert_eq!(
        *run("@import src/test/exports::*; x + y"),
        FenderValue::Int(3)
    );
    assert_eq!(
        *run("@import src/test/exports::(x, y); x + y"),
        FenderValue::Int(3)
    );
}

#[test]
fn assign_operate() {
    assert_eq!(*run("$x = 4; x += 1; x"), FenderValue::Int(5));
    assert_eq!(*run("$x = 5; x /= 2; x"), FenderValue::Int(2));
    assert_eq!(
        *run(r#"$abc = "abc"; abc += "def"; abc"#),
        FenderValue::make_string("abcdef".to_string())
    );
    assert_eq!(*run("$x = 10; x %= 3; x"), FenderValue::Int(1));
}

#[test]
fn algebraic_expressions() {
    assert_eq!(*run("3 + 2"), FenderValue::Int(5));
    assert_eq!(*run("3 + 2 + 2"), FenderValue::Int(7));
    assert_eq!(*run("3 + 2 / 2"), FenderValue::Int(4));
    assert_eq!(*run("(3 + 7) / 2"), FenderValue::Int(5));
    assert_eq!(*run("-2"), FenderValue::Int(-2));
}

#[test]
fn closures() {
    assert_eq!(*run("{1}()"), FenderValue::Int(1));
    assert_eq!(*run("{$}(1)"), FenderValue::Int(1));
    assert_eq!(*run("{{$}(1)}()"), FenderValue::Int(1));
    assert_eq!(*run("(a, b) {a + b}(1, 2)"), FenderValue::Int(3));
}

#[test]
fn variables() {
    assert_eq!(*run("$x = 4; x"), FenderValue::Int(4));
    assert_eq!(*run("$x = 4; $y = x / 2; y"), FenderValue::Int(2));
    assert_eq!(*run("$add = (a, b) {a + b}; 4.add(5)"), FenderValue::Int(9));
    assert_eq!(*run("$abc = 4; abc = abc + 1; abc"), FenderValue::Int(5));
}

#[test]
fn capture() {
    assert_eq!(*run("{$x = 10; {x}()}()"), FenderValue::Int(10));
    assert_eq!(*run("{$x = 10; {x = x / 2}(); x}()"), FenderValue::Int(5));
}

#[test]
fn return_statement() {
    assert_eq!(*run("return 2; 1"), FenderValue::Int(2));
    assert_eq!(*run("{return 2; 1}()"), FenderValue::Int(2));
    assert_eq!(*run("{{return 2; 1}()}()"), FenderValue::Int(2));
    assert_eq!(
        *run("{$x = `test {return@test 2; 1}(); x + 1}()"),
        FenderValue::Int(3)
    );
}

#[test]
fn lists() {
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
fn run_pass_by_reference_file() {
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
fn format_strings() {
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
fn run_quicksort_file() {
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
fn structs() {
    let script = r#"
        struct StructName {name:String, field2}
        $tst_val = StructName(name:"the name", field2:[1, 2, 3, 4])

        (tst_val::name == "the name").else({return false})
        (tst_val::field2 == [1, 2, 3, 4]).else({return false})

        tst_val::field2[1] = 12345
        (tst_val::field2 == [1, 12345, 3, 4]).else({return false})


        tst_val::field2 = "word"
        (tst_val::field2 == "word").else({return false})

        $set_name_to_tim = {$::name = "tim"}
        tst_val.set_name_to_tim()
        (tst_val::name == "tim").else({return false})
    "#;

    assert_eq!(run(script), FenderValue::Bool(true).into());
}

mod hash_maps {
    use super::*;
    use std::collections::HashMap;

    fn make_test_map() -> HashMap<FenderValue, FenderReference> {
        ('a'..='e')
            .into_iter()
            .zip(1..=5)
            .map(|(k, v)| {
                (
                    FenderValue::Char(k),
                    FenderReference::from(FenderValue::Int(v)).into(),
                )
            })
            .collect::<HashMap<_, _>>()
    }

    #[test]
    fn create() {
        assert_eq!(
            run("['a':1,'b':2,'c':3,'d':4,'e':5]"),
            FenderValue::HashMap(make_test_map().into()).into()
        );
    }

    #[test]
    fn check_eq() {
        assert_eq!(
            run("$x = ['a':1,'b':2,'c':3,'d':4,'e':5]; $y = ['a':1,'b':2,'c':3,'d':4,'e':5]; x == y"),
            FenderValue::Bool(true).into()
        );
        assert_eq!(
            run("$x = ['a':1,'b':2,'c':3,'d':4,'e':5]; $y = ['a':1,'b':2,'c':3,'d':4,'e':17]; x == y"),
            FenderValue::Bool(false).into()
        );
        assert_eq!(
            run("$x = ['a':1,'b':2,'c':3,'d':4,'e':5]; $y = ['a':1,'b':2,'c':3]; x == y"),
            FenderValue::Bool(false).into()
        );
        assert_eq!(
            run("$x = ['a':1,'b':2,'c':3,'d':4,'e':5]; $y = ['a':1,'b':2,'c':3,'d':4,'e':5]; x != y"),
            FenderValue::Bool(false).into()
        );
    }

    #[test]
    fn insert() {
        assert_eq!(
            run("$x = ['a':1,'c':3,'d':4,'e':5]; x.insert('b', 2); x"),
            FenderValue::HashMap(make_test_map().into()).into()
        );

        assert_eq!(
            run("$x = ['a':1,'b':3,'c':3,'d':4,'e':5]; x.insert('b', 2); x"),
            FenderValue::HashMap(make_test_map().into()).into()
        );

        assert_eq!(
            run("$x = ['a':1,'b':3,'c':3,'d':4,'e':5]; x.insert('b', 2)"),
            FenderValue::Int(3).into()
        );
    }

    #[test]
    fn assignment() {
        assert_eq!(
            run("$x = ['a':1,'b':2,'c':0,'d':4,'e':5]; x['c'] = 3; x"),
            FenderValue::HashMap(make_test_map().into()).into()
        );
    }
}

mod variadic_functions {
    use freight_vm::error::FreightError;

    use super::*;

    #[test]
    fn fixed_args() {
        let script = r#"
            $fName = (a, b, c){}
            fName(1, 2, 3)
            fName(1, 2, 3, 4)
            null
        "#;

        assert!(matches!(
            run_error(script),
            FreightError::IncorrectArgumentCount {
                expected_min: 3,
                expected_max: Some(3),
                actual: 4
            }
        ));
    }

    #[test]
    fn optional_args() {
        assert_eq!(
            run_error(r#"$fName = (a, b, [c]){};fName(1,2,3);fName(1)"#),
            FreightError::IncorrectArgumentCount {
                expected_min: 2,
                expected_max: Some(3),
                actual: 1
            }
        );

        assert_eq!(
            run_error(r#"$fName = (a, b, [c]){};fName(1,2,3);fName(1,2,3,4)"#),
            FreightError::IncorrectArgumentCount {
                expected_min: 2,
                expected_max: Some(3),
                actual: 4
            }
        );
    }

    #[test]
    fn variadic_args() {
        assert_eq!(
            run_error(r#"$fName = (a, [b, c...]){};fName()"#),
            FreightError::IncorrectArgumentCount {
                expected_min: 1,
                expected_max: None,
                actual: 0
            }
        );
        assert_eq!(
            run(r#"$fName = (a, [b, c...]){ a == 1 && b == null && c == []};fName(1)"#)
                .unwrap_value(),
            FenderValue::Bool(true)
        );

        assert_eq!(
            run(r#"$fName = (a, [b, c...]){ a == 1 && b == 2 && c == [3, 4, 5]};fName(1,2,3,4,5)"#)
                .unwrap_value(),
            FenderValue::Bool(true)
        );
    }
}

mod stdlib {
    use super::*;

    mod cast {
        use super::*;

        #[test]
        fn r#ref() {
            assert_eq!(
                *run("$x = 1; $y = x.ref(); y"),
                FenderValue::Ref(InternalReference::new(FenderReference::FRef(
                    InternalReference::new(FenderValue::Int(1))
                )))
            );
        }

        #[test]
        fn raw() {
            assert_eq!(
                *run("$x = 1; $y = x.ref(); y.raw()"),
                FenderReference::FRef(InternalReference::new(FenderValue::Int(1))).into()
            );
        }

        #[test]
        fn int() {
            assert_eq!(*run(r#"$x = "1"; x"#), FenderValue::make_string("1".into()));
            assert_eq!(*run(r#"$x = "1"; x.int()"#), FenderValue::Int(1));
        }

        #[test]
        fn str() {
            assert_eq!(*run("$x = 1; x"), FenderValue::Int(1));
            assert_eq!(
                *run("$x = 1; x.str()"),
                FenderValue::make_string("1".into())
            );
        }

        #[test]
        fn bool() {
            assert_eq!(
                *run(r#"$x = "true"; x"#),
                FenderValue::make_string("true".into())
            );
            assert_eq!(*run(r#"$x = "true"; x.bool()"#), FenderValue::Bool(true));
        }

        #[test]
        fn list() {
            assert_eq!(
                *run(r#"$x = "hello, world!"; x"#),
                FenderValue::make_string("hello, world!".into())
            );
            assert_eq!(
                *run(r#"$x = "hello, world!"; x.list()"#),
                FenderValue::make_list(
                    "hello, world!"
                        .chars()
                        .map(|c| FenderValue::Char(c).into())
                        .collect()
                )
            );
        }

        #[test]
        fn join_str() {
            use FenderValue as FV;
            assert_eq!(
                *run(r#"$x = ["hello", " world!"]; x"#),
                FV::make_list(vec![
                    FenderReference::FRef(FV::make_string("hello".into()).into()),
                    FenderReference::FRef(FV::make_string(" world!".into()).into())
                ])
            );

            assert_eq!(
                *run(r#"$x = ["hello", " world!"]; x"#),
                FV::make_list(vec![
                    FV::make_string("hello".into()).into(),
                    FV::make_string(" world!".into()).into()
                ])
            );

            assert_eq!(
                *run(r#"$x = ["hello", " world!"]; x.joinStr()"#),
                FV::make_string("hello world!".into())
            );
        }
    }

    mod system {
        use super::*;

        #[test]
        fn pwd() {
            assert_eq!(
                *run("pwd()"),
                FenderValue::make_string(std::env::current_dir().unwrap().to_string_lossy().into())
            );
        }

        #[test]
        fn cd() {
            let pwd = std::env::current_dir().unwrap();
            assert_eq!(
                *run(r#"pwd().println();cd(".."); pwd().println()"#),
                FenderValue::make_string(pwd.parent().unwrap().to_string_lossy().into())
            );
            std::env::set_current_dir(pwd).unwrap();
        }

        #[test]
        fn shell() {
            assert_eq!(
                *run(r#"shell("echo hi")"#),
                FenderValue::make_string("hi\n".into())
            );

            assert_eq!(
                *run(r#" "hi".shell("echo")"#),
                FenderValue::make_string("hi\n".into())
            );

            assert_eq!(
                *run(r#" "hi".shell("echo", "bash -c")"#),
                FenderValue::make_string("hi\n".into())
            );
        }
    }

    mod val_operations {
        use super::*;

        #[test]
        fn len() {
            assert_eq!(*run(r#""hello".len()"#), FenderValue::Int(5));
            assert_eq!(*run(r#"[1, 2, 3, 4, 5].len()"#), FenderValue::Int(5));

            assert!(matches!(*run(r#"2.len()"#), FenderValue::Error(_)))
        }

        #[test]
        fn swap() {
            assert_eq!(
                *run(r#"[0,1,2,3]"#),
                FenderValue::make_list((0..=3).map(|i| FenderValue::Int(i).into()).collect())
            );

            assert_eq!(
                *run(r#"[0,1,2,3].swap(0, 3)"#),
                FenderValue::make_list(
                    ([3, 1, 2, 0])
                        .into_iter()
                        .map(|i| FenderValue::Int(i).into())
                        .collect()
                )
            );
            match *run(r#"[0,1,2,3].swap(0, "3")"#) {
                FenderValue::Error(_) => (),
                _ => panic!(),
            }
        }

        #[test]
        #[should_panic]
        fn swap_panic() {
            run(r#"[0,1,2,3].swap(0, 9)"#);
        }

        #[test]
        fn shuffle() {
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
        fn shuffled() {
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

        #[test]
        fn rand() {
            assert!(matches!(*run(r#"rand()"#), FenderValue::Float(_)))
        }

        #[test]
        fn push() {
            assert_eq!(
                *run(r#"[1, 2, 3, 4, 5]"#),
                FenderValue::make_list((1..=5).map(|i| FenderValue::Int(i).into()).collect())
            );

            assert_eq!(
                *run(r#"[1, 2, 3, 4, 5].push(6)"#),
                FenderValue::make_list((1..=6).map(|i| FenderValue::Int(i).into()).collect())
            );
        }

        #[test]
        fn pop() {
            assert_eq!(
                *run(r#"[1, 2, 3, 4, 5]"#),
                FenderValue::make_list((1..=5).map(|i| FenderValue::Int(i).into()).collect())
            );

            assert_eq!(
                *run(r#"$l = [1, 2, 3, 4, 5]; $v = l.pop(); [l, v]"#),
                FenderValue::make_list(vec![
                    FenderValue::make_list((1..=4).map(|i| FenderValue::Int(i).into()).collect())
                        .into(),
                    FenderValue::Int(5).into()
                ])
            );
        }

        #[test]
        fn dbg() {
            assert_eq!(*run(r#"1"#), FenderValue::Int(1));
            assert_eq!(
                *run(r#"1.dbg()"#),
                FenderValue::make_string("Int(1)".into())
            );
        }

        #[test]
        fn remove() {
            assert_eq!(
                *run(r#"[1, 2, 3, 4, 5]"#),
                FenderValue::make_list((1..=5).map(|i| FenderValue::Int(i).into()).collect())
            );

            assert_eq!(*run(r#"[1, 2, 3, 4, 5].remove(2)"#), FenderValue::Int(3));
        }

        #[test]
        fn remove_pass() {
            assert_eq!(
                *run(r#"[1, 2, 3, 4, 5]"#),
                FenderValue::make_list((1..=5).map(|i| FenderValue::Int(i).into()).collect())
            );

            assert_eq!(
                *run(r#"[1, 2, 3, 4, 5].removePass(2)"#),
                FenderValue::make_list(
                    ([1, 2, 4, 5])
                        .into_iter()
                        .map(|i| FenderValue::Int(i).into())
                        .collect()
                )
            );
        }
    }
}
