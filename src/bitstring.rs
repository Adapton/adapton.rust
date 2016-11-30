#[derive(Eq,PartialEq,Hash,Debug,Clone,Copy)]
pub struct BS {
    pub length: i64,
    pub value: i64,
}

pub trait BitString {
    fn pow(i64, i64) -> i64;
    fn flip(i64, i64) -> i64;
    fn is_set(i64, i64) -> bool;
    fn prepend(i64, BS) -> BS;
    fn length(BS) -> i64;

    const max_len: u32;
}

impl BitString for BS {
    fn pow(b: i64, n: i64) -> i64 {
        match n {
            0 => 1,
            1 => b,
            n => {
                let x = Self::pow(b, n / 2);
                x * x * (if n % 2 == 0 { 1 } else { b })
            }
        }
    }
    fn flip(i: i64, b: i64) -> i64 {
        let n = Self::pow(2, i);
        if n & b == n {
            return b - Self::pow(2, i);
        } else {
            return b + Self::pow(2, i);
        }
    }
    fn is_set(i: i64, b: i64) -> bool {
        let n = Self::flip(i, 0);
        (n & b) == n
    }
    fn prepend(b: i64, bs: BS) -> BS {
        match b {
            0 => {
                if !(Self::is_set(bs.length, bs.value)) {
                    BS {
                        length: bs.length + 1,
                        value: bs.value,
                    }
                } else {
                    BS {
                        length: bs.length + 1,
                        value: Self::flip(bs.length, bs.value),
                    }
                }
            }
            1 => {
                if Self::is_set(bs.length, bs.value) {
                    BS {
                        length: bs.length + 1,
                        value: bs.value,
                    }
                } else {
                    BS {
                        length: bs.length + 1,
                        value: Self::flip(bs.length, bs.value),
                    }
                }
            }
            _ => panic!("b has to be a bit (0 or 1)"),
        }
    }
    fn length(bs: BS) -> i64 {
        bs.length
    }

    const max_len: u32  = 30;
}

#[test]
fn test_pow() {
    assert_eq!(BS::pow(0, 0), 1);
    assert_eq!(BS::pow(42, 0), 1);
    assert_eq!(BS::pow(-2, 0), 1);
    assert_eq!(BS::pow(2, 4), 16);
    assert_eq!(BS::pow(-2, 3), -8);
}

#[test]
fn test_flip() {
    assert_eq!(BS::flip(0, 0), 1);
    assert_eq!(BS::flip(2, 0), 4);
    assert_eq!(BS::flip(1, 7), 5);
    assert_eq!(BS::flip(3, 7), 15);
}

#[test]
fn test_is_set() {
    assert_eq!(BS::is_set(4, 0), false);
    assert_eq!(BS::is_set(0, 0), false);
    assert_eq!(BS::is_set(0, 1), true);
    assert_eq!(BS::is_set(1, 2), true);
    assert_eq!(BS::is_set(0, 2), false);
}

#[test]
fn test_prepend() {
    assert_eq!(BS::prepend(0, BS { length: 0, value: 0}), BS { length: 1, value: 0 });
    assert_eq!(BS::prepend(1, BS { length: 0, value: 0}), BS { length: 1, value: 1});
    assert_eq!(BS::prepend(1, BS { length: 1, value: 1}), BS { length: 2, value: 3});
    assert_eq!(BS::prepend(0, BS { length: 1, value: 1}), BS { length: 2, value: 1});
    assert_eq!(BS::prepend(1, BS { length: 2, value: 1}), BS { length: 3, value: 5});
}
