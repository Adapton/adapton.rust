/// Bit Strings are length/value pairs, so that bit strings with leading
/// zeros aren't conflated.
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
    fn shift_left(BS, i64) -> BS;

    const MAX_LEN: i64;
}

impl BitString for BS {
    /// `pow(b, n)` yields b^n
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
    /// `flip(i, b)` toggles the `i`th bit of `b`.
    fn flip(i: i64, b: i64) -> i64 {
        let n = Self::pow(2, i);
        if n & b == n {
            return b - Self::pow(2, i);
        } else {
            return b + Self::pow(2, i);
        }
    }
    /// `is_set(i, b)` returns true if the `i`th bit of `b` is set
    fn is_set(i: i64, b: i64) -> bool {
        let n = Self::flip(i, 0);
        (n & b) == n
    }
    /// `prepend(b, bs)` prepends the bit `b` onto the bitstring `bs`.
    /// `b` must be either `0` or `1`.
    fn prepend(b: i64, bs: BS) -> BS {
        match b {
            0 if !(Self::is_set(bs.length, bs.value)) =>
                BS {
                    length: bs.length + 1,
                    value: bs.value,
                },
            1 if Self::is_set(bs.length, bs.value) =>
                BS {
                    length: bs.length + 1,
                    value: bs.value,
                },
            0 | 1 =>
                BS {
                    length: bs.length + 1,
                    value: Self::flip(bs.length, bs.value),
                },
            _ => panic!("b has to be a bit (0 or 1)"),
        }
    }
    /// Returns the length of the bitstring `bs`.
    fn length(bs: BS) -> i64 {
        bs.length
    }
    /// Performs a logical shift left on the bitstring `bs`.
    fn shift_left(bs: BS, i: i64) -> BS {
        BS {
            length: bs.length,
            value:(bs.value << i) & (2i64.pow(bs.length as u32) - 1),
        }
    }

    /// The maximum supported length of a bitstring is 30 bits.
    const MAX_LEN: i64 = 30;
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
    assert_eq!(BS::prepend(0, BS { length: 0, value: 0 }), BS { length: 1, value: 0 });
    assert_eq!(BS::prepend(1, BS { length: 0, value: 0 }), BS { length: 1, value: 1 });
    assert_eq!(BS::prepend(1, BS { length: 1, value: 1 }), BS { length: 2, value: 3 });
    assert_eq!(BS::prepend(0, BS { length: 1, value: 1 }), BS { length: 2, value: 1 });
    assert_eq!(BS::prepend(1, BS { length: 2, value: 1 }), BS { length: 3, value: 5 });
}

#[test]
fn test_shift_left() {
    assert_eq!(BS::shift_left(BS { length: 1, value: 0 }, 1), BS { length: 1, value: 0 });
    assert_eq!(BS::shift_left(BS { length: 1, value: 1 }, 1), BS { length: 1, value: 0 });
    assert_eq!(BS::shift_left(BS { length: 2, value: 1 }, 1), BS { length: 2, value: 2 });
    assert_eq!(BS::shift_left(BS { length: 2, value: 3 }, 1), BS { length: 2, value: 2 });
    assert_eq!(BS::shift_left(BS { length: 4, value: 2 }, 2), BS { length: 4, value: 8 });
}
