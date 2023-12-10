// LCM of a series of numbers; recursive
pub fn lcm(n: &[usize]) -> usize {
    if n.len() == 1 {
        return n[0];
    }

    let a = n[0];
    let b = lcm(&n[1..]);
    a * b / gcd(a, b)
}

// GCD of two numbers; recursive
pub fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }

    gcd(b, a % b)
}
