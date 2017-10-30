#[cfg(test)]
mod tests {
    #[test]
    fn test_string() {
        let mut s = String::new();
        s += "a";
        assert_eq!("a", s);
        s += "b";
        assert_eq!("ab", s);

        s += ".";
        assert_eq!("ab.", s);
        s += "0123456789";
        assert_eq!("ab.0123456789", s);
    }
}
